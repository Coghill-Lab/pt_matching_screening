# screening_processing.R

library(dplyr)
library(tidyr)
library(rlang)

process_screening_data <- function(details, known_hiv_neg = NULL, redcap = NULL) {
  stopifnot(is.data.frame(details))
  
  # Required columns in `details`
  needed <- c("MRN", "HIV Diagnosis Present", "VisitType")
  missing <- setdiff(needed, names(details))
  if (length(missing)) {
    abort(paste0("`details` is missing columns: ", paste(missing, collapse = ", ")))
  }
  
  # The "Status...13" header is brittle; pick the first column that starts with "Status"
  status_col <- grep("^Status", names(details), value = TRUE)[1]
  
  # Keep a stable set of columns if present
  keep_cols <- c(
    "MRN", "Location", "PT Last Name", "PT First Name",
    "Gender", "Race", "Ethnicity", "TCC ICF Version",
    status_col, "Primary Lang", "Date/Time", "VisitType",
    "Appt Note", "Resource", "Diagnosis", "HIV Diagnosis Present"
  )
  
  # Filter to HIV+
  details_cut <- details |>
    filter(`HIV Diagnosis Present` == "Yes") |>
    select(any_of(keep_cols)) |>
    distinct()
  
  # Prioritize visit types
  details_orpp <- details_cut |>
    mutate(
      priority = case_when(
        VisitType %in% c("EP", "NP", "NEP", "zoomEP") ~ 1,
        VisitType %in% c("zoomNP", "zoomNEP")         ~ 1.5,
        VisitType == "OTV"                            ~ 2,
        grepl("Radiation", VisitType, ignore.case = TRUE) ~ 3,
        grepl("^SN", VisitType, ignore.case = TRUE)   ~ 4,
        VisitType %in% c("Lab Services", "Lab Procedure") ~ 5,
        TRUE                                          ~ 6
      )
    ) |>
    group_by(MRN) |>
    slice_min(order_by = priority, with_ties = FALSE) |>
    ungroup()
  
  # Remove known HIV negatives
  if (!is.null(known_hiv_neg) && nrow(known_hiv_neg)) {
    known_hiv_neg_mrn <- known_hiv_neg |> select(MRN) |> mutate(MRN = as.character(MRN))
    details_orpp_keep <- details_orpp |> anti_join(known_hiv_neg_mrn, by = "MRN")
  } else {
    details_orpp_keep <- details_orpp
  }
  
  
  # REDCap data, if provided
  if (!is.null(redcap) && nrow(redcap)) {
    redcap <- mutate(redcap, mrn = as.character(mrn))
    
    eligibility_labels <- c(
      "amc_eligibility___10" = "AMC-111 Eligible",
      "amc_eligibility___11" = "AMC-111 Not Eligible",
      "amc_eligibility___18" = "AMC-111 Screen for Eligibility (Recently Quit Smoking)",
      "amc_eligibility___19" = "AMC-111 Not Eligible (Quit more than 1 year ago)",
      "amc_eligibility___20" = "AMC-111 Not Eligible (Lung Cancer)",
      "amc_eligibility___21" = "AMC-111 Not Eligible (< 50 years)",
      "amc_eligibility___17" = "AMC-111 Not Eligible (Active Treatment)",
      "amc_eligibility___13" = "AMC-111 Not Eligible (Never Smoker)",
      "amc_eligibility___14" = "AMC-111 Not Eligible (< 20 pack/yrs)",
      "amc_eligibility___15" = "AMC-111 Not Eligible (CT Scan past 12 mos)",
      "amc_eligibility___16" = "AMC-111 Not Eligible (Cessation services past 30 days)",
      "amc_eligibility___12" = "AMC-111 Unsure"
    )
    elig_vars <- intersect(names(eligibility_labels), names(redcap))
    
    redcap <- redcap |>
      rowwise() |>
      mutate(AMC_111_status = {
        if (!length(elig_vars)) NA_character_ else {
          vals <- c_across(all_of(elig_vars))
          # treat 1/"1"/TRUE/"Yes" as a hit
          hit <- which(vals %in% c(1, "1", TRUE, "Yes"))[1]
          if (is.na(hit)) NA_character_ else eligibility_labels[[elig_vars[hit]]]
        }
      }) |>
      ungroup()
    
    status_summary <- redcap |>
      group_by(mrn) |>
      summarize(AMC_111_status = dplyr::first(AMC_111_status[!is.na(AMC_111_status)]),
                .groups = "drop")
    
    redcap_keep <- redcap |>
      transmute(mrn,
                collection_notes = na_if(as.character(collection_notes), ""),
                note_last_modified = na_if(as.character(note_last_modified), "")
      ) |>
      filter(!(is.na(collection_notes) & is.na(note_last_modified))) |>
      distinct() |>
      group_by(mrn) |>
      mutate(row_num = row_number()) |>
      ungroup()
    
    redcap_wide <- redcap_keep |>
      pivot_wider(
        names_from  = row_num,
        values_from = c(collection_notes, note_last_modified),
        names_glue  = "{.value}_{row_num}"
      )
    
    details_orpp_keep <- details_orpp_keep |>
      left_join(status_summary, by = c("MRN" = "mrn")) |>
      left_join(redcap_wide,  by = c("MRN" = "mrn"))
  }
  
  details_orpp_keep |>
    mutate(Screened = NA) |>
    rename(Language = `Primary Lang`, Status = all_of(status_col)) |>
    select(-priority)
}
