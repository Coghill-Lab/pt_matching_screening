
version_id <- paste0("v0.0.9")


# Back end input ---------------------------------------------------------------

Known_HIVNEG_File <- "data/TCC_Screening_KnownHIVNegatives_20241212.csv"

source("module_screening_processing.R")


# UI ---------------------------------------------------------------------------

ui <-
  shiny::navbarPage(span("{ SMART }", style = "color:white; font-weight:bold;"),
                    id = "navbar_id",
                    theme = bslib::bs_theme(bootswatch = "yeti"),
                    tags$head(
                      tags$link(rel = "stylesheet", type = "text/css", href = "styling.css")
                    ),

                    ## Homepage ------------------------------------------------
                    #shiny::tabPanel("Homepage",
                    #                 icon = icon("home")
                    #                # Explain SMART acronym what it can be used for - any logos
                    #),

                    ## Screening -----------------------------------------------
                    shiny::tabPanel("Screening",
                                    icon = icon("search",lib = "font-awesome"),
                                    shiny::sidebarLayout(
                                      shiny::sidebarPanel(
                                        width = 3,
                                        h4("Load Input Data"),
                                        shiny::fileInput("screening_file_upload","Upload Screening Data"),
                                        shiny::fileInput("known_neg_file_upload","Upload Known HIV Negatives", placeholder = "Optional"),
                                        shiny::fileInput("redcap_file_upload", "Upload REDCap Data"), # add redcap eligibility
                                        conditionalPanel(condition = "input.screening_panel == '1'",
                                                         hr(),
                                                         h4("Filter Patients"),
                                                         tags$head(
                                                           tags$style(HTML('.selectize-input {
                                                                max-height: 82px;
                                                                overflow-y: auto;}'
                                                           )
                                                           )
                                                         ),
                                                         selectizeInput("mrn_column","MRN Column:", choices = NULL, selected = 1),
                                                         selectizeInput("hiv_present_column","HIV Indicator Column:", choices = NULL, selected = 1),
                                                         ),
                                        conditionalPanel(condition = "input.screening_panel == '2'",
                                                         p(),
                                                         selectizeInput("screen_visit_type", "Visit Types:", choices = NULL, selected = NULL, multiple = TRUE),
                                                         shinyWidgets::airDatepickerInput("screen_visit_date_range", label = "Select Visit Date Range:", 
                                                                                          range = TRUE, clearButton = TRUE),
                                                         verbatimTextOutput("checked_rows")),
                                        hr(),
                                        downloadButton("download_final_screening_data", "Download Final Screening Data")
                                      ),
                                      shiny::mainPanel(
                                        shiny::tabsetPanel(id = "screening_panel",
                                                           tabPanel("Input Data",
                                                                    p(),
                                                                    DT::dataTableOutput("screen_table_for_screening"),
                                                                    value = 1
                                                                    ),
                                                           tabPanel("HIV Positive Patients",
                                                                    p(),
                                                                    DT::dataTableOutput("screen_hivpos_table_for_screening"),
                                                                    value = 2
                                                                    ),
                                                           tabPanel("Final Screening Data",
                                                                    p(),
                                                                    DT::dataTableOutput("final_screening_table"),
                                                                    value = 3
                                                           )
                                                           )
                                      )
                                    )
                    ),
                    ## Matching ------------------------------------------------
                    shiny::tabPanel("Matching",
                                    icon = icon("puzzle-piece"),
                                    shiny::sidebarLayout(
                                      shiny::sidebarPanel(
                                        width = 3,
                                        conditionalPanel(condition = "input.matching_panel == '1'",
                                                         p(),
                                                         shiny::fileInput("matching_file_upload","Upload Matching Data"),
                                                         hr(),
                                                         h4("Select Patient to Match", style = "font-size: 22px; font-weight: bold;"),
                                                         selectizeInput("hivpos_pat_to_match","Select HIV Positive Patient to Find Match:", choices = NULL, selected = 1),
                                                         hr(),
                                                         h4("Filters",style = "font-size: 20px;"),
                                                         dateRangeInput("match_data_range","Appointment Date Range:"),
                                                         selectizeInput("match_clinic","Clinic Location:", choices = NULL, selected = NULL, multiple = TRUE),
                                                         fluidRow(
                                                           column(6,
                                                                  numericInput("age_to_match","Age:",value = NULL),
                                                                  selectizeInput("match_race","Race:", choices = c("American Indian, Aleutian, or Eskimo",
                                                                                                                   "Asian Indian, Pakistani", #need better solution for race
                                                                                                                   "Black",
                                                                                                                   "Chamorran",
                                                                                                                   "Chinese",
                                                                                                                   "Filipino",
                                                                                                                   "Hawaiian",
                                                                                                                   "Japanese",
                                                                                                                   "Kampuchean (including Khmer, Cambodian)",
                                                                                                                   "Korean",
                                                                                                                   "More Than One Race",
                                                                                                                   "Other",
                                                                                                                   "Other Asian including Asian and Oriental",
                                                                                                                   "Pacific Islander",
                                                                                                                   "Thai",
                                                                                                                   "Tongan",
                                                                                                                   "Unknown",
                                                                                                                   "Vietnamese",
                                                                                                                   "White"), selected = NULL, multiple = T),
                                                                  selectizeInput("match_gender","Gender:", choices = c("Male", "Female"), selected = NULL, multiple = T)
                                                                  ),
                                                           column(6,
                                                                  numericInput("age_range","+/- years:", value = 5, min = 1, step = 1),
                                                                  selectizeInput("match_ethnicity","Ethnicity:", choices = c("Spanish; Hispanic", "Non-Spanish; non-Hispanic"), selected = NULL, multiple = T),
                                                                  selectizeInput("match_vtype","Visit Type:", choices = c("NP", "NEP"), selected = NULL, multiple = T)
                                                                  )
                                                         ),
                                                         bslib::accordion(id = "term_filters", open = FALSE,
                                                           bslib::accordion_panel("Term Filters",
                                                                                  selectizeInput("key_term_daig_in", "Include Diagnosis Key Term", choices = NULL,
                                                                                                 multiple = TRUE,options = list(create = TRUE)),
                                                                                  selectizeInput("key_term_daig_ex", "Exclude Diagnosis Key Term", choices = NULL,
                                                                                                 multiple = TRUE,options = list(create = TRUE)),
                                                                                  selectizeInput("key_term_notes_in", "Include Notes Key Term", choices = NULL,
                                                                                                 multiple = TRUE,options = list(create = TRUE)),
                                                                                  selectizeInput("key_term_notes_ex", "Exclude Notes Key Term", choices = NULL,
                                                                                                 multiple = TRUE,options = list(create = TRUE))
                                                                                  )
                                                           ),
                                                         p(),
                                                         dateRangeInput("match_diag_data_range","Diagnosis Date Range:"),
                                                         hr(),
                                                         actionButton("SavePatientMatch","Save Match", width = "100%")
                                                         ),
                                        conditionalPanel(condition = "input.matching_panel == '2'",
                                                         downloadButton("dnlnd_data","Export data")
                                                         # Exporting data
                                                         )
                                      ),
                                      shiny::mainPanel(
                                        shiny::tabsetPanel(id = "matching_panel",
                                                           tabPanel("Find Matches",
                                                                    p(),
                                                                    h3(tags$b("HIV Positive Patient to Match",style = "font-size: 22px;")),
                                                                    DT::dataTableOutput("HIV_Pos_table_for_matching"),
                                                                    hr(),
                                                                    h3(tags$b("HIV Negative Patients for Matching",style = "font-size: 22px;")),
                                                                    #reactableOutput("HIV_Neg_table_for_matching"),
                                                                    DT::dataTableOutput("HIV_Neg_table_for_matching"),
                                                                    value = 1
                                                                    ),
                                                           tabPanel("Review Matches",
                                                                    p(),
                                                                    fluidRow(
                                                                      column(6,
                                                                             h3(tags$b("HIV Positive Patients",style = "font-size: 20px;")),
                                                                             DT::dataTableOutput("HIV_Pos_table_match_review")
                                                                             ),
                                                                      column(6,
                                                                             h3(tags$b("HIV Negative Patients",style = "font-size: 20px;")),
                                                                             DT::dataTableOutput("HIV_Neg_table_match_review")
                                                                             )
                                                                    ),
                                                                    value = 2
                                                                    )
                                                           )
                                      )
                                    )
                    ),

                    ## Homepage ------------------------------------------------
                    shiny::tabPanel("About",
                                    # Explain SMART acronym what it can be used for - any logos
                    ),
  )




# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2) # increase upload file size limit from 5MB to 30MB
  
  source("module_screening_processing.R")
  
  ## Reactive Vals -------------------------------------------------------------
  hivneg_file <- reactiveVal(NULL)
  backend_known_hivneg_file <- reactiveVal(NULL)
  backend_hivneg_df <- reactiveVal(NULL)
  
  observe({
    if (file.exists(Known_HIVNEG_File)) {
      backend_known_hivneg_file(Known_HIVNEG_File)
    } else {
      backend_known_hivneg_file(NULL)
    }
  })

  screen_file <- reactiveVal(NULL)
  screen_df <- reactiveVal(NULL)
  screen_df_hivpos <- reactiveVal(NULL)
  screen_df_hivneg <- reactiveVal(NULL)
  
  redcap_file <- reactiveVal(NULL)
  redcap_df <- reactiveVal(NULL)
  
  match_file <- reactiveVal()
  match_df <- reactiveVal()
  
  output_hivpos_df <- reactiveVal(NULL)
  output_hivneg_df <- reactiveVal(NULL)
  
  hiv_matched_list <- reactiveVal(NULL)
  
  comment_df <- reactiveVal(NULL)
  comment_file <- reactiveVal(NULL)
  
  raw_screening_df <- reactiveVal(NULL)
  final_screening_df <- reactiveVal(NULL)
  final_screen_data <- reactiveVal(NULL)
  
  ## HIV-negative upload ----
  observeEvent(input$known_neg_file_upload, {
    req(input$known_neg_file_upload$datapath)
    file <- input$known_neg_file_upload$datapath
    ext  <- tolower(tools::file_ext(file))
    message("HIVNEG path: ", file, " (.", ext, ")")
    
    # read the file
    df <- switch(
      ext,
      "csv"  = readr::read_csv(file, show_col_types = FALSE) |> as.data.frame(),
      "xlsx" = readxl::read_excel(file, sheet = 1, .name_repair = "minimal") |> as.data.frame(),
      "xls"  = readxl::read_excel(file, sheet = 1, .name_repair = "minimal") |> as.data.frame(),
      {
        showNotification("HIV-negative file must be CSV/XLS/XLSX.", type = "error")
        return(invisible(NULL))
      }
    )
    
    # require an MRN column
    if (!"MRN" %in% names(df)) {
      showNotification("HIV-negative file: column 'MRN' not found.", type = "error")
      backend_hivneg_df(NULL)
      return(invisible(NULL))
    }
    
    df_clean <- df |>
      dplyr::transmute(MRN = as.character(.data$MRN)) |>
      dplyr::filter(!is.na(MRN) & MRN != "") |>
      dplyr::distinct()
    
    backend_hivneg_df(df_clean)
    message("HIVNEG rows loaded: ", nrow(df_clean))
  })
  
  
  
  ## Homepage ------------------------------------------------------------------
  
  
  ## Screening -----------------------------------------------------------------
  
  # Update screen csv file name
  observe({
    req(input$screening_file_upload$datapath)
    screen_file(input$screening_file_upload$datapath)
  })
  # Read in input file
  observe({
    req(screen_file())
    file <- screen_file()
    ext <- tools::file_ext(file)
    if (ext == "csv") {
      df <- as.data.frame(readr::read_csv(file, skip = 2)) # might need to update depending on input format
      #df <- df %>%
      #  filter(STATUS %in% c("CONFIRMED", "RESCHEDULED"))
      raw_screening_df(df)
    } else if (ext %in% c("xlsx","xls")) {
      df <- as.data.frame(readxl::read_excel(file, skip = 2,sheet = "Details")) # add skip row number input? or predict?
      raw_screening_df(df)
    }
    mrn_col_pred <- grep("mrn",colnames(df),ignore.case = T, value = T)[1]
    hiv_col_pred <- grep("hiv",colnames(df),ignore.case = T, value = T)[1]
    visit_col_pred <- grep("visit|vtype",colnames(df),ignore.case = T)[1]
    updateSelectizeInput(session,"mrn_column", choices = colnames(df), selected = mrn_col_pred, server = T)
    updateSelectizeInput(session,"hiv_present_column", choices = colnames(df), selected = hiv_col_pred, server = T)
    updateSelectizeInput(session,"screen_visit_type", choices = df[,visit_col_pred], selected = NULL, server = T)
  })
  
  screen_columns_id <- reactiveVal(list())
  
  observe({
    req(raw_screening_df())
    df <- raw_screening_df()
    location_col_pred <- grep("location",colnames(df),ignore.case = T, value = T)[1]
    appt_col_pred <- grep("APPTDTTM|appointment",colnames(df),ignore.case = T, value = T)[1]
    age_col_pred <- grep("age",colnames(df),ignore.case = T, value = T)
    age_col_pred <- age_col_pred[grep("language",age_col_pred, ignore.case = T, value = T, invert = T)][1]
    race_col_pred <- grep("race",colnames(df),ignore.case = T, value = T)[1]
    ethnicity_col_pred <- grep("ethnicity",colnames(df),ignore.case = T, value = T)[1]
    gender_col_pred <- grep("gender|sex",colnames(df),ignore.case = T, value = T)[1]
    visit_col_pred <- grep("visit|vtype",colnames(df),ignore.case = T, value = T)[1]
    diag_date_col_pred <- grep("DIAGNOSIS_DATE",colnames(df),ignore.case = T, value = T)[1]
    diag_notes_col_pred <- grep("DIAGNOSIS",colnames(df),ignore.case = T, value = T)
    diag_notes_col_pred <- diag_notes_col_pred[which(diag_notes_col_pred!=diag_date_col_pred)][1]
    notes_col_pred <- grep("NOTES",colnames(df),ignore.case = T, value = T)[1]
    
    col_id_list <- list(
      clinic_col = location_col_pred,
      appt_date_col = appt_col_pred,
      age_col = age_col_pred,
      race_col = race_col_pred,
      ethnicity_col = ethnicity_col_pred,
      gender_col = gender_col_pred,
      visit_col = visit_col_pred,
      diag_date_col = diag_date_col_pred,
      diag_notes_col = diag_notes_col_pred,
      notes_col = notes_col_pred
    )
    # Save back to reactive
    screen_columns_id(col_id_list)
  })
  
  screen_df_hivpos <- reactive({
    req(raw_screening_df())
    req(input$hiv_present_column)
    req(input$mrn_column)
    df <- raw_screening_df()
    hiv_col_pred <- input$hiv_present_column
    known_hivneg <- backend_hivneg_df()
    mrn_col <- input$mrn_column
    df_pos <- df[which(df[,hiv_col_pred] == "Yes"),]
    # remove known negatives
    if (isTruthy(known_hivneg)) {
      hivneg_mrns <- known_hivneg[,1]
      df_pos_kn <- df_pos[which(df_pos[,mrn_col] %in% hivneg_mrns),]
    }
    df_pos <- df_pos %>% relocate(!!sym(mrn_col))
    df_pos
  })
  screen_df_hivneg <- reactive({
    req(raw_screening_df())
    req(input$hiv_present_column)
    req(input$mrn_column)
    df <- raw_screening_df()
    hiv_col_pred <- input$hiv_present_column
    known_hivneg <- backend_hivneg_df()
    mrn_col <- input$mrn_column
    df_pos <- df[which(df[,hiv_col_pred] == "Yes"),]
    df_neg <- df[which(df[,hiv_col_pred] == "No"),]
    # remove known negatives
    if (isTruthy(known_hivneg)) {
      hivneg_mrns <- known_hivneg[,1]
      df_pos_kn <- df_pos[which(df_pos[,mrn_col] %in% hivneg_mrns),]
      df_neg <- rbind(df_neg,df_pos_kn)
    }
    df_neg <- df_neg %>% relocate(!!sym(mrn_col))
    df_neg
  })
  
  ## read in redcap file
  observe({
    req(input$redcap_file_upload$datapath)
    redcap_file(input$redcap_file_upload$datapath)
  })
  
  observe({
    req(redcap_file())
    file <- redcap_file()
    ext <- tools::file_ext(file)
    if (ext == "csv") {
      df <- read.csv(file)
      redcap_df(df)
    } else {
      showNotification("REDCap file must be a CSV.", type = "error")
    }
  })
  
  final_screen_data <- reactive({
    req(raw_screening_df())         
    req(redcap_df())
    
    tryCatch({
      process_screening_data(
        details  = raw_screening_df(),
        known_hiv_neg       = backend_hivneg_df(),
        redcap       = redcap_df()
      )
    }, error = function(e) {
      showNotification(paste("Error in processing:", e$message), type = "error")
      NULL
    })
  })
  
  observeEvent(backend_hivneg_df(), {
    cat("HIVNEG rows now:", if (is.null(backend_hivneg_df())) 0 else nrow(backend_hivneg_df()), "\n")
  })
  # For trouble shooting
  # observeEvent(final_screen_data(), {
  #   df <- final_screen_data()
  #   message("Final rows: ", if (is.null(df)) "NULL" else nrow(df))
  #   if (!is.null(df)) message("Final cols: ", ncol(df))
  # })
  
  output$download_final_screening_data <- downloadHandler(
    filename = function() paste0("final_screening_data_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- final_screen_data()
      validate(need(!is.null(df), "No final screening data to download."))
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # For trouble shooting
  # observe({
  #   cat("raw:", !is.null(raw_screening_df()),
  #       " hivneg:", !is.null(backend_hivneg_df()),
  #       " redcap:", !is.null(redcap_df()), "\n")
  # })
  # observeEvent(final_screen_data(), {
  #   cat("Final screening nrows:", NROW(final_screen_data()), "\n")
  # })
  
  observeEvent(final_screening_df(), {
    showNotification("Final screening data is ready!", type = "message")
  })
  
  
  # tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}'))
  
  
  observe({
    if (!is.null(final_screening_df())) {
      print(head(final_screening_df()))
    } else {
      print("final_screening_df is NULL")
    }
  })
  
  # Display input file
  output$screen_table_for_screening <- DT::renderDataTable({
    req(raw_screening_df())
    df <- raw_screening_df()
    DT::datatable(df,
                  escape = F,
                  class = "display nowrap",
                  extensions = 'ColReorder',
                  options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                 pageLength = 20,
                                 scrollX = T,
                                 target = "cell",
                                 colReorder = TRUE),
                  rownames = F
    )
  })
  
  # Display input hiv positive file
  output$screen_hivpos_table_for_screening <- DT::renderDataTable({
    req(screen_df_hivpos())
    df <- screen_df_hivpos()
    df <- df %>%
      mutate(Check = NA) %>%
      relocate(Check)
    DT::datatable(df,
                  escape = F,
                  class = "display nowrap",
                  extensions = c('ColReorder', "Select"), 
                  selection = "none",
                  options = list(
                    lengthMenu = c(5, 10, 20, 100, 1000),
                    pageLength = 20,
                    scrollX = T,
                    target = "cell",
                    colReorder = TRUE,
                    # options for Select ext.
                    columnDefs = list(
                      list(targets = 0, orderable = FALSE, className = "select-checkbox")
                    ),
                    select = list(
                      style = "multi", selector = "td:first-child",
                      headerCheckbox = TRUE
                    )), 
                  rownames = F
    )
  })
  
  output$final_screening_table <- DT::renderDataTable({
    df <- final_screen_data()
    validate(need(!is.null(df), "Upload screening data (and optional HIV-neg/REDCap) to see results."))
    
    DT::datatable(
      df,
      escape = FALSE,
      class = "display nowrap",
      extensions = "ColReorder",
      options = list(
        pageLength = 25,
        scrollX   = TRUE,
        colReorder = TRUE,
        select = list(style = "os", items = "cell")  # cell selection
      )
    )
  })
  
  
  ## Update HIV Neg list with new negatives - from filters to be added
  #new_neg_df <- reactive({
  #  req(screen_df_hivneg())
  #  df_list <- list(screen_df_hivneg(),backend_hivneg_df())
  #  new_neg_df <- data.table::rbindlist(df_list, fill = T) # Hopefully tables have same/similar columns?
  #  new_neg_df
  #})
  
  ## Matching ------------------------------------------------------------------
  
  observe({
    req(input$matching_file_upload$datapath)
    match_file(input$matching_file_upload$datapath)
  })
  
  observe({
    req(input$matching_file_upload$datapath)
    req(match_file())
    file <- match_file()
    ext <- tools::file_ext(file)
    if (ext == "csv") {
      df <- as.data.frame(readr::read_csv(file, skip = 3))
      match_df(df)
    } else if (ext %in% c("xlsx","xls")) {
      df <- as.data.frame(readxl::read_excel(file, skip = 2))
      match_df(df)
    }
  })
  match_columns_id <- reactiveVal(list())
  
  observe({
    req(match_df())
    df <- match_df()
    location_col_pred <- grep("location",colnames(df),ignore.case = T, value = T)[1]
    appt_col_pred <- grep("APPTDTTM|appointment",colnames(df),ignore.case = T, value = T)[1]
    age_col_pred <- grep("age",colnames(df),ignore.case = T, value = T)
    age_col_pred <- grep("language",age_col_pred, ignore.case = T, value = T, invert = T)[1]
    race_col_pred <- grep("race",colnames(df),ignore.case = T, value = T)[1]
    ethnicity_col_pred <- grep("ethnicity",colnames(df),ignore.case = T, value = T)[1]
    gender_col_pred <- grep("gender|sex",colnames(df),ignore.case = T, value = T)[1]
    visit_col_pred <- grep("visit|vtype",colnames(df),ignore.case = T, value = T)[1]
    diag_date_col_pred <- grep("DIAGNOSIS_DATE",colnames(df),ignore.case = T, value = T)[1]
    diag_notes_col_pred <- grep("DIAGNOSIS",colnames(df),ignore.case = T, value = T)
    diag_notes_col_pred <- diag_notes_col_pred[which(diag_notes_col_pred!=diag_date_col_pred)][1]
    notes_col_pred <- grep("NOTES",colnames(df),ignore.case = T, value = T)[1]
    
    col_id_list <- list(
      clinic_col = location_col_pred,
      appt_date_col = appt_col_pred,
      age_col = age_col_pred,
      race_col = race_col_pred,
      ethnicity_col = ethnicity_col_pred,
      gender_col = gender_col_pred,
      visit_col = visit_col_pred,
      diag_date_col = diag_date_col_pred,
      diag_notes_col = diag_notes_col_pred,
      notes_col = notes_col_pred
    )
    # Save back to reactive
    match_columns_id(col_id_list)
  })
  
  ### Filter UI ----------------------------------------------------------------
  
  observe({
    
    req(match_df(), match_columns_id())
    df <- match_df()
    match_col_id <- match_columns_id()

    if (isTruthy(input$hivpos_pat_to_match) & isTruthy(screen_columns_id()) & isTruthy(HIV_pos_df_patient_to_match())) {
      patient_data <- HIV_pos_df_patient_to_match()
      patient_mrn <- input$hivpos_pat_to_match
      screen_col_id <- screen_columns_id()

      clinic_pred <- patient_data[1,screen_col_id[["clinic_col"]]]
      age_pred <- patient_data[1,screen_col_id[["age_col"]]]
      race_pred <- patient_data[1,screen_col_id[["race_col"]]]
      ethnicity_pred <- patient_data[1,screen_col_id[["ethnicity_col"]]]
      gender_pred <- patient_data[1,screen_col_id[["gender_col"]]]
      visit_pred <- patient_data[1,screen_col_id[["visit_col"]]]
      appt_pred <- patient_data[1,screen_col_id[["appt_date_col"]]]
      diag_pred <- patient_data[1,screen_col_id[["diag_date_col"]]]
    } else {
      clinic_pred <- NULL
      age_pred <- NULL
      race_pred <- NULL
      ethnicity_pred <- NULL
      gender_pred <- NULL
      visit_pred <- NULL
      appt_pred <- NULL
      diag_pred <- NULL
    }
    
    updateSelectizeInput(session,"match_clinic", choices = unique(df[,match_col_id[["clinic_col"]]]), selected = clinic_pred)
    updateNumericInput(session, "age_to_match", value = age_pred)
    updateSelectizeInput(session,"match_race", choices = unique(df[,match_col_id[["race_col"]]]), selected = race_pred, server = T)
    updateSelectizeInput(session,"match_ethnicity", choices = unique(df[,match_col_id[["ethnicity_col"]]]), selected = ethnicity_pred, server = T)
    updateSelectizeInput(session,"match_gender", choices = unique(df[,match_col_id[["gender_col"]]]), selected = gender_pred, server = T)
    updateSelectizeInput(session,"match_vtype", choices = unique(df[,match_col_id[["visit_col"]]]), selected = visit_pred, server = T)
    updateDateRangeInput(session, "match_data_range",start = appt_pred,end = appt_pred)
    updateDateRangeInput(session, "match_diag_data_range",start = diag_pred,end = diag_pred)
  })
  
  # Store filter settings for each patient
  patient_filter_settings <- reactiveVal(list())
  
  # Update the filter settings when they change
  observe({
    req(input$hivpos_pat_to_match)
    
    current_settings <- list(
      clinic = input$match_clinic,
      appt_range_one = input$match_data_range[1],
      appt_range_two = input$match_data_range[2],
      age_start = input$age_to_match-input$age_range,
      age_stop = input$age_to_match+input$age_range,
      race = input$match_race,
      ethnicity = input$match_ethnicity,
      gender = input$match_gender,
      visit_type = input$match_vtype,
      key_term_diag_in = input$key_term_daig_in,
      key_term_diag_ex = input$key_term_daig_ex,
      key_term_notes_in = input$key_term_notes_in,
      key_term_notes_ex = input$key_term_notes_ex,
      diag_range_one = input$match_diag_data_range[1],
      diag_range_two = input$match_diag_data_range[2]
    )
    
    # Get existing settings
    all_settings <- patient_filter_settings()
    
    # Update settings for current patient
    all_settings[[input$hivpos_pat_to_match]] <- current_settings
    
    # Save back to reactive
    patient_filter_settings(all_settings)
  })
  
  ### Find Matches -------------------------------------------------------------
  
  observe({
    req(input$mrn_column)
    req(screen_df_hivpos())
    df <- screen_df_hivpos()
    hiv_pos_rowsSelected <- input$screen_hivpos_table_for_screening_rows_selected
    df_sub <- df[hiv_pos_rowsSelected,]
    hivpos_mrn <- unique(df_sub[,input$mrn_column])
    updateSelectizeInput(session,"hivpos_pat_to_match",choices = hivpos_mrn, selected = hivpos_mrn[1])
  })
  
  # will need to update for checkbox input table
  HIV_pos_df_patient_to_match <- reactive({
    req(screen_df_hivpos())
    req(input$hivpos_pat_to_match)
    req(input$mrn_column)
    df <- screen_df_hivpos()
    hiv_pos_rowsSelected <- input$screen_hivpos_table_for_screening_rows_selected
    df_sub <- df[hiv_pos_rowsSelected,]
    patient_mrn <- input$hivpos_pat_to_match
    df_sub_mrn <- df_sub[which(df_sub[,input$mrn_column] == patient_mrn),]
    df_sub_mrn
  })
  
  # Display HIV pos table for patient being matched
  output$HIV_Pos_table_for_matching <- DT::renderDataTable({
    req(HIV_pos_df_patient_to_match())
    df <- HIV_pos_df_patient_to_match()
    DT::datatable(df,
                  escape = F,
                  class = "display nowrap",
                  extensions = 'ColReorder',
                  options = list(paging = FALSE,
                                 info = FALSE,
                                 searching = FALSE,
                                 scrollX = T,
                                 target = "cell",
                                 colReorder = TRUE),
                  rownames = F
    )
  })

  HIV_neg_df_filtered_to_match <- reactive({
    req(match_df(), patient_filter_settings(), match_columns_id())
    
    df <- match_df()
    match_col_id <- match_columns_id()
    filter_list <- patient_filter_settings()
    
    df_filtered <- df %>%
      mutate(
        AGE = floor(as.numeric(!!sym(match_col_id[["age_col"]]))),  
        DIAGNOSIS_DATE = as.Date(parse_date_time(!!sym(match_col_id[["diag_date_col"]]), orders = c("ymd", "mdy", "dmy")))
      ) %>%
      #filter(!is.na(DIAGNOSIS_DATE)) %>% # Do we need this?
      filter(HIV == "No") %>%
      filter(!Textbox41 %in% c("Declined", "Withdraw")) %>%
      filter(PRIMARY_LANGUAGE %in% c("English", "Spanish")) %>%
      distinct()
    
    if (!is.null(input$age_to_match) && !is.na(input$age_to_match) && 
        !is.null(input$age_range) && !is.na(input$age_range) &&
        input$age_to_match != "" && input$age_range != "") {
      
      lower_bound <- as.numeric(input$age_to_match) - as.numeric(input$age_range)
      upper_bound <- as.numeric(input$age_to_match) + as.numeric(input$age_range)
      
      print(paste("Age filter applied: Between", lower_bound, "and", upper_bound))  # Debugging log
      
      df_filtered <- df_filtered %>%
        filter(!is.na(AGE)) %>%  # Ensure AGE is not NA before filtering
        filter(between(AGE, lower_bound, upper_bound))
    }
    
    # Apply conditional filters
    if (!is.null(input$match_clinic) && length(input$match_clinic) > 0) {
      df_filtered <- df_filtered %>% filter(LOCATION %in% input$match_clinic)
    }
    if (!is.null(input$match_race) && length(input$match_race) > 0) {
      df_filtered <- df_filtered %>%
        filter(PT_RACE %in% input$match_race)
    }
    if (!is.null(input$match_ethnicity) && length(input$match_ethnicity) > 0) {
      df_filtered <- df_filtered %>%
        filter(PT_ETHNICITY %in% input$match_ethnicity)
    }
    if (!is.null(input$match_gender) && length(input$match_gender) > 0) {
      df_filtered <- df_filtered %>%
        filter(PT_GENDER %in% input$match_gender)
    }
    if (!is.null(input$match_vtype) && length(input$match_vtype) > 0) {
      df_filtered <- df_filtered %>%
        filter(VTYPE %in% input$match_vtype)
    }
    if (!is.null(input$key_term_daig_in) && input$key_term_daig_in != "") {
      df_filtered <- df_filtered %>%
        filter(str_detect(DIAGNOSIS, regex(input$key_term_daig_in, ignore_case = TRUE)))
    }
    if (!is.null(input$key_term_daig_ex) && input$key_term_daig_ex != "") {
      df_filtered <- df_filtered %>%
        filter(!str_detect(DIAGNOSIS, regex(input$key_term_daig_ex, ignore_case = TRUE)))
    }
    if (!is.null(input$key_term_notes_in) && input$key_term_notes_in != "") {
      df_filtered <- df_filtered %>%
        filter(str_detect(NOTES, regex(input$key_term_notes_in, ignore_case = TRUE)))
    }
    if (!is.null(input$key_term_notes_ex) && input$key_term_notes_ex != "") {
      df_filtered <- df_filtered %>%
        filter(!str_detect(NOTES, regex(input$key_term_notes_ex, ignore_case = TRUE)))
    }
    if (!is.null(input$dxdate_start) && !is.null(input$dxdate_stop)) {
      df_filtered <- df_filtered %>%
        filter(between(DIAGNOSIS_DATE, as.Date(input$dxdate_start), as.Date(input$dxdate_stop)))
    }
    
    return(df_filtered)
  })
  
    output$filtered_table <- renderReactable({
    req(HIV_neg_df_filtered_to_match())
    df <- HIV_neg_df_filtered_to_match()
    
    df %>%
      #select(any_of(Variable_Name)) %>%
      #arrange(PT_LAST_NAME) %>%  # Arrange by last name
      reactable(
        sortable = TRUE,
        filterable = TRUE,
        compact = TRUE,
        resizable = TRUE,
        striped = TRUE,
        bordered = TRUE,
        showSortable = TRUE,
        columns = list(
          PRIMARY_LANGUAGE = colDef(name = "primary Language"),
          NOTES = colDef(name = "Notes", minWidth = 200),
          LOCATION = colDef(name = "Location"),
          PT_LAST_NAME = colDef(name = "Last name"),
          PT_FIRST_NAME = colDef(name = "First name"),
          DIAGNOSIS = colDef(name = "Diagnosis", minWidth = 150),
          DIAGNOSIS_DATE = colDef(name = "Diagnosis date"),
          PT_ETHNICITY = colDef(name = "Ethnicity"),
          Textbox38 = colDef(name = "Consent number", minWidth = 80),
          Textbox41 = colDef(name = "Consent status"),
          Textbox62 = colDef(name = "Consent version", minWidth = 120)
        ),
        theme = reactableTheme(
          headerStyle = list(
            "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
            "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
            backgroundColor = "lightblue"),
          borderColor = "#dfe2e9",
          stripedColor = "#f6f8fa",
          cellPadding = "8px 12px"
        )
      )
  })
  # Display HIV neg table for matching patients
  output$HIV_Neg_table_for_matching <- DT::renderDataTable({
    req(HIV_neg_df_filtered_to_match())  # Use the filtered dataset
    df <- HIV_neg_df_filtered_to_match()
    print(paste("Final dataset rows:", nrow(df)))  # Debugging
   DT::datatable(df,
                 escape = F,
                 class = "display nowrap",
                 extensions = 'ColReorder',
                 options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                pageLength = 20,
                                scrollX = T,
                                target = "cell",
                                colReorder = TRUE),
                 rownames = F
   )
  })
  
  
  ### Review Matches -----------------------------------------------------------
  
  
  # Display input file
  output$HIV_Pos_table_match_review <- DT::renderDataTable({
    req(HIV_pos_df_patient_to_match())
    df <- HIV_pos_df_patient_to_match()
    DT::datatable(df,
                  escape = F,
                  class = "display nowrap",
                  extensions = 'ColReorder',
                  options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                 pageLength = 20,
                                 scrollX = T,
                                 target = "cell",
                                 colReorder = TRUE),
                  rownames = F
    )
  })
  
  # Display input file
  output$HIV_Neg_table_match_review <- DT::renderDataTable({
    req(HIV_neg_df_filtered_to_match())
    df <- HIV_neg_df_filtered_to_match()
    DT::datatable(df,
                  escape = F,
                  class = "display nowrap",
                  extensions = 'ColReorder',
                  options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                 pageLength = 20,
                                 scrollX = T,
                                 target = "cell",
                                 colReorder = TRUE),
                  rownames = F
    )
  })

}














shinyApp(ui = ui, server = server)