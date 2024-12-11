# install.packages("tidyverse")
# install.packages("here")
library(tidyverse)
library(here)

## this part you edit ##########################################################
## MRN for output file name
mrn <- "654321"

## csv file from SQL query
in_data <- "10.2.23 to 10.6.23.csv"



## parameter selections -----
location = "GI Clinic"
age_start = 55   ## any integer
age_stop = 65    ## any integer

race = NULL     ## Multiple allowed NULL or character vector eg. c("Black", "White")
## choices: c("American Indian, Aleutian, or Eskimo",
## "Asian Indian, Pakistani", "Black", "Chamorran", "Chinese", "Filipino",
## "Hawaiian", "Japanese", "Kampuchean (including Khmer, Cambodian)", "Korean",
## "More Than One Race", "Other", "Other Asian including Asian and Oriental",
## "Pacific Islander", "Thai", "Tongan", "Unknown", "Vietnamese", "White")#
## "Asian Indian, Pakistani", "Black", "Chamorran", "Chinese", "Filipino",
## "Hawaiian", "Japanese", "Kampuchean (including Khmer, Cambodian)", "Korean",
## "More Than One Race", "Other", "Other Asian including Asian and Oriental",
## "Pacific Islander", "Thai", "Tongan", "Unknown", "Vietnamese", "White")

ethnicity = NULL  ## [NULL, "Spanish; Hispanic", "Non-Spanish; non-Hispanic"]
gender = NULL     ## [NULL, "Male", "Female"]
vtype = c("NP", "NEP")      ## multiple allowed NULL or c("NP", "NEP")
key_term_daig_in = NULL  ## NULL or any text (not case sensitive)
key_term_daig_ex = NULL  ## NULL or any text (not case sensitive) - also removes missing (NA)
dxdate_start = NULL      ## NULL or any date as.Date("2023-11-16") - (note must be in ISO format YYY-MM-DD)
dxdate_stop = NULL       ## NULL or any date as.Date("2023-11-16") - (note must be in ISO format YYY-MM-DD)
key_term_notes_in = NULL  ## NULL or any text (not case sensitive)
key_term_notes_ex = NULL  ## NULL or any text (not case sensitive) - also removes missing (NA)
print_code = TRUE        ## [TRUE, FALSE]




################################################################################

## Don't edit - just run ==== 

################################################################################

date_range <- str_remove(in_data, "\\.csv|\\.xlsx")

## create final file name
report_file_name <- paste0("MRN-", mrn, "_", date_range, ".html")


## Render the document
rmarkdown::render(
  input = here::here("code", "matching_d2.Rmd"),
  output_file = report_file_name,
  output_format = "html_document",
  output_dir = here::here("Matches"),
  params = list(
    data =             in_data,
    location =         location,
    age_start =        age_start,
    age_stop =         age_stop,
    race =             race,
    ethnicity =        ethnicity,
    gender =           gender,
    vtype =            vtype,
    key_term_daig_in = key_term_daig_in,
    key_term_daig_ex = key_term_daig_ex,
    dxdate_start =     dxdate_start,
    dxdate_stop =      dxdate_stop,
    key_term_notes_in = key_term_notes_in,
    key_term_notes_ex = key_term_notes_ex,
    out_file_mrn =     mrn,
    print_code =       print_code
  )
  
)
