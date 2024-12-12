library(shiny)
library(tidyverse)
library(janitor)
library(reactable)

dat <- read_csv("data/s10.2.23 to 10.6.23.csv",skip=3,col_names = TRUE) |> 
  ## need to remove extra "tables" at the end of the file
  filter(STATUS %in% c("CONFIRMED", "RESCHEDULED"))

dat$NOTES <- str_to_sentence(dat$NOTES)

ui <- fluidPage(
  titlePanel("Dataset Filtering"),
  sidebarLayout(
    sidebarPanel(
      textInput("data", "Input dataset:", value = "s10.2.23 to 10.6.23.csv"),
      textInput("location", "Relevant Clinic Location", value = "GI Clinic"),
      numericInput("age_start", "Starting Age:", value = 55, min = 0),
      numericInput("age_stop", "Stopping Age:", value = 65, min = 0),
      selectInput("race", "Race filter:", 
                  choices = c("American Indian, Aleutian, or Eskimo",
                              "Asian Indian, Pakistani",
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
                              "White"), 
                  selected = NULL, multiple = TRUE),
      selectInput("ethnicity", "Ethnicity filter:",
                  choices = c("Spanish; Hispanic", "Non-Spanish; non-Hispanic"),
                  selected = NULL),
      selectInput("gender", "Gender filter:", 
                  choices = c("Male", "Female"), selected = NULL),
      selectInput("vtype", "Visit Type filter:", 
                  choices = c("NP", "NEP"), selected = NULL, multiple = TRUE),
      textInput("key_term_daig_in", "Include Diagnosis Key Term:", value = ""),
      textInput("key_term_daig_ex", "Exclude Diagnosis Key Term:", value = ""),
      textInput("key_term_notes_in", "Include Notes Key Term:", value = ""),
      textInput("key_term_notes_ex", "Exclude Notes Key Term:", value = ""),
      dateInput("dxdate_start", "Starting Dx Date:", value = NULL),
      dateInput("dxdate_stop", "Stopping Dx Date:", value = NULL)
    ),
    mainPanel(
      reactableOutput("filtered_table")
    )
  )
)

server <- function(input, output, session) {
  # Reactive to calculate date range
  date_range <- reactive({
    diagnosis_dates <- dat$DIAGNOSIS_DATE %>% 
      lubridate::parse_date_time(orders = c("ymd", "mdy", "dmy"), quiet = TRUE) %>% 
      na.omit()
    list(start = min(diagnosis_dates, na.rm = TRUE),
         stop = max(diagnosis_dates, na.rm = TRUE))
  })

  observe({
    updateDateInput(session, "dxdate_start", value = date_range()$start)
    updateDateInput(session, "dxdate_stop", value = date_range()$stop)
  })
  
  output$filtered_table <- renderReactable({
    req(input$location, input$age_start, input$age_stop)
    
    Variable_Name <- c("LOCATION", "MRN", "PT_LAST_NAME", "PT_FIRST_NAME",
                       "DIAGNOSIS", "DIAGNOSIS_DATE", "AGE", "DOB", 
                       "PT_GENDER", "PT_RACE", "PT_ETHNICITY", 
                       "PRIMARY_LANGUAGE", "Textbox62", "Textbox41", 
                       "Textbox38", "APPTDTTM", "RESOURCE", "VTYPE", 
                       "NOTES", "HIV")
    
    dat_filtered <- dat %>% 
      select(any_of(c("LOCATION", "MRN", "PT_LAST_NAME", "PT_FIRST_NAME",
                      "DIAGNOSIS", "DIAGNOSIS_DATE", "AGE", "DOB", 
                      "PT_GENDER", "PT_RACE", "PT_ETHNICITY", 
                      "PRIMARY_LANGUAGE", "Textbox62", "Textbox41", 
                      "Textbox38", "APPTDTTM", "RESOURCE", "VTYPE", 
                      "NOTES", "HIV"))) %>%
      mutate(
        AGE = floor(AGE),  # Round down age
        DIAGNOSIS_DATE = as.Date(parse_date_time(DIAGNOSIS_DATE, orders = c("ymd", "mdy", "dmy"))),  # Convert to date
        DOB = as.Date(DOB, format = "%m/%d/%y"),
        DOB = ifelse(year(DOB) > 2024, DOB - years(100), DOB), # Adjust future years to the past
        DOB = as.Date(DOB)
      ) %>%
      filter(!is.na(DIAGNOSIS_DATE)) %>%
      filter(LOCATION %in% input$location) %>%
      filter(HIV == "No") %>%
      filter(!Textbox41 %in% c("Declined", "Withdraw")) %>%
      filter(PRIMARY_LANGUAGE %in% c("English", "Spanish")) %>%
      filter(between(AGE, input$age_start, input$age_stop)) %>%
      distinct()
    
    # Conditional filters
    if (!is.null(input$race)) {
      dat_filtered <- dat_filtered %>%
        filter(PT_RACE %in% input$race)
    }
    if (!is.null(input$ethnicity)) {
      dat_filtered <- dat_filtered %>%
        filter(PT_ETHNICITY %in% input$ethnicity)
    }
    if (!is.null(input$gender)) {
      dat_filtered <- dat_filtered %>%
        filter(PT_GENDER %in% input$gender)
    }
    if (!is.null(input$vtype)) {
      dat_filtered <- dat_filtered %>%
        filter(VTYPE %in% input$vtype)
    }
    if (!is.null(input$key_term_daig_in) && input$key_term_daig_in != "") {
      dat_filtered <- dat_filtered %>%
        filter(str_detect(DIAGNOSIS, input$key_term_daig_in))
    }
    if (!is.null(input$key_term_daig_ex) && input$key_term_daig_ex != "") {
      dat_filtered <- dat_filtered %>%
        filter(str_detect(DIAGNOSIS, input$key_term_daig_ex, negate = TRUE))
    }
    if (!is.null(input$key_term_notes_in) && input$key_term_notes_in != "") {
      dat_filtered <- dat_filtered %>%
        filter(str_detect(DIAGNOSIS, input$key_term_notes_in))
    }
    if (!is.null(input$key_term_notes_ex) && input$key_term_notes_ex != "") {
      dat_filtered <- dat_filtered %>%
        filter(str_detect(DIAGNOSIS, input$key_term_notes_ex, negate = TRUE))
    }
    if (!is.null(input$dxdate_start) & !is.null(input$dxdate_stop)) {
      dat_filtered <- dat_filtered %>%
        filter(between(DIAGNOSIS_DATE, as.Date(input$dxdate_start), as.Date(input$dxdate_stop)))
    }
    
    
    dat_filtered %>%
      select(any_of(Variable_Name)) %>%
      arrange(PT_LAST_NAME) %>%  # Arrange by last name
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
}

shinyApp(ui, server)
