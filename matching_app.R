library(shiny)
library(tidyverse)
library(janitor)
library(reactable)

dat <- read_csv("data/s10.2.23 to 10.6.23.csv",skip=3,col_names = TRUE) |> 
  ## need to remove extra "tables" at the end of the file
  filter(STATUS %in% c("CONFIRMED", "RESCHEDULED"))

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
                              "Asian Indian, Pakistani", "Black", "Chamorran",
                              "Chinese", "Filipino", "Hawaiian", "Japanese",
                              "Kampuchean (including Khmer, Cambodian)", "Korean",
                              "More Than One Race", "Other", 
                              "Other Asian including Asian and Oriental", 
                              "Pacific Islander", "Thai", "Tongan", "Unknown", 
                              "Vietnamese", "White"), 
                  selected = NULL, multiple = TRUE),
      selectInput("ethnicity", "Ethnicity filter:",
                  choices = c("Spanish; Hispanic", "Non-Spanish; non-Hispanic"),
                  selected = NULL),
      selectInput("gender", "Gender filter:", 
                  choices = c("Male", "Female"), selected = NULL),
      selectInput("vtype", "Visit Type filter:", 
                  choices = c("NP", "NEP"), selected = NULL, multiple = TRUE),
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
        DIAGNOSIS_DATE = parse_date_time(DIAGNOSIS_DATE, orders = c("ymd", "mdy", "dmy"))
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
        resizable = TRUE
      )
  })
}

shinyApp(ui, server)
