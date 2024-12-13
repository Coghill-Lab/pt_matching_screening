
version_id <- paste0("v0.0.9")


# Back end input ---------------------------------------------------------------

Known_HIVNEG_File <- "data/TCC_Screening_KnownHIVNegatives_20241212.csv"




# UI ---------------------------------------------------------------------------

ui <-
  shiny::navbarPage(title = span("{ SMART }", style = "color:white; font-weight:bold;"),
                    id = "navbar_id",
                    theme = bslib::bs_theme(bootswatch = "yeti"),
                    tags$head(
                      tags$link(rel = "stylesheet", type = "text/css", href = "styling.css")
                    ),
                    ## Homepage ------------------------------------------------
                    shiny::tabPanel("Homepage",
                                    icon = icon("home")
                                    # Explain SMART acronym what it can be used for - any logos
                    ),
                    ## Screening -----------------------------------------------
                    shiny::tabPanel("Screening",
                                    icon = icon("search",lib = "font-awesome"),
                                    shiny::sidebarLayout(
                                      shiny::sidebarPanel(
                                        width = 3,
                                        h4("Load Input Data"),
                                        shiny::fileInput("input_file_upload","Upload Input Data"),
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
                                                         selectizeInput("hiv_present_column","HIV Indicator Column:", choices = NULL, selected = 1)
                                                         # Filters
                                                         )
                                      ),
                                      shiny::mainPanel(
                                        shiny::tabsetPanel(id = "screening_panel",
                                                           tabPanel("Input Data",
                                                                    p(),
                                                                    DT::dataTableOutput("input_table_for_screening"),
                                                                    value = 1
                                                                    ),
                                                           tabPanel("HIV Positive Patients",
                                                                    p(),
                                                                    DT::dataTableOutput("input_hivpos_table_for_screening"),
                                                                    value = 2
                                                                    ),
                                                           tabPanel("HIV Negative Patients",
                                                                    p(),
                                                                    DT::dataTableOutput("input_hivneg_table_for_screening"),
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
                                                         h4("Select Patient to Match", style = "font-size: 24px; font-weight: bold;"),
                                                         selectizeInput("hivpos_pat_to_match","Select HIV Positive Patient to Find Match:", choices = NULL, selected = 1),
                                                         hr(),
                                                         h4("Filters",style = "font-size: 20px;"),
                                                         dateRangeInput("match_data_range","Appointment Date Range:"),
                                                         selectizeInput("match_clinic","Clinic Location:", choices = NULL, selected = NULL, multiple = TRUE),
                                                         fluidRow(
                                                           column(6,
                                                                  numericInput("age_to_match","Age:",value = NULL),
                                                                  selectizeInput("match_race","Race:", choices = NULL, selected = NULL, multiple = T),
                                                                  selectizeInput("match_gender","Gender:", choices = NULL, selected = NULL, multiple = T)
                                                                  ),
                                                           column(6,
                                                                  numericInput("age_range","+/- years:", value = 5, min = 0, step = 1),
                                                                  selectizeInput("match_ethnicity","Ethnicity:", choices = NULL, selected = NULL, multiple = T),
                                                                  selectizeInput("match_vtype","Visit Type:", choices = NULL, selected = NULL, multiple = T)
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
                                                         # Exporting data
                                                         )
                                      ),
                                      shiny::mainPanel(
                                        shiny::tabsetPanel(id = "matching_panel",
                                                           tabPanel("Find Matches",
                                                                    p(),
                                                                    h3(tags$b("HIV Positive Patient to Match"),style = "font-size: 20px;"),
                                                                    DT::dataTableOutput("HIV_Pos_table_for_matching"),
                                                                    hr(),
                                                                    h3(tags$b("HIV Negative Patients for Matching"),style = "font-size: 20px;"),
                                                                    DT::dataTableOutput("HIV_Neg_table_for_matching"),
                                                                    value = 1
                                                                    ),
                                                           tabPanel("Review Matches",
                                                                    p(),
                                                                    fluidRow(
                                                                      column(6,
                                                                             h3(tags$b("HIV Positive Patients")),
                                                                             DT::dataTableOutput("HIV_Pos_table_match_review")
                                                                             ),
                                                                      column(6,
                                                                             h3(tags$b("HIV Negative Patients")),
                                                                             DT::dataTableOutput("HIV_Neg_table_match_review")
                                                                             )
                                                                    ),
                                                                    value = 2
                                                                    )
                                                           )
                                      )
                                    )
                    ),
                    ## Recognition ---------------------------------------------
                    shiny::tabPanel("Recognition",
                                    icon = icon("award"),
                                    shiny::sidebarLayout(
                                      shiny::sidebarPanel(
                                        width = 3,
                                        
                                      ),
                                      shiny::mainPanel(
                                        
                                      )
                                    )
                    )
  )




# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  ## Reactive Vals -------------------------------------------------------------
  backend_known_hivneg_file <- reactiveVal(NULL)
  observe({
    if (file.exists(Known_HIVNEG_File)) {
      backend_known_hivneg_file(Known_HIVNEG_File)
    } else {
      backend_known_hivneg_file(NULL)
    }
  })
  backend_hivneg_df <- reactiveVal(NULL)
  
  input_file <- reactiveVal(NULL)
  input_df <- reactiveVal(NULL)
  input_df_hivpos <- reactiveVal(NULL)
  input_df_hivneg <- reactiveVal(NULL)
  
  output_hivpos_df <- reactiveVal(NULL)
  output_hivneg_df <- reactiveVal(NULL)
  
  hiv_matched_list <- reactiveVal(NULL)
  
  # Read in know HIV Neg file
  observe({
    req(backend_known_hivneg_file())
    file <- backend_known_hivneg_file()
    ext <- tools::file_ext(file)
    if (ext == "csv") {
      df <- as.data.frame(readr::read_csv(file)) # might need to update depending on input format
      backend_hivneg_df(df)
    } else if (ext %in% c("xlsx","xls")) {
      df <- as.data.frame(readxl::excel_sheets(file))
      backend_hivneg_df(df)
    }
  })
  
  ## Homepage ------------------------------------------------------------------
  
  
  ## Screening -----------------------------------------------------------------
  
  # Update input csv file name
  observe({
    req(input$input_file_upload$datapath)
    input_file(input$input_file_upload$datapath)
  })
  # Read in input file
  observe({
    req(input_file())
    file <- input_file()
    ext <- tools::file_ext(file)
    if (ext == "csv") {
      df <- as.data.frame(readr::read_csv(file, skip = 3)) # might need to update depending on input format
      #df <- df %>%
      #  filter(STATUS %in% c("CONFIRMED", "RESCHEDULED"))
      input_df(df)
    } else if (ext %in% c("xlsx","xls")) {
      df <- as.data.frame(readxl::read_excel(file, skip = 2)) # add skip row number input? or predict?
      #df <- df %>%
      #  filter(STATUS %in% c("CONFIRMED", "RESCHEDULED"))
      #df <- readxl::read_excel(file, col_names = F)
      #df <- df[cumsum(complete.cases(df)) != 0, ]
      #colnames(df) <- df[1,]
      #df <- df[-1,]
      input_df(df)
      mrn_col_pred <- grep("mrn",colnames(df),ignore.case = T, value = T)[1]
      hiv_col_pred <- grep("hiv",colnames(df),ignore.case = T, value = T)[1]
      updateSelectizeInput(session,"mrn_column", choices = colnames(df), selected = mrn_col_pred, server = T)
      updateSelectizeInput(session,"hiv_present_column", choices = colnames(df), selected = hiv_col_pred, server = T)
    }
  })
  
  input_df_filtered <- reactive({
    req(input_df())
    df <- input_df()
    
    # apply filters
    
    df
  })
  
  observe({
    req(input_df_filtered())
    req(input$hiv_present_column)
    req(input$mrn_column)
    df <- input_df_filtered()
    hiv_col_pred <- input$hiv_present_column
    known_hivneg <- backend_hivneg_df()
    mrn_col <- input$mrn_column
    
    
    # apply filters
    
    df_pos <- df[which(df[,hiv_col_pred] == "Yes"),]
    df_neg <- df[which(df[,hiv_col_pred] == "No"),]
    # remove known negatives
    if (isTruthy(known_hivneg)) {
      hivneg_mrns <- known_hivneg[,1]
      df_pos_kn <- df_pos[which(df_pos[,mrn_col] %in% hivneg_mrns),]
      df_neg <- rbind(df_neg,df_pos_kn)
    }
    
    input_df_hivpos(df_pos)
    input_df_hivneg(df_neg)
  })
  
  # Display input file
  output$input_table_for_screening <- DT::renderDataTable({
    req(input_df_filtered())
    df <- input_df_filtered()
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
  output$input_hivpos_table_for_screening <- DT::renderDataTable({
    req(input_df_hivpos())
    df <- input_df_hivpos()
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
  # Display input hiv negative file
  output$input_hivneg_table_for_screening <- DT::renderDataTable({
    req(input_df_hivneg())
    df <- input_df_hivneg()
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
  
  ## Update HIV Neg list with new negatives - from filters to be added
  #new_neg_df <- reactive({
  #  req(input_df_hivneg())
  #  df_list <- list(input_df_hivneg(),backend_hivneg_df())
  #  new_neg_df <- data.table::rbindlist(df_list, fill = T) # Hopefully tables have same/similar columns?
  #  new_neg_df
  #})
  
  ## Matching ------------------------------------------------------------------
  
  ### Save Patient Filters -----------------------------------------------------
  

  # Store filter settings for each patient
  patient_filter_settings <- reactiveVal(list())
  
  # Update the filter settings when they change
  observe({
    req(input$hivpos_pat_to_match)
    
    current_settings <- list(
      age = input$age_to_match,
      age_range = input$age_range,
      race = input$match_race,
      ethnicity = input$match_ethnicity,
      gender = input$match_gender,
      clinic = input$match_clinic,
      visit_type = input$match_vtype,
      date_range = input$match_data_range,
      diag_date_range = input$match_diag_data_range,
      key_term_diag_in = input$key_term_daig_in,
      key_term_diag_ex = input$key_term_daig_ex,
      key_term_notes_in = input$key_term_notes_in,
      key_term_notes_ex = input$key_term_notes_ex
    )
    
    # Get existing settings
    all_settings <- patient_filter_settings()
    
    # Update settings for current patient
    all_settings[[input$hivpos_pat_to_match]] <- current_settings
    
    # Save back to reactive
    patient_filter_settings(all_settings)
  })
  
  ### Filter UI ----------------------------------------------------------------
  
  # Create a reactive expression that gets the selected patient's data
  selected_hiv_patient <- reactive({
    req(input_df(), input$hivpos_pat_to_match, input$mrn_column)
    df <- input_df()
    df[df$mrn_column == input$hivpos_pat_to_match, ]
  })
  
  # Modify the observe to use the selected patient's data
  observe({
    req(input_df(), input$hivpos_pat_to_match)
    
    # Get stored settings for this patient
    all_settings <- patient_filter_settings()
    stored_settings <- all_settings[[input$hivpos_pat_to_match]]
    
    df <- input_df()
    patient_data <- selected_hiv_patient()
    
    # Get column indices
    location_col_pred <- grep("location", colnames(df), ignore.case = TRUE)
    race_col_pred <- grep("race", colnames(df), ignore.case = TRUE)
    ethnicity_col_pred <- grep("ethnicity", colnames(df), ignore.case = TRUE)
    gender_col_pred <- grep("gender|sex", colnames(df), ignore.case = TRUE)
    visit_col_pred <- grep("visit", colnames(df), ignore.case = TRUE)
    
    # Update the inputs with the selected patient's values
    updateSelectizeInput(session, "match_clinic", choices = unique(df[,location_col_pred]), 
                         selected = patient_data[1, location_col_pred])
    
    updateSelectizeInput(session, "match_race", choices = unique(df[,race_col_pred]), 
                         selected = patient_data[1, race_col_pred], server = TRUE)
    
    updateSelectizeInput(session, "match_ethnicity", choices = unique(df[,ethnicity_col_pred]), 
                         selected = patient_data[1, ethnicity_col_pred], server = TRUE)
    
    updateSelectizeInput(session, "match_gender", choices = unique(df[,gender_col_pred]), 
                         selected = patient_data[1, gender_col_pred], server = TRUE)
    
    updateSelectizeInput(session, "match_vtype", choices = unique(df[,visit_col_pred]), 
                         selected = patient_data[1, visit_col_pred], server = TRUE)
  
    ### Auto Populate Filters with Patient data --------------------------------
    selected_patient <- df[df[[input$mrn_column]] == input$hivpos_pat_to_match, ]
    # For debugging
    # print(selected_patient)
    # print(stored_settings)


    # Update race, gender, ethnicity
    if("Race" %in% colnames(df)) {
      updateSelectizeInput(session, "match_race",
                           choices = unique(df$Race), #couldn't get it to work using race_col_pred
                           selected = selected_patient$Race[1])
    }
      updateSelectizeInput(session, "match_gender",
                           choices = unique(df$Gender),
                           selected = selected_patient$Gender[1])

      updateSelectizeInput(session, "match_ethnicity",
                           choices = unique(df$Ethnicity),
                           selected = selected_patient$Ethnicity[1])
    
    # Update date range inputs
      updateDateRangeInput(session, "match_data_range",
                           start = patient_data[1, "appointment_date"],
                           end = patient_data[1, "appointment_date"])


      updateDateRangeInput(session, "match_diag_data_range",
                           start = patient_data[1, "diagnosis_date"],
                           end = patient_data[1, "diagnosis_date"])
    
  })
  
  ### Find Matches -------------------------------------------------------------
  
  observe({
    #req(output_hivpos_mrn())
    req(input$mrn_column)
    req(input_df_hivpos())
    df <- input_df_hivpos()
    hivpos_mrn <- unique(df[,input$mrn_column])
    updateSelectizeInput(session,"hivpos_pat_to_match",choices = hivpos_mrn, selected = hivpos_mrn[1])
  })
  
  HIV_pos_df_patient_to_match <- reactive({
    req(input_df_hivpos())
    req(input$hivpos_pat_to_match)
    req(input$mrn_column)
    df <- input_df_hivpos()
    patient_mrn <- input$hivpos_pat_to_match
    df_mrn <- df[which(df[,input$mrn_column] == patient_mrn),]
    df_mrn
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
  
  # Display HIV neg table for matching patients
  output$HIV_Neg_table_for_matching <- DT::renderDataTable({
    req(input_df_hivneg())
    df <- input_df_hivneg()
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
  
  
  ## Recognition ---------------------------------------------------------------
  
  
}














shinyApp(ui = ui, server = server)