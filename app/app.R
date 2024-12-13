
version_id <- paste0("v0.0.9")


# Back end input ---------------------------------------------------------------

Known_HIVNEG_File <- "data/TCC_Screening_KnownHIVNegatives_20241212.csv"




# UI ---------------------------------------------------------------------------

ui <-
  shiny::navbarPage("{ SMART }",
                    id = "navbar_id",
                    theme = bslib::bs_theme(bootswatch = "yeti"),
                    ## Homepage ------------------------------------------------
                    #shiny::tabPanel("Homepage",
                    #                # Explain SMART acronym what it can be used for - any logos
                    #),
                    ## Screening -----------------------------------------------
                    shiny::tabPanel("Screening",
                                    shiny::sidebarLayout(
                                      shiny::sidebarPanel(
                                        width = 3,
                                        h4("Load Input Data"),
                                        shiny::fileInput("screening_file_upload","Upload Screening Data"),
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
                                                                    DT::dataTableOutput("screen_table_for_screening"),
                                                                    value = 1
                                                                    ),
                                                           tabPanel("HIV Positive Patients",
                                                                    p(),
                                                                    DT::dataTableOutput("screen_hivpos_table_for_screening"),
                                                                    value = 2
                                                                    ),
                                                           tabPanel("HIV Negative Patients",
                                                                    p(),
                                                                    DT::dataTableOutput("screen_hivneg_table_for_screening"),
                                                                    value = 3
                                                                    )
                                                           )
                                      )
                                    )
                    ),
                    ## Matching ------------------------------------------------
                    shiny::tabPanel("Matching",
                                    shiny::sidebarLayout(
                                      shiny::sidebarPanel(
                                        width = 3,
                                        conditionalPanel(condition = "input.matching_panel == '1'",
                                                         p(),
                                                         shiny::fileInput("matching_file_upload","Upload Matching Data"),
                                                         hr(),
                                                         h4("Select Patient to Match"),
                                                         selectizeInput("hivpos_pat_to_match","Select HIV Positive Patient to Find Match:", choices = NULL, selected = 1),
                                                         hr(),
                                                         h4("Filters"),
                                                         dateRangeInput("match_data_range","Appointment Date Range:"),
                                                         selectizeInput("match_clinic","Clinic Location:", choices = NULL, selected = NULL, multiple = TRUE),
                                                         fluidRow(
                                                           column(6,
                                                                  numericInput("age_to_match","Age:",value = NULL),
                                                                  selectizeInput("match_race","Race:", choices = NULL, selected = NULL, multiple = T),
                                                                  selectizeInput("match_gender","Gender:", choices = NULL, selected = NULL, multiple = T)
                                                                  ),
                                                           column(6,
                                                                  numericInput("age_range","+/- years:", value = NULL, min = 0, step = 1),
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
                                                                    h3(tags$b("HIV Positive Patient to Match")),
                                                                    DT::dataTableOutput("HIV_Pos_table_for_matching"),
                                                                    hr(),
                                                                    h3(tags$b("HIV Negative Patients for Matching")),
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
  
  screen_file <- reactiveVal(NULL)
  screen_df <- reactiveVal(NULL)
  screen_df_hivpos <- reactiveVal(NULL)
  screen_df_hivneg <- reactiveVal(NULL)
  
  match_file <- reactiveVal()
  match_df <- reactiveVal()
  
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
      df <- as.data.frame(readr::read_csv(file, skip = 3)) # might need to update depending on input format
      #df <- df %>%
      #  filter(STATUS %in% c("CONFIRMED", "RESCHEDULED"))
      screen_df(df)
    } else if (ext %in% c("xlsx","xls")) {
      df <- as.data.frame(readxl::read_excel(file, skip = 2)) # add skip row number input? or predict?
      #df <- df %>%
      #  filter(STATUS %in% c("CONFIRMED", "RESCHEDULED"))
      #df <- readxl::read_excel(file, col_names = F)
      #df <- df[cumsum(complete.cases(df)) != 0, ]
      #colnames(df) <- df[1,]
      #df <- df[-1,]
      screen_df(df)
      #save(list = ls(), file = "tesst.RData", envir = environment())
      mrn_col_pred <- grep("mrn",colnames(df),ignore.case = T, value = T)[1]
      hiv_col_pred <- grep("hiv",colnames(df),ignore.case = T, value = T)[1]
      updateSelectizeInput(session,"mrn_column", choices = colnames(df), selected = mrn_col_pred, server = T)
      updateSelectizeInput(session,"hiv_present_column", choices = colnames(df), selected = hiv_col_pred, server = T)
    }
  })
  
  screen_columns_id <- reactiveVal(list())
  
  observe({
    req(screen_df())
    df <- screen_df()
    location_col_pred <- grep("location",colnames(df),ignore.case = T)
    appt_col_pred <- grep("APPTDTTM|appointment",colnames(df),ignore.case = T)
    age_col_pred <- grep("age",colnames(df),ignore.case = T)
    race_col_pred <- grep("race",colnames(df),ignore.case = T)
    ethnicity_col_pred <- grep("ethnicity",colnames(df),ignore.case = T)
    gender_col_pred <- grep("gender|sex",colnames(df),ignore.case = T)
    visit_col_pred <- grep("visit|vtype",colnames(df),ignore.case = T)
    diag_date_col_pred <- grep("DIAGNOSIS_DATE",colnames(df),ignore.case = T)
    diag_notes_col_pred <- grep("DIAGNOSIS",colnames(df),ignore.case = T)
    diag_notes_col_pred <- diag_notes_col_pred[which(diag_notes_col_pred!=diag_date_col_pred)]
    notes_col_pred <- grep("NOTES",colnames(df),ignore.case = T)
    
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
  
  screen_df_filtered <- reactive({
    req(screen_df())
    df <- screen_df()
    
    # apply filters
    
    df
  })
  
  observe({
    req(screen_df_filtered())
    req(input$hiv_present_column)
    req(input$mrn_column)
    df <- screen_df_filtered()
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
    
    screen_df_hivpos(df_pos)
    screen_df_hivneg(df_neg)
  })
  
  # Display input file
  output$screen_table_for_screening <- DT::renderDataTable({
    req(screen_df_filtered())
    df <- screen_df_filtered()
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
  output$screen_hivneg_table_for_screening <- DT::renderDataTable({
    req(screen_df_hivneg())
    df <- screen_df_hivneg()
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
    location_col_pred <- grep("location",colnames(df),ignore.case = T)
    appt_col_pred <- grep("APPTDTTM|appointment",colnames(df),ignore.case = T)
    age_col_pred <- grep("age",colnames(df),ignore.case = T)
    race_col_pred <- grep("race",colnames(df),ignore.case = T)
    ethnicity_col_pred <- grep("ethnicity",colnames(df),ignore.case = T)
    gender_col_pred <- grep("gender|sex",colnames(df),ignore.case = T)
    visit_col_pred <- grep("visit|vtype",colnames(df),ignore.case = T)
    diag_date_col_pred <- grep("DIAGNOSIS_DATE",colnames(df),ignore.case = T)
    diag_notes_col_pred <- grep("DIAGNOSIS",colnames(df),ignore.case = T)
    diag_notes_col_pred <- diag_notes_col_pred[which(diag_notes_col_pred!=diag_date_col_pred)]
    notes_col_pred <- grep("NOTES",colnames(df),ignore.case = T)
    
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
    hivpos_mrn <- unique(df[,input$mrn_column])
    updateSelectizeInput(session,"hivpos_pat_to_match",choices = hivpos_mrn, selected = hivpos_mrn[1])
  })
  
  # wil need to update for checkbox input table
  HIV_pos_df_patient_to_match <- reactive({
    req(screen_df_hivpos())
    req(input$hivpos_pat_to_match)
    req(input$mrn_column)
    df <- screen_df_hivpos()
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

  HIV_neg_df_filtered_to_match <- reactive({
    req(match_df(),patient_filter_settings(),columns_id())
    df <- match_df()
    col_id <- columns_id()
    filter_list <- patient_filter_settings()
    
    df_filtered <- df %>%
      mutate(
        AGE = floor(!!sym(filter_list[["age_col"]])),  # Round down age
        DIAGNOSIS_DATE = as.Date(parse_date_time(!!sym(filter_list[["diag_date_col"]]), orders = c("ymd", "mdy", "dmy"))),  # Convert to date
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
    req(match_df())
    df <- match_df()
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