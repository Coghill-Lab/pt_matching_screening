
version_id <- paste0("v0.0.9")


# Back end input ---------------------------------------------------------------

Confirmed_Known_HIVNEG_File <- ""

Existing_Known_HIVNEG_File <- "data/10.2.23 to 10.6.23_HIVNEG.csv"



# UI ---------------------------------------------------------------------------

ui <-
  shiny::navbarPage("{ SMART }",
                    id = "navbar_id",
                    theme = bslib::bs_theme(bootswatch = "yeti"),
                    ## Homepage ------------------------------------------------
                    shiny::tabPanel("Homepage",
                                    # Explain SMART acronym what it can be used for - any logos
                    ),
                    ## Screening -----------------------------------------------
                    shiny::tabPanel("Screening",
                                    shiny::sidebarLayout(
                                      shiny::sidebarPanel(
                                        width = 3,
                                        h4("Load Input Data"),
                                        shiny::fileInput("input_file_upload","Upload Input Data"),
                                        conditionalPanel(condition = "input.screening_panel == '1'",
                                                         hr(),
                                                         h4("Filter Patients")
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
                                    shiny::sidebarLayout(
                                      shiny::sidebarPanel(
                                        width = 3,
                                        conditionalPanel(condition = "input.matching_panel == '1'",
                                                         h4("Select Patient to Match"),
                                                         selectizeInput("hivpos_pat_to_match","Select HIV Positive Patient to Find Match:", choices = NULL, selected = 1),
                                                         hr(),
                                                         h4("Filter Matches"),
                                                         # Filters
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
  input_file <- reactiveVal(NULL)
  backend_hivneg_file <- reactiveVal(Existing_Known_HIVNEG_File)
  backend_known_hivneg_file <- reactiveVal(Confirmed_Known_HIVNEG_File)
  
  input_df <- reactiveVal(NULL)
  input_df_hivpos <- reactiveVal(NULL)
  input_df_hivneg <- reactiveVal(NULL)
  
  backend_hivneg_df <- reactiveVal(NULL)
  backend_hivneg_mrn <- reactiveVal(NULL)
  
  input_hivpos_mrn <- reactiveVal(NULL)
  input_hivneg_mrn <- reactiveVal(NULL)
  
  output_hivpos_mrn <- reactiveVal(NULL)
  output_hivneg_mrn <- reactiveVal(NULL)
  hiv_matched_list <- reactiveVal(NULL)
  
  # Read in existing HIV Neg csv file
  observe({
    req(backend_hivneg_file())
    file <- backend_hivneg_file()
    ext <- tools::file_ext(file)
    if (ext == "csv") {
      df <- readr::read_csv(file) # might need to update depending on input format
      backend_hivneg_df(df)
    } else if (ext %in% c("xlsx","xls")) {
      df <- readxl::excel_sheets(file)
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
      df <- readr::read_csv(file, skip = 3) # might need to update depending on input format
      input_df(df)
    } else if (ext %in% c("xlsx","xls")) {
      df <- readxl::excel_sheets(file)
      input_df(df)
    }
    # Subset HIV pos and neg here?
    df_pos <- df[which(df$HIV == "Yes"),]
    df_neg <- df[which(df$HIV == "No"),]
    input_df_hivpos(df_pos)
    input_df_hivneg(df_neg)
  })
  
  # Display input file
  output$input_table_for_screening <- DT::renderDataTable({
    req(input_df())
    df <- input_df()
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
  
  # Update HIV Neg list with new negatives - from filters to be added
  new_neg_df <- reactive({
    req(input_df_hivneg())
    req(backend_hivneg_df())
    df_list <- list(input_df_hivneg(),backend_hivneg_df())
    new_neg_df <- data.table::rbindlist(df_list, fill = T) # Hopefully tables have same/similar columns?
    new_neg_df
  })
  
  ## Matching ------------------------------------------------------------------
  
  ### Find Matches -------------------------------------------------------------
  
  # Update HIV Pos MRNs that will be matched
  observe({
    hiv_pos_mrn <- c("99091","99107","99125","99162") # temp testing code - will be from check box input on data table
    output_hivpos_mrn(hiv_pos_mrn)
  })
  observe({
    req(output_hivpos_mrn())
    req(input_df_hivpos())
    hivpos_mrn <- output_hivpos_mrn()
    updateSelectizeInput(session,"hivpos_pat_to_match",choices = hivpos_mrn, selected = hivpos_mrn[1])
  })
  
  HIV_pos_df_patient_to_match <- reactive({
    req(input_df_hivpos())
    req(input$hivpos_pat_to_match)
    df <- input_df_hivpos()
    patient_mrn <- input$hivpos_pat_to_match
    df_mrn <- df[which(df$MRN == patient_mrn),]
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
    req(new_neg_df())
    df <- new_neg_df()
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