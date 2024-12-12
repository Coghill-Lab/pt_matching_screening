
version_id <- paste0("v0.0.9")


# Back end input ---------------------------------------------------------------

Confirmed_Known_HIVNEG_File <- ""

Existing_Known_HIVNEG_File <- ""

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
                                        
                                      ),
                                      shiny::mainPanel(
                                        
                                      )
                                    )
                    ),
                    ## Matching ------------------------------------------------
                    shiny::tabPanel("Matching",
                                    shiny::sidebarLayout(
                                      shiny::sidebarPanel(
                                        width = 3,
                                        
                                      ),
                                      shiny::mainPanel(
                                        
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
  backend_hivneg_file <- reactiveVal(NULL)
  backend_known_hivneg_file <- reactiveVal(NULL)
  
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
  
  ## Homepage ------------------------------------------------------------------
  
  
  ## Screening -----------------------------------------------------------------
  
  
  ## Matching ------------------------------------------------------------------
  
  
  ## Recognition ---------------------------------------------------------------
  
  
}














shinyApp(ui = ui, server = server)