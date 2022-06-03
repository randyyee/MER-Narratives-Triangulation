triangulation_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(width = 12,
            fluidRow(width = 12,
                     ui_updater_ui(ns("ui_a"))),
            fluidRow(width = 12,
                     box(width = 3, 
                         title = "Summary",
                         textOutput(ns("title"))),
                     box(width = 9,
                         title = "Narrative",
                         textOutput(ns("content")))
            ),
            
            fluidRow(width = 12,
                     box(width = 12, 
                         title = "Instructions",
                         p("Click on 1 row in the NARRATIVES tab in order to display results in the MSD tab."))
            ),
            
            tabBox(width = 12,
                   tabPanel("Narratives",
                            dataTableOutput(ns("narrativesdt"))),
                   tabPanel("Pivot Table",
                            div(style = 'overflow-y: scroll',
                                rpivotTableOutput(ns("filter_msd_df"))
                            )),
                   tabPanel("MSD",
                            dataTableOutput(ns("mdt")))
            )
  )
}


triangulation_server <- function(id, 
                                 msd_df,
                                 nm_df,
                                 n_df,
                                 bing, stopwords, negationwords){
  
  moduleServer(id, function(input, output, session){
    
    # Sub-Modules ----
    # UI Outputs (1=sub-narratives;2=sub-bigrams;3=sub-sentiments)
    ui_info        <- ui_updater_server("ui_a", nm_df, n_df,
                                        bing, stopwords, negationwords)
    
    # Sub-Module Outputs
    output$narrativesdt <- DT::renderDataTable({
      DT::datatable(ui_info[[1]]()[,c(1,5,6,9,11)],
                    selection = "single",
                    rownames=FALSE,
                    filter="top",
                    options = list(
                      searchHighlight = TRUE,
                      scroller = TRUE,
                      scrollX = TRUE,
                      scrollY = 700
                      
                    )
      )
    })
    
    # User selection through datatable
    filteredTable_selected <- reactive({
      ids <- input$narrativesdt_rows_selected
      ui_info[[1]]()[ids,]
    })
    
    row_count          <- reactive({input$narrativesdt_rows_selected}) 
    operatingunit_name <- reactive({req(row_count()) 
      filteredTable_selected()[[1, 1]]}) 
    indicator_name     <- reactive({req(row_count()) 
      filteredTable_selected()[[1, 5]]})
    im_name            <- reactive({req(row_count()) 
      filteredTable_selected()[[1, 9]]})
    support_name       <- reactive({req(row_count()) 
      filteredTable_selected()[[1, 6]]})
    narratives_content <- reactive({req(row_count()) 
      filteredTable_selected()[[1, 11]]})
    
    # Narratives summary after user selection
    output$title <- renderText({
      paste(row_count(), operatingunit_name(), indicator_name(), support_name(), im_name(), sep = "; ")
    })
    
    output$content <- renderText({
      req(row_count())
      narratives_content()
    })
    
    # MSD filter after selection through datatable
    output$filter_msd_df <- renderRpivotTable({
      req(row_count())
      rpivotTable(msd_df %>%
                    filter(operatingunit == operatingunit_name()) %>%
                    filter(indicator     == indicator_name()) %>%
                    #filter(grepl(im_name(), mech_name)) %>%
                    filter(indicatortype == support_name()) %>%
                    select(-contains("uid")),
                  rows=c("period", "mech_name"),
                  cols=c("operatingunit", "country", "standardizeddisaggregate"),
                  vals = "value", aggregatorName = "Integer Sum"
      )
      
    })
    
    output$mdt <- DT::renderDataTable({
      DT::datatable(msd_df,
                    selection = "single",
                    rownames=FALSE,
                    filter="top",
                    options = list(
                      searchHighlight = TRUE,
                      scroller = TRUE,
                      scrollX = TRUE,
                      scrollY = 700
                      
                    )
      )
    })
    
  })
  
}
