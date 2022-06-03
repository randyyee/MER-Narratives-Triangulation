impact_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    ui_updater_ui(ns("ui_b")),
    fluidRow(
      tabBox(width = 12,
             tabPanel("Ngrams", dataTableOutput(ns("narratives_ngrams"))),
             tabPanel("Sentences", dataTableOutput(ns("narratives_sentences")))
      )
    )
  )
}


impact_server <- function(id, 
                          nm_df,
                          n_df,
                          bing, stopwords, negationwords){
  
  moduleServer(id, function(input, output, session){
    
    # Sub-Modules ----
    # UI Outputs (1=sub-narratives;2=sub-bigrams;3=sub-sentiments)
    ui_info        <- ui_updater_server("ui_b", nm_df, n_df, bing, stopwords, negationwords)
    
    output$narratives_ngrams <- renderDataTable({
      
      DT::datatable(
        ui_info[[2]]()[2:12],
        
        rownames = FALSE,
        filter   = "top",
        extensions = c("Buttons"),
        options = list(
          scrollX = T,
          dom = 'lBfrtip',
          buttons = list(
            list(extend = "csv", text = "Download Full Results", filename = "data",
                 exportOptions = list(
                   modifier = list(page = "all")
                 )
            )
          )
        )
      )
      
    })
    
    output$narratives_sentences <- renderDataTable({
      
      DT::datatable(
        ui_info[[4]](),
        
        rownames = FALSE,
        filter   = "top",
        extensions = c("Buttons"),
        options = list(
          dom = 'lBfrtip',
          buttons = list(
            list(extend = "csv", text = "Download Full Results", filename = "data",
                 exportOptions = list(
                   modifier = list(page = "all")
                 )
            )
          )
        )
      )
      
    })
    
  })
  
}
