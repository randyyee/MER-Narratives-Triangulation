dashboard_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    tags$style(type="text/css", "body { overflow-y: scroll; }"),
    
    ui_updater_ui(ns("ui")),
    
    fluidRow(width             = 12,
             infoBox(width     = 3,
                     title     = "Overall Sentiment",
                     textOutput(ns("sentimentscore")),
                     icon      = icon("comments"),
                     fill      = T
             ),
             infoBox(width     = 3,
                     title     = "Most Positive",
                     div(style ="font-size:10px;",
                         uiOutput(ns("max_overall"))
                     ),
                     icon      = icon("thumbs-up"),
                     color     = "green",
                     fill      = T
             ),
             infoBox(width     = 3,
                     title     = "Most Negative",
                     div(style = "font-size:10px;",
                         uiOutput(ns("min_overall"))
                     ),
                     icon     = icon("thumbs-down"),
                     color    = "red",
                     fill     = T
             ),
             infoBox(width    = 3,
                     title    = "Trending",
                     textOutput(ns("trending")),
                     icon     = icon("arrow-up"),
                     color    = "yellow",
                     fill     = T
             )
    ),
    fluidRow(width            = 12,
             box(width        = 6,
                 title        = "Sentiment Score",
                 status       = "primary",
                 solidHeader  = T,
                 plotOutput(ns("sentimentplot"))
             ),
             box(width       = 6,
                 title       = "Top 20 Sentiment Contributions",
                 status      = "primary",
                 solidHeader = T,
                 plotOutput(ns("contribution"))
             )
    ),
    fluidRow(width           = 12,
             box(width       = 6,
                 title       = "Bigram Network",
                 status      = "primary",
                 solidHeader = T,
                 plotOutput(ns("bigramviz"))
             ),
             box(width       = 6,
                 title       = "Wordcloud",
                 status      = "primary",
                 solidHeader = T,
                 wordcloud2Output(ns("wordcloudy"))
             )
    ),
    fluidRow(width           = 12,
             tabBox(width    = 12,
                    tabPanel("Narratives", DT::dataTableOutput(ns("narrativesdt"))),
                    tabPanel("Sentiments",     DT::dataTableOutput(ns("prepared_sent_table")))
             )
    )
  )
  
}



dashboard_server <- function(id, 
                             nm_df,
                             n_df,
                             bing, stopwords, negationwords
){
  
  moduleServer(id, function(input, output, session){
    
    # Sub-Modules ----
    # UI Outputs (1=sub-narratives;2=sub-bigrams;3=sub-sentiments)
    ui_info        <- ui_updater_server("ui", nm_df, n_df, bing, stopwords, negationwords)
    
    # /////////////////////////////////////////////////////////////////////////////////////////    
    # Narratives Subset ----
    output$narrativesdt <- DT::renderDataTable(server = FALSE,{
      
      DT::datatable(
        ui_info[[1]]() %>%
          unite("Info", `Operating Unit`:Period, sep = ";"),
        
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
    
    # /////////////////////////////////////////////////////////////////////////////////////////
    # Sentiments ----
    
    ## Sentiments Subset Table ----
    output$prepared_sent_table <- DT::renderDataTable({
      
      DT::datatable(
        ui_info[[3]](),
        
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
    
    summaries <- reactive({ sent_summary(ui_info[[3]]()) })
    
    ## Infoboxes ----
    output$sentimentscore <- renderText({
      
      format(as.numeric(sum(ui_info[[3]]()$sentiment, na.rm = T)), big.mark=",")
      
    })
    
    output$min_overall <- renderUI({
      
      summaries()[[1]]
      
    })
    
    output$max_overall <- renderUI({
      
      summaries()[[2]]
      
    })
    
    output$trending <- renderText({   
      
      str_to_title((ui_info[[3]]() %>%
                      count(ngram, sort = T) %>%
                      top_n(1))$ngram[[1]])
    })
    
    ## Plots ----
    output$sentimentplot <- renderPlot({
      
      if(length(unique(ui_info[[3]]()$`Indicator Bundle`)) == 1) {
        
        ggplot(ui_info[[3]]() %>%
                 group_by(`Operating Unit`, `Indicator Bundle`, `Indicator`) %>%
                 summarise(sentiment = sum(sentiment, na.rm = T)) %>%
                 ungroup(), aes(`Indicator`, sentiment, fill = `Indicator`)) +
          geom_col(show.legend = T, na.rm = T) +
          facet_wrap(~`Operating Unit`, scales = "free_x") +
          theme_linedraw() +
          theme(axis.title.x       = element_blank(),
                axis.title.y       = element_blank(),
                axis.text.x        = element_blank(),
                axis.ticks.x       = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.background   = element_blank(),
                plot.background    = element_blank(),
                legend.background  = element_blank(),
                legend.title       = element_blank())
        
      } else {
        
        ggplot(ui_info[[3]]() %>%
                 group_by(`Operating Unit`, `Indicator Bundle`) %>%
                 summarise(sentiment = sum(sentiment,na.rm = T)) %>%
                 ungroup(), aes(`Indicator Bundle`, sentiment, fill = `Indicator Bundle`)) +
          geom_col(show.legend = T, na.rm = T) +
          facet_wrap(~`Operating Unit`, scales = "free_x") +
          theme_linedraw() +
          theme(axis.title.x       = element_blank(),
                axis.title.y       = element_blank(),
                axis.text.x        = element_blank(),
                axis.ticks.x       = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.background   = element_blank(),
                plot.background    = element_blank(),
                legend.background  = element_blank(),
                legend.title       = element_blank())
      }
      
    }, bg = "transparent")
    
    ## Sentiment Contributes ----
    prepared_sent_c <- reactive({ 
      ui_info[[2]]() %>% 
        prepare_sent_contributes()
    })
    
    output$contribution <- renderPlot({
      
      ggplot(prepared_sent_c(), aes(ngram, n, fill = sentiment), environment = environment()) +
        geom_col() +
        coord_flip() +
        scale_x_discrete(guide=guide_axis(n.dodge=2)) +
        theme_linedraw() +
        theme(axis.title.x       = element_blank(),
              axis.title.y       = element_blank(),
              axis.text.x        = element_blank(),
              axis.ticks.x       = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.background   = element_blank(),
              plot.background    = element_blank(),
              legend.background  = element_blank(),
              legend.title       = element_blank(),
              legend.position    = "top")
      
      
    }, bg = "transparent")
    
    # /////////////////////////////////////////////////////////////////////////////////////////
    # Bigrams ----
    
    output$bigramviz <- renderPlot({
      
      ui_info[[2]]() %>%
        count(word1, word2, sort = T) %>%
        top_n(50) %>%
        visualize_bigrams()
      
    }, bg = "transparent")
    
    
    # /////////////////////////////////////////////////////////////////////////////////////////
    # TF_IDF ----
    
    # output$tfidf_df <- DT::renderDataTable({
    #   DT::datatable(
    #     ui_info[[2]]() %>%
    #       count(Name, ngram, sort = T) %>%
    #       bind_tf_idf(ngram, Name, n) %>%
    #       arrange(desc(tf_idf)) %>%
    #       mutate(tf     = round(tf,     2),
    #              idf    = round(idf,    2),
    #              tf_idf = round(tf_idf, 2)) %>%
    #       separate(Name, c("Operating Unit", "Indicator Bundle", "Indicator"), sep = "\\|"),
    #     rownames=FALSE,
    #     filter="top",
    #     options = list(
    #       searchHighlight = TRUE,
    #       scroller = TRUE,
    #       scrollX = TRUE,
    #       scrollY = 700)
    #   )
    #   
    # })
    
    # /////////////////////////////////////////////////////////////////////////////////////////
    # Wordcloud ----
    
    output$wordcloudy <- renderWordcloud2({
      
      wordcloud2a(
        ui_info[[2]]() %>%
          count(ngram, sort = T) %>%
          rename("word" = "ngram") %>%
          rename("freq" = "n"),
        color = "black",
        backgroundColor = "transparent"
      )
      
    })
    
  })
  
  
}


