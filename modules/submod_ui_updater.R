# Dashboard SubModule: Narratives UI Updater

ui_updater_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(width = 12,
             box(width = 3, uiOutput(ns("ous"))),
             box(width = 3, uiOutput(ns("ars"))),
             box(width = 3, uiOutput(ns("inds"))),
             box(width = 3, uiOutput(ns("pers")))
    ),
    fluidRow(width = 12,
             box(width = 6, textInput(ns("term"), "Sentence Filter (filters sentences for word(s))", "")),
             box(width = 3, numericInput(ns("ngram"), "Ngram", value=2, min = 1, max = NA, step = 1))
             
    )
  )
  
}


ui_updater_server <- function(id, nm_df, n_df,
                              bing, stopwords, negationwords){
  
  moduleServer(id, function(input, output, session){
    
    # Get term and ngram input
    term       <- reactive({input$term})
    ngram_num  <- reactive({input$ngram})
    
    # Get slicer inputs
    ou_label   <- reactive({input$ou_list})
    area_label <- reactive({input$area_list})
    ind_label  <- reactive({input$indicator_list})
    per_label  <- reactive({input$period_list})
    
    # Update slicer choices depending on other slicers
    output$ous <- renderUI({
      selectInput (session$ns("ou_list"), 
                   label   = "Operating Unit", 
                   choices = append(sort(unique(nm_df$`Operating Unit`)), "All"))
    })
    
    output$ars <- renderUI({
      shiny::validate(
        need(length(input$ou_list) > 0, "Calculating...")
      )
      selectInput (session$ns("area_list"), 
                   label   = "Program Area", 
                   choices = append("All", sort(unique(filter(nm_df,if(input$ou_list != "All") (`Operating Unit` == input$ou_list) else TRUE)$`Indicator Bundle`))))
    })
    
    output$inds <- renderUI({
      shiny::validate(
        need(length(input$ou_list) > 0, "Calculating..."),
        need(length(input$area_list) > 0, "Calculating...")
      )
      selectInput (session$ns("indicator_list"), 
                   label   = "Indicator", 
                   choices = append("All", sort(unique((nm_df %>% 
                                                          filter(if(input$ou_list   != "All") (`Operating Unit` == input$ou_list)     else TRUE) %>%
                                                          filter(if(input$area_list != "All") (`Indicator Bundle` == input$area_list) else TRUE))$`Indicator`))))
    })
    
    output$pers <- renderUI({
      selectInput (session$ns("period_list"), 
                   label   = "Period", 
                   choices = append(sort(unique(filter(nm_df,!str_detect(`Period`, "Target"))$`Period`), decreasing = T), "All"))
    })
    
    
    # Return filtered narratives
    narratives_sub <- reactive({
      shiny::validate(
        need(length(input$ou_list) > 0, "Calculating..."),
        need(length(input$area_list) > 0, "Calculating..."),
        need(length(input$indicator_list) > 0, "Calculating..."),
        need(length(input$period_list) > 0, "Calculating...")
      )
      n_df %>%
        filter(if(input$ou_list        != "All") (`Operating Unit`   == input$ou_list)         else TRUE) %>%
        filter(if(input$area_list      != "All") (`Indicator Bundle` == input$area_list)       else TRUE) %>%
        filter(if(input$indicator_list != "All") (`Indicator`        == input$indicator_list)  else TRUE) %>%
        filter(if(input$period_list    != "All") (`Period`           == input$period_list)     else TRUE) %>%
        filter(str_detect(Narrative, term()))
    })
    
    # Return filtered bigrams
    bigrams_sub <- reactive({
      # shiny::validate(
      #   need(length(input$ou_list) > 0, "Calculating..."),
      #   need(length(input$area_list) > 0, "Calculating..."),
      #   need(length(input$indicator_list) > 0, "Calculating..."),
      #   need(length(input$period_list) > 0, "Calculating...")
      # )
      # b_df %>%
      narratives_sub() %>%
        prepare_bigrams(., bing, stopwords, negationwords, ngram_num())
    })
    
    # Return filtered sentiments
    sentiments_sub <- reactive({
      bigrams_sub() %>%
        prepare_sentiments()
    })
    
    # Return filtered sentences
    sentences_sub <- reactive({
      narratives_sub() %>%
        unnest_tokens(sentence, Narrative, token = "sentences") %>%
        filter(str_detect(sentence, term())) %>%
        group_by_at(vars(-sentence)) %>%
        summarise(Sentences = paste0(sentence, collapse = "[...]")) %>%
        ungroup()
    })
    
    return(    
      list(
        narratives_sub,
        bigrams_sub,
        sentiments_sub,
        sentences_sub
      ))
    
  })
  
}
