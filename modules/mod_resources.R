resources_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      box(width = 8, title = "Lexicons",
          p("These are the lexicons used in the narratives analyses. They are standard lexicons in NLP literature.
Obviously, the language used in the narratives is particular to HIV/AIDS, so standard lexicons may not always agree with usage (i.e. \"suppression\" being a positive term in PEPFAR).
Here you can edit entries to improve the results of the analyses. If you disagree with an entry, feel free to delete it from the lexicon.
If you find an entry incorrectly valued, feel free to revalue it. Any changes made will be saved and be made persistently available to the application."),
          # p("If you would like to make edits to the Google Sheet directly:"),
          # tags$a(href="https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit#gid=1057635978", "Lexicons")
      ),
      box(width = 4, selectInput(NS(id,"resource_list"), "Choose Resource",
                                 choices = c("Bing", "Stop Words", "Covid Words")))
    ),
    fluidRow(
      box(width = 12, DT::dataTableOutput(NS(id,"resourcedt")))
    )
  )
}


resources_server <- function(id, bing, stopwords, covidwords){
  
  moduleServer(id, function(input, output, session){
    
    resource_label <- reactive({input$resource_list})
    
    output$resourcedt <- DT::renderDataTable({
      
      if(resource_label() == "Bing") {
        
        DT::datatable(bing, options = list(pageLength = 25))
        
      } else if(resource_label() == "Stop Words"){
        
        DT::datatable(stopwords, options = list(pageLength = 25))
        
      } else {
        
        DT::datatable(covidwords, options = list(pageLength = 25))
        
      }
      
    })
    
  })
}