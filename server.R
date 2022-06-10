#### Server ####
server <- function(input, output, session) {
  
  # which users should have access to data? IN THIS CASE ONLY PARTNERS - USG FOLKS SEE EVERYTHING, PARTNERS LIMITED TO MECH ACCESS
  USG_USERS     = c("Agency", "Interagency", "Global Agency", "Global")
  PARTNER_USERS = c("Global Partner", "Partner")
  
  # user information
  user_input  <-  reactiveValues(authenticated = F, # Switch for testing
                                 status = "",
                                 d2_session = NULL)
  
  # is the user authenticated?
  output$ui <- renderUI({
    if(user_input$authenticated == FALSE) {
      uiOutput("uiLogin")
    } else {
      uiOutput("authenticated")
    }
  })
  
  # login page with username and password
  output$uiLogin  <-  renderUI({
    wellPanel(fluidRow(img(src = "pepfar.png", align = "center"), h4("Welcome to the MER-Narratives App. Please login with your DATIM credentials:")),
              fluidRow(textInput("user_name", "Username: ", width = "600px"),
                       passwordInput("password", "Password:",  width = "600px"),
                       actionButton("login_button", "Log in!")))
  })
  
  # once you login this page shows up
  output$authenticated <- renderUI({
    
    #### Server Modules ####
    dashboard_server(    "d", narratives_meta, narratives_df, bing, stopwords, negationwords)
    impact_server(       "i", narratives_meta, narratives_df, bing, stopwords, negationwords)
    resources_server(    "r", bing, stopwords, covidwords)
    triangulation_server("t", msd_df, narratives_meta, narratives_df, bing, stopwords, negationwords)
    
    #### UI: Sidebar ####
    sidebar <- dashboardSidebar(sidebarMenu(
      menuItem("Home",              tabName = "home",        icon = icon("home")),
      menuItem("Dashboard",         tabName = "analyses",    icon = icon("chart-line")),
      menuItem("Ngrams Explorer",   tabName = "impacttable", icon = icon("compass")),
      menuItem("MER Triangulation", tabName = "triangulate", icon = icon("puzzle-piece")),
      menuItem("Resources",         tabName = "resources",   icon = icon("book"))
    ),
    busyIndicator()
    )
    
    #### UI: Body ####
    body <- dashboardBody(shinyDashboardThemes(theme = "poor_mans_flatly"),
                          
                          tabItems(tabItem(tabName = "home",        home_ui()),
                                   tabItem(tabName = "analyses",    dashboard_ui("d")),
                                   tabItem(tabName = "impacttable", impact_ui("i")),
                                   tabItem(tabName = "triangulate", triangulation_ui("t")),
                                   tabItem(tabName = "resources",   resources_ui("r"))
                          )
    )
    
    dashboardPage(title = "Narrator", dashboardHeader(title = "Narratives Application"),
                  sidebar, body)
  })
  
  # User and mechanisms reactive value pulled only once ----
  user       <- reactiveValues(type = NULL)
  mechanisms <- reactiveValues(my_cat_ops = NULL)
  userGroups <- reactiveValues(streams = NULL)
  
  # Login process ----
  observeEvent(input$login_button, {
    tryCatch({
      datimutils::loginToDATIM(base_url = Sys.getenv("BASE_URL"),
                               username = input$user_name,
                               password = input$password,
                               d2_session_envir = parent.env(environment())
      )
      
      # DISALLOW USER ACCESS TO THE APP-----
      
      # store data so call is made only once
      userGroups$streams <-  datimutils::getMyStreams()
      user$type <- datimutils::getMyUserType()
      mechanisms$my_cat_ops <- datimutils::listMechs()
      
      # if a user is not to be allowed deny them entry
      if (!user$type %in% USG_USERS) {
        
        # alert the user they cannot access the app
        sendSweetAlert(
          session,
          title = "YOU CANNOT LOG IN",
          text = "You are not authorized to use this application",
          type = "error"
        )
        
        # log them out
        Sys.sleep(3)
        flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
        user_input$authenticated  <-  FALSE
        user_input$user_name <- ""
        user_input$authorized  <-  FALSE
        user_input$d2_session  <-  NULL
        d2_default_session <- NULL
        gc()
        session$reload()
        
      }
    },
    # This function throws an error if the login is not successful
    error = function(e) {
      flog.info(paste0("User ", input$user_name, " login failed."), name = "datapack")
    }
    )
    
    if (exists("d2_default_session")) {
      if (any(class(d2_default_session) == "d2Session")) {
        user_input$authenticated  <- TRUE
        user_input$d2_session  <-  d2_default_session$clone()
        d2_default_session <- NULL
        
      }
    } else {
      sendSweetAlert(
        session,
        title = "Login failed",
        text = "Please check your username/password!",
        type = "error"
      )
    }
  })
  
}