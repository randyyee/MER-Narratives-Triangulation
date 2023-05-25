# ShinyICPI: Narratives Explorer (SHINE) 
library(shiny)
library(shinydashboard)
library(tidyverse)
library(aws.s3)
library(paws)
library(tidytext)
library(wordcloud2)
library(ggraph)
library(igraph)
library(shinycustomloader)
library(shinyBS)
library(datimutils)
library(rpivotTable)
library(DT)
library(reactable)
library(scales)
library(gdata)
library(bslib)
library(futile.logger)
library(shinyWidgets)
library(datimutils)
library(httr)
library(magrittr)
library(xml2)

source("./functions/sentiments_management.R")
source("./functions/bigrams_management.R")
#source("global.R")

# /////////////////////////////////////////////////////////////////////////////////////////

# Login ----
## Enter Button ----
jscode_login <- '$(document).keyup(function(e) {
    var focusedElement = document.activeElement.id;
    console.log(focusedElement);
    if (e.key == "Enter" && focusedElement == "user_name") {
    $("#password").focus();
    } else if (e.key == "Enter" && focusedElement == "password") {
    $("#login_button_oauth").click();
    }
});'

## Initiate Logging ----
logger <- flog.logger()
flog.appender(appender.console(), name = "usgpartners")

#Removed ifelse
APP_URL <- Sys.getenv("APP_URL") #This will be your shiny server path


oauth_app <- httr::oauth_app(Sys.getenv("OAUTH_APPNAME"),
                             key = Sys.getenv("OAUTH_KEYNAME"), # dhis2 = Client ID
                             secret = Sys.getenv("OAUTH_SECRET"), #dhis2 = Client Secret
                             redirect_uri = APP_URL
)

oauth_api <- httr::oauth_endpoint(base_url = paste0(Sys.getenv("BASE_URL"), "uaa/oauth"),
                                  request = NULL,
                                  authorize = "authorize",
                                  access = "token"
)

oauth_scope <- "ALL"

has_auth_code <- function(params) {
  
  return(!is.null(params$code))
}

# which users should have access to data? IN THIS CASE ONLY PARTNERS - USG FOLKS SEE EVERYTHING, PARTNERS LIMITED TO MECH ACCESS
USG_USERS = c("Agency", "Interagency", "Global Agency", "Global")
PARTNER_USERS = c("Global Partner", "Partner")

# /////////////////////////////////////////////////////////////////////////////////////////
# Server ----
server <- function(input, output, session) {
  
  # Login Logic ----
  ready <- reactiveValues(ok = FALSE)
  
  # User Information ----
  user_input  <-  reactiveValues(authenticated = FALSE,
                                 status = "",
                                 d2_session = NULL,
                                 memo_authorized = FALSE)
  
  # Logout Process ----
  observeEvent(input$logout, {
    req(input$logout)
    # Returns to the log in screen without the authorization code at top
    updateQueryString("?", mode = "replace", session = session)
    flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
    ready$ok <- FALSE
    user_input$authenticated  <-  FALSE
    user_input$user_name <- ""
    user_input$authorized  <-  FALSE
    user_input$d2_session  <-  NULL
    d2_default_session <- NULL
    gc()
    session$reload()
  })
  
  # User Authentication Check ----
  output$ui <- renderUI({
    if(user_input$authenticated == FALSE && !interactive()) { #Added to address OAuth
      uiOutput("uiLogin")
    } else {
      uiOutput("authenticated")
    }
  })
  
  # Login Page ----
  output$uiLogin  <-  renderUI({
    
    fluidPage(
      wellPanel(
        fluidRow(
          h4("Please login with your DATIM credentials:"),
          br()
        ),
        fluidRow(
          textInput("user_name", "Username: ", width = "500px"),
          passwordInput("password", "Password:", width = "500px"),
          actionButton("login_button", "Log in!"),
          
          actionButton("login_button_oauth", "Log in with DATIM"),
          uiOutput("ui_hasauth"),
          uiOutput("ui_redirect")
        )
      )
    )
    
  })
  ########### AFTER AUTHENTICATION ############################################## 
  # Dashboard Page (RY) ----
  output$authenticated <- renderUI({ 
    navbarPage(theme = bs_theme(version = 5, bootswatch = "yeti"),
               title = "ShinyICPI: Narratives Explorer (ShINE)",
               tabPanel("Dashboard",
                        sidebarLayout(
                          sidebarPanel(width=2,
                                       HTML('<script> document.title = "SHiNE"; </script>'),
                                       selectInput("ou_list",        label = "Operating Unit", choices = "All"),
                                       selectInput("country_list",   label = "Country",        choices = "All"),
                                       selectInput("period_list",    label = "Period",         choices = c("2023 Q1", "2023 Q2", "2023 Q3", "2023 Q4"),
                                                   selected = "2023 Q1"),
                                       selectInput("area_list",      label = "Program Area",   choices = "All"),
                                       selectInput("indicator_list", label = "Indicator",      choices = "All"),
                                       selectInput("agency_list",    label = "Agency",         choices = "All"),
                                       selectInput("partner_list",   label = "Partner",        choices = "All", selectize = TRUE),
                                       hr(),
                                       div(style="text-align:center; font-style: italic;", "This filter inputs into stringr's 'str_detect' function.
                        The default interpretation is a regular expression. 
                        Use regex() for finer control of the matching behaviour."),
                                       textInput("term", "Narratives Filter", ""),
                                       downloadButton("downloadData", "Download All Narratives", style = "color:black"),
                                       downloadButton("downloadData2", "Download Prepped Narratives", style = "color:black"),
                                       hr(),
                                       p("Contact:"),
                                       tags$a(href="mailto:pcx5@cdc.gov", "Randy Yee (pcx5@cdc.gov)")
                          ),
                          
                          mainPanel(width=10,
                                    
                                    tags$head(tags$style(HTML("
                                .btn {text-align: left; width: 100%;}
                                .card-header {height: 50px;}
                                .card-body {background-color: white;}
                                .rt-th {font-weight: normal;}
                                               "))),
                                    
                                    fluidRow(
                                      column(12,
                                             card(full_screen = TRUE,
                                                  card_header("Indicator Trends"),
                                                  card_body_fill(plotOutput("trends"))
                                             )
                                      )
                                    ),
                                    
                                    fluidRow(
                                      column(4,
                                             navs_tab_card(full_screen = TRUE,
                                                           nav("Sentiments", 
                                                               card_body_fill(plotOutput("sentimentplot"))
                                                           ),
                                                           nav("Top 10 Contributions",
                                                               card_body_fill(plotOutput("contribution"))
                                                           )
                                             )
                                      ),
                                      column(4,
                                             card(full_screen = TRUE,
                                                  card_header("Bigram Network"),
                                                  card_body_fill(plotOutput("bigramviz"))
                                             )
                                      ),
                                      column(4,
                                             card(full_screen = TRUE,
                                                  card_header("Wordcloud"),
                                                  card_body_fill(wordcloud2Output("wordcloudy"))
                                             )
                                      )),
                                    tabBox(width = 12,
                                           tabPanel("Summary", reactableOutput("mertable")),
                                           tabPanel("Raw Data: Narratives", dataTableOutput("narrativesdt")),
                                           tabPanel("Raw Data: MSD Pivot", div(style = 'overflow-y: scroll',
                                                                               p("Click on a row in the Narratives tab in order to filter results."),
                                                                               rpivotTableOutput("filter_msd_df"))),
                                           tabPanel("Resources: Bigrams", dataTableOutput("narratives_ngrams")),
                                           tabPanel("Resources: Sentences", dataTableOutput("narratives_sentences")))
                                    
                          )
                        )
               )
    )
  })
  
  # Redirected to OAuth login Agent ----
  output$ui_redirect <- renderUI({
    #print(input$login_button_oauth) useful for debugging
    if (!is.null(input$login_button_oauth)) { # nolint
      if (input$login_button_oauth > 0) { # nolint
        url <- httr::oauth2.0_authorize_url(oauth_api, oauth_app, scope = oauth_scope)
        redirect <- sprintf("location.replace(\"%s\");", url)
        tags$script(HTML(redirect))
      } else NULL
    } else NULL
  })
  
  # User and Mech Reactive Value Pulled Only Once ----
  user <- reactiveValues(type = NULL)
  mechanisms <- reactiveValues(my_cat_ops = NULL)
  userGroups <- reactiveValues(streams = NULL)
  
  # Login Button OAuth Checks ----
  observeEvent(input$login_button_oauth > 0, {
    
    # Grabs the code from the url
    params <- parseQueryString(session$clientData$url_search)
    # Wait until the auth code actually exists
    req(has_auth_code(params))
    
    # Manually create a token
    token <- httr::oauth2.0_token(
      app = oauth_app,
      endpoint = oauth_api,
      scope = oauth_scope,
      use_basic_auth = TRUE,
      oob_value = APP_URL,
      cache = FALSE,
      credentials = httr::oauth2.0_access_token(endpoint = oauth_api,
                                                app = oauth_app,
                                                code = params$code,
                                                use_basic_auth = TRUE)
    )
    
    loginAttempt <- tryCatch({
      user_input$uuid <- uuid::UUIDgenerate()
      datimutils::loginToDATIMOAuth(base_url =  Sys.getenv("BASE_URL"),
                                    token = token,
                                    app = oauth_app,
                                    api = oauth_api,
                                    redirect_uri = APP_URL,
                                    scope = oauth_scope,
                                    d2_session_envir = parent.env(environment())
      )
      
      # Disallow User into App ----
      
      # store data so call is made only once
      userGroups$streams <-  datimutils::getMyStreams()
      user$type <- datimutils::getMyUserType()
      mechanisms$my_cat_ops <- datimutils::listMechs()
      
      # if a user is not to be allowed deny them entry
      if (!user$type %in% c(USG_USERS, PARTNER_USERS)) {
        
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
    # This function throws an error if the login is not successful ----
    error = function(e) {
      flog.info(paste0("User ", input$user_name, " login failed. ", e$message), name = "usgpartners")
    }
    )
    
    if (exists("d2_default_session")) {
      
      user_input$authenticated  <-  TRUE
      user_input$d2_session  <-  d2_default_session$clone()
      d2_default_session <- NULL
      
      source("dataimport.R")
      
      
      # Need to check the user is a member of the PRIME Data Systems Group, COP Memo group, or a super user
      # user_input$memo_authorized <-
      #   grepl("VDEqY8YeCEk|ezh8nmc4JbX", user_input$d2_session$me$userGroups) |
      #   grepl(
      #     "jtzbVV4ZmdP",
      #     user_input$d2_session$me$userCredentials$userRoles
      #   )
      # flog.info(
      #   paste0(
      #     "User ",
      #     user_input$d2_session$me$userCredentials$username,
      #     " logged in."
      #   ),
      #   name = "usgpartners"
      # )
      # 
      # flog.info(
      #   paste0(
      #     "User ",
      #     user_input$d2_session$me$userCredentials$username,
      #     " logged in."
      #   ),
      #   name = "usgpartners"
      # )
      
      # # show user information ----
      # observeEvent(input$me_button, {
      #   output$message <- renderPrint({ user$type })
      # })
      # 
      # # show streams ids data ----
      # observeEvent(input$groupid_button, {
      #   groups_id_df <- userGroups$streams
      #   
      #   # display streams
      #   output$message <- renderPrint({ groups_id_df })
      #   
      # })
      # 
      # # show mechs by cocuid ----
      # observeEvent(input$mech_cocuid_button, {
      #   
      #   # display mechanisms  
      #   output$table <- renderDataTable(mechanisms$my_cat_ops[,c("combo_id", "name")],
      #                                   options = list(
      #                                     pageLength = 10
      #                                   )
      #   )
      #   
      # })
      # 
      # # show mechs by mechs code----
      # observeEvent(input$mech_id_button, {
      #   
      #   # display mechanisms  
      #   output$table <- renderDataTable(mechanisms$my_cat_ops[,c("mech_code", "name")],
      #                                   options = list(
      #                                     pageLength = 10
      #                                   )
      #   )
      #   
      # })
      # 
      # # show mechs by name ----
      # observeEvent(input$mech_name_button, {
      #   
      #   # display mechanisms 
      #   output$table <- renderDataTable(mechanisms$my_cat_ops[,c("name"), drop=FALSE],
      #                                   options = list(
      #                                     pageLength = 10
      #                                   )
      #   )
      #   
      #   
      # })
      
      # Dashboard Section (RY) ----
      ## Slicer Logic ----
      
      # Filter metadata
      narratives_meta2 <- reactive({
        narratives_meta %>%
          filter(if(input$ou_list        != "All") (`Operating Unit`              %in% input$ou_list) else TRUE) %>%
          filter(if(input$country_list   != "All") (`Country`                     %in% input$country_list) else TRUE) %>%
          filter(if(input$period_list    != "All") (`Period`                      %in% input$period_list) else TRUE) %>%
          filter(if(input$area_list      != "All") (`Indicator Bundle`            %in% input$area_list) else TRUE) %>%
          filter(if(input$indicator_list != "All") (`Indicator`                   %in% input$indicator_list) else TRUE) %>%
          filter(if(input$agency_list    != "All") (`Funding Agency`              %in% input$agency_list) else TRUE) %>%
          filter(if(input$partner_list   != "All") (`Implementing Mechanism Name` %in% input$partner_list) else TRUE)
      })
      
      # Get slicer inputs
      ou_label      <- reactive({c(input$ou_list)})
      country_label <- reactive({c(input$country_list)})
      per_label     <- reactive({c(input$period_list)})
      area_label    <- reactive({c(input$area_list)})
      ind_label     <- reactive({c(input$indicator_list)})
      per_label     <- reactive({c(input$period_list)})
      agc_label     <- reactive({c(input$agency_list)})
      par_label     <- reactive({c(input$partner_list)})
      
      # Update slicer inputs
      observe({
        updateSelectInput(session, "ou_list",        choices = append("All", sort(unique(narratives_meta2()$`Operating Unit`))),              selected = ou_label())
        updateSelectInput(session, "country_list",   choices = append("All", sort(unique(narratives_meta2()$`Country`))),                     selected = country_label())
        updateSelectInput(session, "period_list",    choices = sort(unique(narratives_meta$`Period`), decreasing = T),                        selected = per_label())    
        updateSelectInput(session, "area_list",      choices = append("All", sort(unique(narratives_meta2()$`Indicator Bundle`))),            selected = area_label())
        updateSelectInput(session, "indicator_list", choices = append("All", sort(unique(narratives_meta2()$`Indicator`))),                   selected = ind_label()) 
        updateSelectInput(session, "agency_list",    choices = append("All", sort(unique(narratives_meta2()$`Funding Agency`))),              selected = agc_label())
        updateSelectInput(session, "partner_list",   choices = append("All", sort(unique(narratives_meta2()$`Implementing Mechanism Name`))), selected = par_label())               
      })
      
      ## Data Subsets ----
      narratives_subdf <- reactive({
        narratives_df %>%
          filter(if(ou_label()      != "All") (`Operating Unit`             %in% ou_label())      else TRUE) %>%
          filter(if(country_label() != "All") (`Country`                    %in% country_label()) else TRUE) %>%
          filter(if(per_label()     != "All") (`Period`                     %in% per_label())     else TRUE) %>%
          filter(if(area_label()    != "All") (`Indicator Bundle`           %in% area_label())    else TRUE) %>%
          filter(if(ind_label()     != "All") (`Indicator`                  %in% ind_label())     else TRUE) %>%
          filter(if(agc_label()     != "All") (`Funding Agency`             %in% agc_label())     else TRUE) %>%
          filter(if(par_label()     != "All") (`Implementing Mechanism Name`%in% par_label())     else TRUE) %>%
          filter(if(term() != "")(str_detect(Narrative, term())) else TRUE)
      })
      
      narratives_prepdf <- reactive({
        narratives_subdf() %>%
          prepare_bigrams(., bing, stopwords, negationwords, 2)
      })
      
      # Subset MSD
      msd_subdf <- reactive({
        msd_df %>%
          filter(if(ou_label()      != "All") (`Operating Unit`%in% ou_label())      else TRUE) %>%
          filter(if(country_label() != "All") (`Country`       %in% country_label()) else TRUE) %>%
          filter(if(ind_label()     != "All") (`Indicator`     %in% ind_label())     else TRUE)
      })
      
      # /////////////////////////////////////////////////////////////////////////////////////////
      ## Trends ----
      output$trends <- renderPlot({
        df <- msd_subdf() %>%
          filter(Indicator %in% unique(narratives_meta2()$Indicator)) %>%
          filter(Standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
          filter(!Attribute %in% c("cumulative", "targets")) %>%
          group_by(Indicator, Numeratordenom, Period) %>%
          reframe(Value = sum(Value, na.rm=T)) %>%
          select(`Indicator`,
                 `Numeratordenom`,
                 `Period`,
                 `Value`) %>% 
          unite("Indicator", c(`Indicator`, `Numeratordenom`), sep="_") %>%
          mutate(Period = gsub(" ", "", str_sub(Period,3,-1)))
        
        p <- ggplot(df, aes(x = Period, y = Value, color = Indicator, group = 1)) +
          geom_line() +
          geom_point() +
          theme_linedraw() +
          theme(legend.position = "none",
                axis.text.x = element_text(angle = 75, hjust = 1),
                axis.title = element_blank(),
                panel.grid.minor = element_blank(),
                strip.text = element_text(face = "bold")) +
          scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
          facet_wrap( ~ Indicator, scales = "free_y") 
        #ggplotly(
        p
        #)
      })
      
      
      
      ## MSD Table ----
      output$mertable <- renderReactable({
        
        table <- narratives_subdf() %>%
          left_join(
            (
              msd_subdf() %>%
                filter(`Fiscal Year` == max(`Fiscal Year`)) %>%
                filter(Standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
                filter(Attribute %in% c("targets", "cumulative"))%>%
                group_by(`Operating Unit`, Country, Indicator, Numeratordenom, `Support Type`, `Fiscal Year`, Attribute) %>%
                summarise(Value = sum(Value, na.rm = T)) %>%
                ungroup() %>%
                pivot_wider(names_from = Attribute, values_from = Value) %>%
                rowwise() %>%
                mutate(cumulative = ifelse("cumulative" %in% names(.), cumulative, 0),
                       targets = ifelse("targets" %in% names(.), targets, 0)) %>%
                ungroup() %>%
                mutate(Achievement = percent(cumulative/targets, accuracy=1)) %>%
                select(`Operating Unit`,
                       `Country`,
                       `Fiscal Year`, 
                       Indicator,
                       Numeratordenom,
                       `Support Type`,
                       Cumulative = cumulative, 
                       Target = targets, 
                       Achievement)
            ), by=c("Operating Unit",
                    "Country",
                    "Indicator",
                    "Support Type"
            )
          ) %>%
          select(-c(`Org Level`, `Indicator Bundle`, `Fiscal Year`, `Funding Agency`)) %>%
          unite("Indicator", c(`Indicator`, `Numeratordenom`, `Support Type`), sep="-") %>%
          left_join(
            (
              msd_subdf() %>%
                filter(Standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
                filter(!Attribute %in% c("cumulative", "targets")) %>%
                unite("Indicator", c(`Indicator`, `Numeratordenom`, `Support Type`), sep="-") %>%
                group_by(`Operating Unit`, Country, `Mechanism Code`, Indicator) %>%
                mutate(Period = Period,
                       `Previous Qtr` = lag(Value, order_by=Period),
                       `Qtrly Percent Change` = percent((Value-`Previous Qtr`)/`Previous Qtr`, accuracy=1)) %>%
                ungroup() %>%
                filter(Period == max(Period)) %>%
                select(`Operating Unit`, 
                       `Country`, 
                       `Indicator`, 
                       `Mechanism Code`,
                       `Value`,
                       `Previous Qtr`,
                       `Qtrly Percent Change`)
            ), by=c("Operating Unit",
                    "Country",
                    "Indicator",
                    "Mechanism Code"
            )
          ) %>%
          unite("Mechanism", c(`Mechanism Code`, `Implementing Mechanism Name`), sep="-") %>%
          mutate(`Percent of Cumulative` = percent(Value/Cumulative, accuracy=1)) %>%
          unite("Cumulative vs Target", c(`Cumulative`, `Target`), sep=" vs. ") %>%
          select(-Period)
        
        reactable(table,
                  columns = list(
                    Achievement = colDef(format = colFormat(percent = TRUE, digits = 1)),
                    # `Qtrly Percent Change` = colDef(cell = function(value) {
                    #   if (is.na(value)){
                    #     paste(value, icon("horizontal-rule"))
                    #   } else if (value > 0) {
                    #     paste(value, icon("arrow-up"))
                    #   } else if (value < 0){
                    #     paste(value, icon("arrow-down"))
                    #   } else{
                    #     paste(value, icon("arrows-left-right"))
                    #     }
                    # }),
                    Narrative = colDef(minWidth = 500)
                  ),
                  groupBy = c("Operating Unit", "Country", "Indicator")
        )
      })
      
      ## Narrative Pivot ----
      output$narrativesdt <- DT::renderDataTable({
        DT::datatable(narratives_subdf()[,c(1,3,5,6,7,9,11)],
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
        narratives_subdf()[ids,]
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
      
      # MSD filter after selection through datatable
      observe({
        if(isTruthy(row_count())) {
          output$filter_msd_df <- renderRpivotTable({
            rpivotTable(msd_subdf() %>%
                          filter(`Operating Unit` == operatingunit_name()) %>%
                          filter(Indicator     == indicator_name()) %>%
                          filter(`Support Type` == support_name()) %>%
                          select(-contains("uid")),
                        rows=c("Period", "Mechanism Code"),
                        cols=c("Operating Unit", "Country", "Standardizeddisaggregate"),
                        vals = "Value", aggregatorName = "Integer Sum"
            )
          })
        } else {
          output$filter_msd_df <- renderRpivotTable({
            rpivotTable(msd_subdf() %>%
                          select(-contains("uid")),
                        rows=c("Period"),
                        cols=c("Operating Unit", "Country", "Standardizeddisaggregate"),
                        vals = "Value", aggregatorName = "Integer Sum")
          })
        }
      })
      
      # /////////////////////////////////////////////////////////////////////////////////////////
      ## Sentiments ----
      
      ## Summarize Sentiments ----
      sentiments_df <- reactive(
        narratives_prepdf() %>%
          prepare_sentiments()
      )
      
      ## Sentiment Plot ----
      output$sentimentplot <- renderPlot({
        if(area_label() != "All") {
          ggplot(sentiments_df() %>%
                   group_by(`Operating Unit`, `Indicator Bundle`, `Indicator`) %>%
                   summarise(sentiment = sum(sentiment, na.rm = T)) %>%
                   ungroup(), aes(`Indicator`, sentiment, fill = `Indicator`)) +
            geom_col(show.legend = T, na.rm = T) +
            facet_wrap(~`Operating Unit`, scales = "free_x") +
            theme_linedraw() +
            theme(strip.text         = element_text(face = "bold"),
                  axis.title.x       = element_blank(),
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
        } else {
          ggplot(sentiments_df() %>%
                   group_by(`Operating Unit`, `Indicator Bundle`) %>%
                   summarise(sentiment = sum(sentiment, na.rm = T)) %>%
                   ungroup(), aes(`Indicator Bundle`, sentiment, fill = `Indicator Bundle`)) +
            geom_col(show.legend = T, na.rm = T) +
            facet_wrap(~`Operating Unit`, scales = "free_x") +
            theme_linedraw() +
            theme(axis.title         = element_blank(),
                  axis.text.x        = element_blank(),
                  axis.ticks.x       = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.background   = element_blank(),
                  plot.background    = element_blank(),
                  legend.background  = element_blank(),
                  legend.title       = element_blank(),
                  legend.position    = "top")
        }
      }, bg = "transparent")
      
      ## Sentiment Contribution Plot ----
      output$contribution <- renderPlot({
        ggplot(narratives_prepdf() %>% 
                 prepare_sent_contributes(), aes(ngram, n, fill = sentiment), environment = environment()) +
          geom_col() +
          coord_flip() +
          scale_x_discrete(guide=guide_axis(n.dodge=2)) +
          theme_linedraw() +
          theme(axis.title         = element_blank(),
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
      ## Bigrams ----
      
      output$narratives_ngrams <- renderDataTable({
        DT::datatable(
          narratives_prepdf()[,c(1,7,8,9,10,14,15,16,17,18,19)],
          rownames = FALSE,
          filter   = "top",
          extensions = c("Buttons"),
          options = list(
            scrollX = T,
            dom = 'lBfrtip',
            buttons = list(
              list(extend = "csv", text = "Download Full Results", filename = "MERNarrativeBigrams",
                   exportOptions = list(
                     modifier = list(page = "all")
                   )
              )
            )
          )
        )
      })
      
      output$bigramviz <- renderPlot({
        
        narratives_prepdf() %>%
          count(word1, word2, sort = T) %>%
          top_n(50) %>%
          visualize_bigrams()
      }, bg = "transparent")
      
      # /////////////////////////////////////////////////////////////////////////////////////////
      ## Wordcloud ----
      
      output$wordcloudy <- renderWordcloud2({
        
        wordcloud2a(
          narratives_prepdf() %>%
            count(ngram, sort = T) %>%
            rename("word" = "ngram") %>%
            rename("freq" = "n"),
          color = "black",
          backgroundColor = "transparent"
        )
      })
      
      # /////////////////////////////////////////////////////////////////////////////////////////
      ## Sentences ----
      
      term <- reactive({input$term})
      
      output$narratives_sentences <- renderDataTable({
        
        DT::datatable(
          narratives_subdf() %>%
            unnest_tokens(sentence, Narrative, token = "sentences") %>%
            group_by_at(vars(-sentence)) %>%
            summarise(Sentences = paste0(sentence, collapse = "[...]")) %>%
            ungroup(),
          
          rownames = FALSE,
          filter   = "top",
          extensions = c("Buttons"),
          options = list(
            dom = 'lBfrtip',
            buttons = list(
              list(extend = "csv", text = "Download Full Results", filename = "MERNarrativeSentences",
                   exportOptions = list(
                     modifier = list(page = "all")
                   )
              )
            )
          )
        ) # END OF Dashboard Section
      })
      
      # /////////////////////////////////////////////////////////////////////////////////////////
      ## Download ----
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste('NarrativesRaw_', Sys.Date(), '.csv', sep='_')
        },
        content = function(con) {
          write.csv(narratives_df, con)
        }
      )
      
      output$downloadData2 <- downloadHandler(
        filename = function() {
          paste('NarrativesPrepped-', Sys.Date(), '.csv', sep='_')
        },
        content = function(con) {
          write.csv(narratives_prepdf(), con)
        }
      )
    }
    
  })
  
  
}
