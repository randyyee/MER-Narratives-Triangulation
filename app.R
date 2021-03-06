library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(readxl)
library(googlesheets4)
library(tidyverse)
library(rpivotTable)
library(tidytext)
library(DT)
library(reshape2)
library(wordcloud2)
library(ggplot2)
library(igraph)
library(ggraph)
library(leaflet)
library(crayon)

# Diagnostics
#library(profvis)
#library(reactlog)

options(shiny.maxRequestSize=4000*1024^2)
#options(shiny.reactlog = TRUE)

##### Functions ####
source("./functions/data_management.R")
source("./functions/sentiments_management.R")
source("./functions/bigrams_management.R")

##### Modules ####
source("./modules/mod_home.R")
source("./modules/mod_dashboard.R")
source("./modules/submod_ui_updater.R")
source("./modules/mod_mapboard.R")
source("./modules/mod_impacttable.R")
source("./modules/mod_resources.R")
source("./modules/mod_triangulation.R")

#### Test Data ####
#load("narratives_data.RData")

#### Text Resources ####
gs4_deauth()

bing          <- read_sheet("https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit?usp=sharing",
                         sheet = "HIV Sentiments (based on Bing et al.)")

stopwords     <- read_sheet("https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit?usp=sharing",
                         sheet = "Stopwords")

negationwords <- read_sheet("https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit?usp=sharing",
                         sheet = "Negation")

covidwords    <- read_sheet("https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit?usp=sharing",
                         sheet = "Covidwords")
#--------------------------------------------------------------------------------------------#
#### UI: Sidebar ####
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home",              tabName = "home",        icon = icon("home")),
    menuItem("Dashboard",         tabName = "analyses",    icon = icon("line-chart")),
    menuItem("Ngrams Explorer",   tabName = "impacttable", icon = icon("compass")),
    menuItem("MER Triangulation", tabName = "triangulate", icon = icon("puzzle-piece")),
    menuItem("Resources",         tabName = "resources",   icon = icon("book")),
    fileInput("n_path",
              shiny::HTML("<span style='color: white'>Choose Narratives File (xlsx)</span>"),
              multiple = FALSE),
    fileInput("m_path",
              shiny::HTML("<span style='color: white'>Choose MSD File (txt)</span>"),
              multiple = FALSE)
  )
)


#### UI: Body ####
body <- dashboardBody(
  shinyDashboardThemes(theme = "poor_mans_flatly"),
  tabItems(
    tabItem(tabName = "home",        home_ui()),
    tabItem(tabName = "analyses",    dashboard_ui("d")),
    tabItem(tabName = "impacttable", impact_ui("i")),
    tabItem(tabName = "triangulate", triangulation_ui("t")),
    tabItem(tabName = "resources",   resources_ui("r"))
  )
)


#### UI ####
ui <- dashboardPage(
  title = "Narrator",
  dashboardHeader(title = "Narratives Application"),
  sidebar,
  body
)


#--------------------------------------------------------------------------------------------#
#### SERVER ####
server <- function(input, output, session){
  
  # Import Datasets
  msd_df <- reactive({
    req(input$m_path)
    msd_import(input$m_path$datapath)
  })
  
  narratives_df <- reactive({
    req(input$n_path)
    filter(read_excel(input$n_path$datapath, col_types = "text", skip = 7), !str_detect(`Operating Unit`, "Office"))
  })
  
  prepared_dfs <- reactive({

    bigrams <- prepare_bigrams(narratives_df(), bing, stopwords, negationwords)
    sents   <- prepare_sentiments(bigrams)
    #sents_c <- prepare_sent_contributes(bigrams)
    return(list(bigrams, sents))

  })
  
  #prepared_dfs <- reactive({list(bigrams, sents, sents_c)})
  
  
  # Server Modules
  dashboard_server("d", narratives_df(), prepared_dfs())
  
  impact_server("i", narratives_df())
  
  resources_server("r", bing, stopwords, covidwords)
  
  triangulation_server("t", narratives_df(), msd_df())
  
}
                      


#--------------------------------------------------------------------------------------------#
shinyApp(ui = ui, server = server)

