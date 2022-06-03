home_ui <- function(){
  
  fluidRow(
    box(width = 12,
        tags$h2("PEPFAR MER Narratives"),
        tags$h3("Introduction"),
        p("This tool triangulates Monitoring, Evaluation, and Reporting (MER) narratives with indicator results.
          The Narratives Traingulation Tool takes in MER Narratives and MER Structured Datasets (MSD) providing cross-filtering between the two data sources.
          Once the required data sources have been imported, the pivot table visualizer can be used to investigate the content of the selected narrative using the MER indicators.
          This tool also has features for textual analysis of the narratives including sentiments, tf-idf, and n-grams analyses."),
        hr(),
        tags$h3("Instructions"),
        tags$a(href="https://pepfar-panorama.org/pepfarlanding/#login", "Download Narratives and MSDs here!"),
        tags$ol(
          tags$li("Explore the narratives using the dashboard"),
          tags$li("Summarize program changes using the Ngrams explorer"),
          tags$li("Triangulate the narrative with MER indicators"),
          tags$li("Evaluate and revise lexicon resources")
        ),
        hr(),
        tags$h3("Questions?"),
        tags$strong("Randy Yee (pcx5@cdc.gov)"),
        p("CDC | CGH | DGHT | HIDMSB | DSAT-ICPI"),
        tags$h3("Acknowledgements"),
        p(tags$strong("Siddharth Dixit (sdixit@baosystems.com)")),
        p(tags$strong("Max Chandler (mchandler@baosystems.com)")),
        p(tags$strong("Peter Gilbertson (pgilbertson@baosystems.com)")),
        p(tags$strong("Stephanie Fowler (sfowler@baosystems.com)")),
        p("PEPFAR PRIME Systems | PEPFAR Data Analytics Platform")
    ) # End home/fluidrow/box
  ) # End home/fluidrow
}