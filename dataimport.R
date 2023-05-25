# /////////////////////////////////////////////////////////////////////////////////////////
#### Text Resources ####
bing          <- readxl::read_excel("data/HIV_AIDS_Narrative_Resources.xlsx", sheet = "HIV Sentiments (based on Bing e")
stopwords     <- readxl::read_excel("data/HIV_AIDS_Narrative_Resources.xlsx", sheet = "Stopwords")
negationwords <- readxl::read_excel("data/HIV_AIDS_Narrative_Resources.xlsx", sheet = "Negation")

#### Metadata ####
# nmeta_df <- aws.s3::s3read_using(FUN       = read.csv,
#                                  bucket     = Sys.getenv("TEST_BUCKET"),
#                                  object     = "Narratives/Current_Frozen/detailed_metadata.csv")
# mmeta_df <- aws.s3::s3read_using(FUN       = read.csv,
#                                  bucket    = Sys.getenv("TEST_BUCKET"),
#                                  object    = "MER_Structured_Datasets/Current_Frozen/OU_Recent/summary_metadata.csv")

#### Narratives Resources ####

narratives_df <- aws.s3::s3read_using(FUN       = readxl::read_excel,
                                      col_types = "text",
                                      bucket    = Sys.getenv("TEST_BUCKET"),
                                      object    = "Narratives/Current_Frozen/xlsx/Narratives.xlsx") %>%
  unite("period", fiscal_year:fiscal_quarter, remove = T, sep = " ") %>%
  filter(operating_unit != "\r\nOffice of U.S. Foreign Assistance Resources") %>%
  rename_all(~str_to_title(str_replace_all(., "_", " ")))

narratives_meta <- narratives_df %>%
  select(c("Operating Unit", "Country", "Indicator Bundle", "Indicator", "Period", "Funding Agency", "Implementing Mechanism Name")) %>%
  distinct()

#### MSD Resources ####

msd_df <- aws.s3::s3read_using(FUN           = readr::read_delim, "|",
                               escape_double = FALSE,
                               trim_ws       = TRUE,
                               col_types     = readr::cols(.default   = readr::col_character(),
                                                           targets    = readr::col_double(),
                                                           qtr1       = readr::col_double(),
                                                           qtr2       = readr::col_double(),
                                                           qtr3       = readr::col_double(),
                                                           qtr4       = readr::col_double(),
                                                           cumulative = readr::col_double()),
                               bucket        = Sys.getenv("TEST_BUCKET"),
                               object        = "MER_Structured_Datasets/Current_Frozen/OU_Recent/txt/MER_Structured_Datasets_OU_IM_Recent.txt") %>%
  rename(Q1 = qtr1, Q2 = qtr2, Q3 = qtr3, Q4 = qtr4) %>%
  pivot_longer(targets:cumulative, names_to = "attribute", values_to = "value") %>%
  unite("period", c("fiscal_year", "attribute"), sep = " ", remove = F) %>%
  filter(!is.na(value)) %>%
  rename_all(~str_to_title(str_replace_all(., "_", " "))) %>%
  rename(`Operating Unit` = Operatingunit,
         `Support Type` = Indicatortype,
         `Mechanism Code` = `Mech Code`)
