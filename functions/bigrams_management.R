# Bigram functions adapted from tidytext vignettes

prepare_bigrams <- function(dataset, sentiments, stopwords, negationwords, numm){
  
  stopwords2 <- anti_join(stopwords, negationwords)
  negationwords <- negationwords %>% mutate(negate = "negate")
  
  dataset %>%
    unnest_tokens(ngram, Narrative, token = "ngrams", n = numm) %>%# Bigram functions adapted from tidytext vignettes

prepare_bigrams <- function(dataset, sentiments, stopwords, negationwords, numm){
  
  stopwords2 <- anti_join(stopwords, negationwords)
  negationwords <- negationwords %>% mutate(negate = "negate")
  
  dataset %>%
    unnest_tokens(ngram, Narrative, token = "ngrams", n = numm, drop = F) %>%
    mutate(ngram = str_replace(ngram, "\\s", "|")) %>%
    separate(ngram, into = c("word1", "word2"), sep = "\\|") %>%
    filter(!word1 %in% stopwords2$word, # stopwords - negation
           !word2 %in% stopwords$word) %>% # all stopwords
    filter(!str_detect(word1, "\\d"),
           !str_detect(word2, "\\d"))  %>%
    left_join(negationwords, by = c("word1" = "word"))%>%
    left_join(sentiments,    by = c("word1" = "word"))%>%
    left_join(sentiments,    by = c("word2" = "word")) %>%
    mutate(negate      = case_when(negate == "negate" ~-1,
                                   TRUE ~ NA_real_),
           sentiment.x = case_when(sentiment.x == "positive" ~ 1,
                                   sentiment.x == "negative" ~ -1,
                                   TRUE ~ NA_real_),
           sentiment.y = case_when(sentiment.y == "positive" ~ 1,
                                   sentiment.y == "negative" ~ -1,
                                   TRUE ~ NA_real_)) %>%
    rowwise()%>%
    mutate(sentiment   = case_when(is.na(sentiment.x) & is.na(sentiment.y) ~NA_real_,
                                   TRUE ~prod(negate, sentiment.x, sentiment.y, na.rm = T))) %>%
    ungroup() %>%
    mutate(sentiment   = case_when(sentiment ==  1 ~ "positive",
                                   sentiment == -1 ~ "negative",
                                   TRUE ~ NA_character_)) %>%
    unite(ngram, word1, word2, sep = " ", remove = F) %>%
    unite(Name, `Operating Unit`, `Indicator Bundle`, `Indicator`, sep = "|", remove = F)
  
}


count_bigrams <- function(dataset, stopwords){
  
  dataset %>%
    unnest_tokens(ngram, Narrative, token = "ngrams", n = 2) %>%
    mutate(ngram = str_replace(ngram, "\\s", "|")) %>%
    separate(ngram, into = c("word1", "word2"), sep = "\\|") %>%
    filter(!word1 %in% stopwords$word,
           !word2 %in% stopwords$word) %>%
    filter(!str_detect(word1, "\\d"),
           !str_detect(word2, "\\d")) %>%
    count(word1, word2, sort = T)
  
}


visualize_bigrams <- function(bigrams){
  
  set.seed(2021)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>% 
    graph_from_data_frame() %>%
    ggraph(layout =  "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = F, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
  
}

# Wordcloud2 Solution from https://github.com/rstudio/shinydashboard/issues/281
wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -base:::pi/4, maxRotation = base:::pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}
    mutate(ngram = str_replace(ngram, "\\s", "|")) %>%
    separate(ngram, into = c("word1", "word2"), sep = "\\|") %>%
    filter(!word1 %in% stopwords2$word, # stopwords - negation
           !word2 %in% stopwords$word) %>% # all stopwords
    filter(!str_detect(word1, "\\d"),
           !str_detect(word2, "\\d"))  %>%
    left_join(negationwords, by = c("word1" = "word"))%>%
    left_join(sentiments,    by = c("word1" = "word"))%>%
    left_join(sentiments,    by = c("word2" = "word")) %>%
    mutate(negate      = case_when(negate == "negate" ~-1,
                                   TRUE ~ NA_real_),
           sentiment.x = case_when(sentiment.x == "positive" ~ 1,
                                   sentiment.x == "negative" ~ -1,
                                   TRUE ~ NA_real_),
           sentiment.y = case_when(sentiment.y == "positive" ~ 1,
                                   sentiment.y == "negative" ~ -1,
                                   TRUE ~ NA_real_)) %>%
    rowwise()%>%
    mutate(sentiment   = case_when(is.na(sentiment.x) & is.na(sentiment.y) ~NA_real_,
                                   TRUE ~prod(negate, sentiment.x, sentiment.y, na.rm = T))) %>%
    ungroup() %>%
    mutate(sentiment   = case_when(sentiment ==  1 ~ "positive",
                                   sentiment == -1 ~ "negative",
                                   TRUE ~ NA_character_)) %>%
    unite(ngram, word1, word2, sep = " ", remove = F) %>%
    unite(Name, `Operating Unit`, `Indicator Bundle`, `Indicator`, sep = "|", remove = F)
  
}


count_bigrams <- function(dataset, stopwords){
  
  dataset %>%
    unnest_tokens(ngram, Narrative, token = "ngrams", n = 2) %>%
    mutate(ngram = str_replace(ngram, "\\s", "|")) %>%
    separate(ngram, into = c("word1", "word2"), sep = "\\|") %>%
    filter(!word1 %in% stopwords$word,
           !word2 %in% stopwords$word) %>%
    filter(!str_detect(word1, "\\d"),
           !str_detect(word2, "\\d")) %>%
    count(word1, word2, sort = T)
  
}


visualize_bigrams <- function(bigrams){
  
  set.seed(2021)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>% 
    graph_from_data_frame() %>%
    ggraph(layout =  "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = F, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
  
}

# Wordcloud2 Solution from https://github.com/rstudio/shinydashboard/issues/281
wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -base:::pi/4, maxRotation = base:::pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}
