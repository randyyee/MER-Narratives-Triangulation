# Sentiment functions

prepare_sentiments <- function(dataset){
  
  dataset %>%
    filter(!is.na(sentiment)) %>%
    count(`Operating Unit`, `Indicator Bundle`, Indicator, ngram, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  
}


prepare_sent_contributes <- function(dataset){
  
  dataset %>%
    filter(!is.na(sentiment)) %>%
    count(ngram, sentiment, sort = T) %>%
    top_n(50) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n))%>%
    mutate(ngram = reorder(ngram, n))
  
}

sent_summary <- function(sent_df){

  ind <- sent_df %>%
    select(`Operating Unit`, `Indicator Bundle`, `Indicator`, sentiment) %>%
    group_by(`Operating Unit`, `Indicator Bundle`, `Indicator`) %>%
    summarize(total = sum(sentiment, na.rm = T)) %>%
    ungroup()
  
  return(
    list(
      HTML(
        paste(
          ind$`Operating Unit`[ind$total   == min(ind$total)],
          ind$`Indicator Bundle`[ind$total == min(ind$total)],
          ind$`Indicator`[ind$total        == min(ind$total)], 
          sep = "<br/>"
        )
      ),
      HTML(
        paste(
          ind$`Operating Unit`[ind$total   == max(ind$total)],
          ind$`Indicator Bundle`[ind$total == max(ind$total)],
          ind$`Indicator`[ind$total        == max(ind$total)], 
          sep = "<br/>"
        )
      )
    )
  )
  
}

sentiment_by_ou <- function(sent_df){
  
  prepare_sentiments(sent_df) %>%
    group_by(`Operating Unit`) %>%
    summarize(total = sum(sentiment, na.rm = T)) %>%
    ungroup()
  
}


sentiment_by_area <- function(sent_df){
  
  prepare_sentiments(sent_df) %>%
    group_by(`Operating Unit`, `Indicator Bundle`) %>%
    summarize(total = sum(sentiment, na.rm = T)) %>%
    ungroup()
  
}


sentiment_by_ind <- function(sent_df){
  
  prepare_sentiments(sent_df) %>%
      group_by(`Operating Unit`, `Indicator Bundle`, `Indicator`) %>%
      summarize(total = sum(sentiment, na.rm = T)) %>%
      ungroup()
  
}
