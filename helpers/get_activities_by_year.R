get_activities_by_year = function(graph) {
  edges <- graph %>% as_data_frame(what="edges")
  
  start_df <- edges %>%
    mutate(year = lubridate::year(start_date)) %>%
    group_by(year) %>% summarize(count = n())
  
  end_df <- edges %>%
    mutate(year = lubridate::year(end_date)) %>%
    group_by(year) %>% summarize(count = n())
  
  txns_per_year <- start_df %>% rbind(end_df) %>%
    group_by(year) %>% summarize(count = sum(count)) %>%
    filter(!is.na(year))
}