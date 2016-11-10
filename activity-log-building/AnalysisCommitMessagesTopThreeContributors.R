topThreeContributors <- eventDataTable %>%
  select(author, identifier) %>%
  group_by(author) %>% 
  summarise(amount = n()) %>% 
  ungroup() %>% 
  arrange(desc(amount)) %>% 
  top_n(3) %>% select(author)