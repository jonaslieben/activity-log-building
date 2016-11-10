



simplifyCommitMessage <- function(eventDataTable, contributorNumberOfTop, amountOfWords) {
  # look up top 3 thre contributors by selecting the author, identifier, group by author in order to count the number of rows for
  # each author, ungroup in order to order all rows in descending order and select only the top 3 authors
  topContributors <- eventDataTable %>%
    select(author, identifier) %>%
    group_by(author) %>% 
    summarise(amount = n()) %>% 
    ungroup() %>% 
    arrange(desc(amount)) %>% 
    top_n(contributorNumberOfTop) %>% select(author)
  
  # use only the distinct commit messages for that author
  commitMessages <- eventDataTable %>% filter(author == unlist(topContributors$author[contributorNumberOfTop])) %>% select(message) %>% distinct()
  # convert the commit message to character variables in order to apply some mutations on these character variables
  commitMessages <- rapply(commitMessages, as.character, classes="factor", how="replace")
  
  # for each row
  for(i in 1:length(commitMessages$message)) {
    # check if the string contains a ":". In most cases there is a filename mentioned before this. Therefore, we keep all words after the ":".
    # Then we trim the string in order that no spaces are at the beginning of the string
    # Then we keep only the amountOfWords given in the input
    if(grepl(":", commitMessages$message[i])) {
      commitMessages$message[i] <- word(commitMessages$message[i], 2, sep=":")
      commitMessages$message[i] <- trimws(commitMessages$message[i])
      commitMessages$message[i] <- word(commitMessages$message[i], 1, amountOfWords)
      # check if the string contains a "]". In most cases there is something done to the branch such as a split. 
      #As this is less relevant for the activity, we don't keep this Therefore, we keep all words after the "]".
      # Then we trim the string in order that no spaces are at the beginning of the string
      # Then we keep only the amountOfWords given in the input
    } else if(grepl("]", commitMessages$message[i])) {
      commitMessages$message[i] <- word(commitMessages$message[i], 2, sep="]")
      commitMessages$message[i] <- trimws(commitMessages$message[i])
      commitMessages$message[i] <- word(commitMessages$message[i], 1, amountOfWords)
    } else {
      commitMessages$message[i] <- word(commitMessages$message[i], 1, amountOfWords)
    }
  }
  return(commitMessages$message)
}
