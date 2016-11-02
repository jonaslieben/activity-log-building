library(dplyr)
library(lubridate)

#Amount of commits
amountOfCommits <- function(eventDataTable) {
  #select all unique commits and count them
  return(select(eventDataTable, identifier) %>% distinct() %>% count())
}
#Amount of unique files currently in project
amountOfFilesCurrrentlyInProject <- function(eventDataTable) {
  #select all files which are added, count them. Do the same for files which are removed. Amount of files equals added minus removed
  return(select(eventDataTable, status) %>% filter(status == "added") %>% count()) - (select(eventDataDplyr, status) %>% filter(status == "removed") %>% count())
}
#Amount of unique files which have been in the project
amountOfFilesHaveBeenInProject <- function(eventDataTable) {
  #select all unique files and count them
  return(select(eventDataTable, filename) %>% distinct() %>% count())
}
#Amount of people who contributed
amountOfPeopleContributed <- function(eventDataTable) {
  #select all unique authors and count them
  return(select(eventDataTable, author) %>% distinct() %>% count())
}
#Average amount of commits by one person
averageCommitsPerPerson <- function(eventDataTable) {
  #average = amount of commits divided by amount of people who contributed
  return(amountOfCommits(eventDataTable) / amountOfPeopleContributed(eventDataTable))
}

#Average amount of files per commit
averageFilesPerCommit <- function(eventDataTable) {
  #average = amount of unique files divided by the amount of commits
  return(amountOfFilesHaveBeenInProject(eventDataTable) / amountOfCommits(eventDataTable))
}

#Average amount of file changes (added/modified/removed) per person
averageFilesPerPerson <- function(eventDataTable) {
  return(averageCommitsPerPerson(eventDataTable) * averageFilesPerCommit(eventDataTable))
}

#Commits over time

#file changes/added/deleted over time