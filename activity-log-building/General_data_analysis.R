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

#Max amount of commits by one person
maxCommitsPerPerson <- function(eventDataTable) {
  #select column author and identifier, group table on author and count the amount of identifier per person
  commitsByOnePerson <- eventDataTable %>% select(author,identifier) %>% distinct() %>% group_by(author) %>% summarise(commits = n())
  
  #take the maximum 
  return(summarise(commitsByOnePerson,max(commits)))
}

#Min amount of commits by one person
minCommitsPerPerson <- function(eventDataTable) {
  #select column author and identifier, group table on author and count the amount of identifiers per person
  commitsByOnePerson <- eventDataTable %>% select(author,identifier) %>% distinct() %>% group_by(author) %>% summarise(commits = n())
  
  #take the minimum 
  return(summarise(commitsByOnePerson,min(commits)))
}

#Max amount of files changed by one person
maxFilesChangedPerPerson <- function(eventDataTable) {
  #select column author and filename, delete all duplicated rows, group table on author and count the amount of files per person
  filesChangedByOnePerson <- eventDataTable %>% select(author,filename) %>% distinct() %>% group_by(author) %>% summarise(files = n())
  #take the maximum
  return(summarise(filesChangedByOnePerson, max(files)))
}

#Min amount of files changed by one person
minFilesChangedPerPerson <- function(eventDataTable) {
  #select column author and filename, delete all duplicated rows, group table on author and count the amount of files per person
  filesChangedByOnePerson <- eventDataTable %>% select(author,filename) %>% distinct() %>% group_by(author) %>% summarise(files = n())
  #take the minimum
  return(summarise(filesChangedByOnePerson, min(files)))
}

#Amount of files added, modified, removed and renamed 
amountOfFileOperationsPerType <- function(eventDataTable) {
  eventDataDplyr %>% select(identifier, status) %>% group_by(status) %>% summarise(amount = n())
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