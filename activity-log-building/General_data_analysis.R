library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)

#Amount of commits
amountOfCommits <- function(eventDataTable) {
  #select all unique commits and count them
  return(select(eventDataTable, identifier) %>% 
           distinct() %>% 
           count())
}
#Amount of unique files currently in project
amountOfFilesCurrrentlyInProject <- function(eventDataTable) {
  #select all files which are added, count them. Do the same for files which are removed. Amount of files equals added minus removed
  return(select(eventDataTable, status) %>% 
           filter(status == "added") %>% 
           count()) - 
    (select(eventDataDplyr, status) %>% 
       filter(status == "removed") %>% 
       count())
}
#Amount of unique files which have been in the project
amountOfFilesHaveBeenInProject <- function(eventDataTable) {
  #select all unique files and count them
  return(select(eventDataTable, filename) %>% 
           distinct() %>% 
           count())
}
#Amount of people who contributed
amountOfPeopleContributed <- function(eventDataTable) {
  #select all unique authors and count them
  return(select(eventDataTable, author) %>% 
           distinct() %>% 
           count())
}
#Average amount of commits by one person
averageCommitsPerPerson <- function(eventDataTable) {
  #average = amount of commits divided by amount of people who contributed
  return(amountOfCommits(eventDataTable) / amountOfPeopleContributed(eventDataTable))
}

#Max amount of commits by one person
maxCommitsPerPerson <- function(eventDataTable) {
  #select column author and identifier, group table on author and count the amount of identifier per person
  commitsByOnePerson <- eventDataTable %>% 
    select(author,identifier) %>% 
    distinct() %>% 
    group_by(author) %>% 
    summarise(commits = n())
  
  #take the maximum 
  return(summarise(commitsByOnePerson,max(commits)))
}

#Min amount of commits by one person
minCommitsPerPerson <- function(eventDataTable) {
  #select column author and identifier, group table on author and count the amount of identifiers per person
  commitsByOnePerson <- eventDataTable %>% 
    select(author,identifier) %>% 
    distinct() %>% 
    group_by(author) %>% 
    summarise(commits = n())
  
  #take the minimum 
  return(summarise(commitsByOnePerson,min(commits)))
}

#Max amount of files changed by one person
maxFilesChangedPerPerson <- function(eventDataTable) {
  #select column author and filename, delete all duplicated rows, group table on author and count the amount of files per person
  filesChangedByOnePerson <- eventDataTable %>% 
    select(author,filename) %>% 
    distinct() %>% 
    group_by(author) %>% 
    summarise(files = n())
  #take the maximum
  return(summarise(filesChangedByOnePerson, max(files)))
}

#Min amount of files changed by one person
minFilesChangedPerPerson <- function(eventDataTable) {
  #select column author and filename, delete all duplicated rows, group table on author and count the amount of files per person
  filesChangedByOnePerson <- eventDataTable %>% 
    select(author,filename) %>% 
    distinct() %>% 
    group_by(author) %>% 
    summarise(files = n())
  #take the minimum
  return(summarise(filesChangedByOnePerson, min(files)))
}

#Amount of files added, modified, removed and renamed 
amountOfFileOperationsPerType <- function(eventDataTable) {
  #select the identifier and the status of all records, group on status and count the amount of records
  return(eventDataTable %>% 
           select(identifier, status) %>% 
           group_by(status) %>% 
           summarise(amount = n()))
}

#Average amount of modification, additions, removes and renames per person
averageAmountOfFilesOperationsPerTypePerPerson <- function(eventDataTable) {
  #select the identifier, the author and the status of all records, group on status and author and count the amount of records
  operationsPerTypePerAuthor <- eventDataTable %>% 
    select(identifier, status, author) %>% 
    group_by(status, author) %>% 
    summarise(amount = n())
  #calculate the average for each status by grouping on status and taking the mean
  averageOperations <- operationsPerTypePerAuthor %>% 
    group_by(status) %>% 
    summarise(mean(amount))
  return(averageOperations)
}

#Max amount of modification, additions, removes and renames per person
maxAmountOfFilesOperationsPerTypePerPerson <- function(eventDataTable) {
  #select the identifier, the author and the status of all records, group on status and author and count the amount of records
  operationsPerTypePerAuthor <- eventDataTable %>% 
    select(identifier, status, author) %>% 
    group_by(status, author) %>% 
    summarise(amount = n())
  #calculate the maximum for each status by grouping on status and taking the mean
  maxOperations <- operationsPerTypePerAuthor %>% 
    group_by(status) %>% 
    summarise(max(amount))
  return(maxOperations)
}

#Min amount of modification, additions, removes and renames per person
minAmountOfFilesOperationsPerTypePerPerson <- function(eventDataTable) {
  #select the identifier, the author and the status of all records, group on status and author and count the amount of records
  operationsPerTypePerAuthor <- eventDataTable %>% 
    select(identifier, status, author) %>% 
    group_by(status, author) %>% 
    summarise(amount = n())
  #calculate the maximum for each status by grouping on status and taking the mean
  minOperations <- operationsPerTypePerAuthor %>% 
    group_by(status) %>%
    summarise(min(amount))
  return(minOperations)
}
#Commit messages used more than once for a different commits
commitMessagesUsedMoreThanOnce <- function(eventDataTable) {
  #select the identifier and message, filter out duplicates, group on message, count the amount of rows and filter all the rows with an amount bigger than 1
  return(eventDataTable %>% 
           select(identifier, message) %>% 
           distinct() %>% 
           group_by(message) %>%
           summarise(amount = n()) %>%
           filter(amount > 1))
}

#Author of commits with the same message
AuthorOfCommitsWithMessagesUsedMoreThanOnce <- function(eventDataTable) {
  #select the identifier and message, filter duplicates, group on message, count the amount of rows and filter all the rows with an amount bigger than 1
  messagesUsedMoreThanOnce <- eventDataTable %>%
    select(identifier, message) %>%
    distinct() %>%
    group_by(message) %>%
    summarise(amount = n()) %>%
    filter(amount > 1)
  #select the message and author, filter for the messages which were found in the previous step and filter out duplicates
  return(eventDataTable %>% 
           select(message, author) %>%
           filter(message %in% messagesUsedMoreThanOnce$message) %>%
           distinct())
}

#Ten most modified files
tenMostModifiedFiles <- function(eventDataTable) {
  #select the identifier, the status and the filename and filter all rows with a status equal to modified. 
  #group the data on filename and calculate the amount of rows
  modificationsInFile <- eventDataTable %>% 
    select(identifier, status, filename) %>% 
    filter(status == "modified") %>% 
    group_by(filename) %>% 
    summarise(amount = n())
  #order the rows in descending order based on the column amount and take the top 10 rows
  return(modificationsInFile %>% arrange(desc(amount)) %>% top_n(10))
}
#Ten most active people in terms of commits
tenMostActivePeopleOnCommits <- function(eventDataTable) {
  #select the author and the identifier, remove the duplicate rows and group on author, calculate the amount of rows 
  #and ungroup in order that they can be ordered in descending order. Show only the top 10 rows
  return(eventDataTable %>%
           select(author, identifier) %>%
           distinct() %>%
           group_by(author) %>% 
           summarise(amount = n()) %>% 
           ungroup() %>% 
           arrange(desc(amount)) %>% 
           top_n(10))
}

#Ten most active people in terms of files changed, added, removed and renamed
tenMostActivePeopleOnFileOperations <- function(eventDataTable) {
  #select the author and the identifier, 
  #leave the duplicate rows (difference with function above), because each row represents a file which was modified, added, removed or renamed 
  #group on author, calculate the amount of rows 
  #and ungroup in order that they can be ordered in descending order. Show only the top 10 rows
  return(eventDataTable %>%
           select(author, identifier) %>%
           group_by(author) %>% 
           summarise(amount = n()) %>% 
           ungroup() %>% 
           arrange(desc(amount)) %>% 
           top_n(10))
}

#The number of different files on which the ten most active people work 
numberFilesTenMostActivePeople <- function(eventDataTable) {
  #get the ten most active people
  authors <- tenMostActivePeopleOnFileOperations(eventDataTable)
  #filter the dataset based on these people, select the column author and file name, 
  #remove duplicate rows (otherwise we will not have measured the number of different files)
  #group by author, count the amount of rows, 
  #ungroup in order that we can order in descending order
  return(eventDataTable %>% 
           filter(author %in% authors$author) %>% 
           select(author, filename) %>% 
           distinct() %>% 
           group_by(author) %>% 
           summarise(number = n()) %>% 
           ungroup %>% 
           arrange(desc(number)))
}
#Average amount of files per commit
averageFilesPerCommit <- function(eventDataTable) {
  #average = amount of unique files divided by the amount of commits
  return(amountOfFilesHaveBeenInProject(eventDataTable) / amountOfCommits(eventDataTable))
}

#Average amount of file changes (added/modified/removed) per person
averageFilesPerPerson <- function(eventDataTable) {
  #avg equals average commits per person times average files per commit
  return(averageCommitsPerPerson(eventDataTable) * averageFilesPerCommit(eventDataTable))
}

#Commits over time per month
commitsOverTime <- function(eventDataTable) {
  #save event data table in a temporary variable in order that the original one will not be manipulated
  eventDataTableTemp <- eventDataTable
  #convert the timestamp to a date by keeping only the first ten characters
  eventDataTableTemp$date <- ymd(substr(eventDataTableTemp$timestamp, 0, 10))
  #set the day of every variable in date to one
  eventDataTableTemp$date <- floor_date(eventDataTableTemp$date, "month")
  #make the input table for the chart which contains the date and all commits, with removed duplicated rows, grouped on date in order that the amount of commits per date can be summed
  inputDataChart <- eventDataTableTemp %>% 
    select(date, identifier) %>% 
    distinct() %>% 
    group_by(date) %>% 
    summarise(number = n())
  #make a line chart with on the x-axis the date and the y-axis the number of commits
  chart <- ggplot(inputDataChart) + geom_line(aes(x = date, y = number)) + ylab("number of commits")
  return(chart)
}
#Files changes/added/deleted over time
fileOperationsOverTime <- function(eventDataTable) {
  #save event data table in a temporary variable in order that the original one will not be manipulated
  eventDataTableTemp <- eventDataTable
  #convert the timestamp to a date by keeping only the first ten characters
  eventDataTableTemp$date <- ymd(substr(eventDataTableTemp$timestamp, 0, 10))
  #set the day of every variable in date to one
  eventDataTableTemp$date <- floor_date(eventDataTableTemp$date, "month")
  #make the input table for the modifiedchart which contains the date and all files which are modified grouped on date in order that the number of modified files can be summed
  inputDataChart <- eventDataTableTemp %>% 
    select(filename,status, identifier, date) %>% 
    filter(status == "modified") %>%
    group_by(date) %>% 
    summarise(amount = n()) 
  #make the chart using ggplot and a bar chart with inputDataChart as the data, the date mapped on the x-axis and the amount mapped on the y-axis
  modifiedChart <- ggplot(inputDataChart) + geom_bar(aes(x = date, y = amount), stat = "identity") + ylab("number of files modified")
  
  #make the input table for the addedChart which contains the date and all files which are added grouped on date in order that the number of added files can be summed
  inputDataChart <- eventDataTableTemp %>% 
    select(filename,status, identifier, date) %>% 
    filter(status == "added") %>%
    group_by(date) %>% 
    summarise(amount = n())
  #make the chart using ggplot and a bar chart with inputDataChart as the data, the date mapped on the x-axis and the amount mapped on the y-axis
  addedChart <- ggplot(inputDataChart) + geom_bar(aes(x = date, y = amount), stat = "identity") + ylab("number of files added")
  
  #make the input table for the removedChart which contains the date and all files which are removed grouped on date in order that the number of removed files can be summed
  inputDataChart <- eventDataTableTemp %>% 
    select(filename,status, identifier, date) %>% 
    filter(status == "removed") %>%
    group_by(date) %>% 
    summarise(amount = n()) 
  #make the chart using ggplot and a bar chart with inputDataChart as the data, the date mapped on the x-axis and the amount mapped on the y-axis
  removedChart <- ggplot(inputDataChart) + geom_bar(aes(x = date, y = amount), stat = "identity") + ylab("number of files removed")
  
  #make the input table for the renamedChart which contains the date and all files which are added grouped on date in order that the number of renamed files can be summed
  inputDataChart <- eventDataTableTemp %>% 
    select(filename,status, identifier, date) %>% 
    filter("renamed") %>%
    group_by(status, date) %>% 
    summarise(amount = n()) 
  #make the chart using ggplot and a bar chart with inputDataChart as the data, the date mapped on the x-axis and the amount mapped on the y-axis
  renamedChart <- ggplot(inputDataChart) + geom_bar(aes(x = date, y = amount), stat = "identity") + ylab("number of files renamed")
  
  #put all charts into one grid with two columns
  grid.arrange(modifiedChart, addedChart, removedChart, renamedChart, ncol = 2)
  return(grid)
}

#Amount of active users over time
amountOfActiveUsers <- function(eventDataTable) {
  #save event data table in a temporary variable in order that the original one will not be manipulated
  eventDataTableTemp <- eventDataTable
  #convert the timestamp to a date by keeping only the first ten characters
  eventDataTableTemp$date <- ymd(substr(eventDataTableTemp$timestamp, 0, 10))
  #set the day of every variable in date to one
  eventDataTableTemp$date <- floor_date(eventDataTableTemp$date, "month")
  #make the input table for the chart which contains the date and all authors where duplicate rows are removed, grouped on date in order that the amount of active users per month can be calculated
  inputDataChart <- eventDataTableTemp %>%
    select(author, date) %>% 
    distinct() %>%
    group_by(date) %>% 
    summarise(amount = n()) 
  
  #calculate the min amount of active users in a month
  minimumActiveUsers <- inputDataChart %>% summarise(min(amount))
  #calculate the max amount of active users in a month
  maximumActiveUsers <- inputDataChart %>% summarise(max(amount))

  #make the chart using ggplot and a line chart with inputDataChart as the data, the date mapped on the x-axis and the amount mapped on the y-axis
  #the y-axis is using a scale with tick-marks from the min to the max amount of active users in steps of one
  chart <- ggplot(inputDataChart) + geom_line(aes(x = date, y = amount)) + ylab("amount of active users") + scale_y_continuous(breaks = minimumActiveUsers$`min(amount)`:maximumActiveUsers$`max(amount)`)
  return(chart)
}