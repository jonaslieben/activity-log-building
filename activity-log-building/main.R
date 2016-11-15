library(jsonlite)
library(httr)
library(lubridate)
library(dplyr)

source("Extract_data_functions.R")
source("Classify_commit_messages.R")

#GitHub authentication data
username = "jonaslieben"
#do not forget to add a password
password = "test123"

#Github project data
owner = "apple"
repository = "swift-corelibs-foundation"

#owner = "jonaslieben"
#repository = "activity-log-building"
  
#set up the authentication 
authenticate <- authenticate(username,password)

branchIdentifiers <- retrieveIdentifiersBranches(authenticate,owner,repository)
branchIdentifiers

commitIdentifiers <- retrieveAllCommitIdentifiers(authenticate, owner, repository)
commitIdentifiers

eventData <- extractEventData(authenticate, owner, repository)
eventData <- addBeginningTimestamp(eventData)
str(eventData)

eventDataTable <- countAccordingToClassificationScheme(eventDataTable =  preprocessingNlp(eventData), classificationScheme = loadClassificationScheme())
eventDataTable <- classifyCommit(eventDataTable)
eventDataTable %>% select(message, type) %>% group_by(type) %>% summarise(n = n())