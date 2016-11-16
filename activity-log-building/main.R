library(jsonlite)
library(httr)
library(lubridate)
library(dplyr)
library(tm)
library(stringr)

source("Extract_data_functions.R")
source("Classify_commit_messages.R")
source("Calculate_right_beginning_timestamp.R")

#GitHub authentication data
username = "jonaslieben"
#do not forget to add a password
password = ""

#Github project data
owner = "apple"
repository = "swift-corelibs-foundation"
  
#set up the authentication 
authenticate <- authenticate(username,password)

#Extract the data from Github and add a provisionalTimestamp
eventData <- extractEventData(authenticate, owner, repository)
eventData <- addProvisionalBeginningTimestamp(eventData)

#classify the activities into 4 different types (adaptive, corrective, perfective and unknown)
eventDataTable <- countAccordingToClassificationScheme(eventDataTable =  preprocessingNlp(eventData), classificationScheme = loadClassificationScheme())
eventDataTable <- classifyCommit(eventDataTable)

#add the correct beginningtimestamp
eventDataTable <- removeBeginningTimeStampFromOutlierObservations(eventDataTable)
eventDataTable <- addBeginningTimeStampsToFirstCommits(eventDataTable)

#save everything in the eventData variable
eventData <- eventDataTable

#remove variables which are not relevant
eventData$identifier <- NULL
eventDataTable <- NULL


