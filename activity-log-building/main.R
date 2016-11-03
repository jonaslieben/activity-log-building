library(jsonlite)
library(httr)


source("Extract_data_functions.R")

#GitHub authentication data
username = "jonaslieben"
#do not forget to add a password
password = "test123"

#Github project data
owner = "twitter"
repository = "twitter-server"

#owner = "jonaslieben"
#repository = "activity-log-building"
  
#set up the authentication 
authenticate <- authenticate(username,password)

branchIdentifiers <- retrieveIdentifiersBranches(authenticate,owner,repository)
branchIdentifiers

commitIdentifiers <- retrieveAllCommitIdentifiers(authenticate, owner, repository)
commitIdentifiers

eventData <- extractEventData(authenticate, owner, repository)
str(eventData)
