library(jsonlite)
library(httr)
source("Extract_data_functions.R")

#GitHub authentication data
username = "jonaslieben"
password = 

#Github project data
owner = "twitter"
repository = "twitter-server"

#set up the authentication 
authenticate <- authenticate(username,password)

branchIdentifiers <- retrieveIdentifiersBranches(authenticate,owner,repository)
branchIdentifiers

commitIdentifiers <- retrieveAllCommitIdentifiers(authenticate, owner, repository)
commitIdentifiers