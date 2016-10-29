library(jsonlite)
library(httr)
source("Extract_data_functions.R")

#GitHub authentication data
username = "jonaslieben"
password = "test123"

#Github project data
owner = "twitter"
repository = "twitter-server"

#set up the authentication 
authenticate <- authenticate(username,password)

branchIdentifiers <- retrieveIdentifiersBranches(authenticate,owner,repository)
branchIdentifiers

#for(string in shaStrings)
