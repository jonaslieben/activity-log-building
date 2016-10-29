library(jsonlite)
library(httr)




retrieveIdentifiersBranches <- function(authentication, owner, repository) {
  #retrieve all branches of a project
  url <- paste("https://api.github.com/repos/", owner, "/", repository, "/branches", sep = "")
  branchesJSON <- GET(url, authentication)
  
  #parse the JSON output to dataframes
  branches <- fromJSON(content(branchesJSON, "text"))
  
  #save the sha (the unique identifier of in this case a branch) in an object in order that it can be used for retrieving the commits
  shaStrings <- branches$commit$sha
  
  return (shaStrings)
}




