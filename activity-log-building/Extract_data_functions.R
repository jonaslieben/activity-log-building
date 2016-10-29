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

retrieveAllCommitIdentifiers <- function(authentication, owner, repository) {
  #retrieve all identifiers of a branch
  branchIdentifiers <- retrieveIdentifiersBranches(authentication, owner, repository)
  
  for(branchIdentifier in branchIdentifiers) {
    commitIdentifiers <- c()
    # keep an index for the pages as paging is used and only 100 commits fit onto one page
    index <- 1
    # retrieve all commit identifiers of one branch
    repeat {
      url <- paste("https://api.github.com/repos/",owner,"/", repository, "/commits", sep ="")
      # do the API call for getting all the 
      commitJSON <- GET(url,query = list(per_page = 100, sha = branchIdentifier, page = index), authentication)
      # convert the JSON to R objects
      commitData <- fromJSON(content(commitJSON, "text"))
      index = index + 1
      # if there are no items anymore, stop the loop
      if(length(commitData$sha) == 0) {
        break
      } else {
        #add the identifiers to the list
        commitIdentifiersNew <- c(commitIdentifiers, commitData$sha)
        commitIdentifiers <- commitIdentifiersNew
      }
    }
  }
  
  return(commitIdentifiers)
}
