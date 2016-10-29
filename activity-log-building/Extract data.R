library(jsonlite)
library(httr)

#set up the authentication 
authenticate <- authenticate("jonaslieben","test123")

branchesJSON <- GET("https://api.github.com/repos/twitter/twitter-server/branches", authenticate)
