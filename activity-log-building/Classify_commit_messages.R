loadClassificationScheme <- function() {
  # Load the classification dictionary from the paper of Andreas Mauczka, Markus Huber Christian Schanes, Wolfgang Schramm, Mario Bernhart, and Thomas Grechenig
  # title: Tracing Your Maintenance Work – A Cross-Project Validation of an Automated Classification Dictionary for Commit Messages
  # year of publication: 2012
  corrective <- c("active", "against", "already", "bad", "block", "bug", "build", "call", "case", "catch",  "character", "compile", "correctly", "create", "different", "dump", "except", "exist", "explicitly", "fail", "fast", "format", "good", "hack", "hard", "help", "init", "instead", "introduce", "issue", "lock", "log", "logic", "look", "merge", "operation" , "pass", "previous", "previously", "probably", "problem", "properly", "random", "recent", "request", "reset", "review", "run", "safe", "set", "similar", "simplify", "special", "test", "think", "try", "turn", "valid", "wait", "warning")
  correctiveStrong <- c("warn", "wrong", "cause", "error", "failure", "fix", "miss", "null", "oops")
  adaptive <- c("active", "against", "already", "bad", "behavior", "block", "build", "call", "case", "catch", "character", "compile",  "correctly", "create", "different", "dump", "except", "exist", "explicitly", "fail", "fast", "format", "good", "hack", "hard", "header", "help", "include", "init", "inline", "instead", "introduce", "issue", "lock", "log", "logic", "look", "merge", "operation", "operations", "pass",  "previous", "previously", "probably", "properly", "random", "recent", "request", "reset", "review", "run", "safe", "set", "similar", "simplify", "special", "test", "think",  "try", "turn", "valid", "wait")
  adaptiveStrong <- c("add", "additional", "appropriate", "available", "change", "compatibility", "config", "configuration", "context", "currently", "default", "documentation", "easier", "feature", "future", "information", "install", "internal", "method", "necessary", "new", "old", "patch", "protocol", "provide", "release", "replace", "require", "security", "simple", "structure", "switch", "text", "trunk", "useful", "user", "version")
  perfectiveStrong <- c("cleanup", "consistent", "declaration", "definition", "move", "prototype", "removal", "static", "style", "unused", "variable", "whitespace")
  perfective <- c("header", "include", "inline", "warning")
  
  #create corpus
  corpus <- Corpus(DataframeSource(data.frame(corrective)))
  #stem words
  corpus.temp <- tm_map(corpus, stemDocument, language = "english")
  #replace each word by its stem. This means that every word becomes the stem. For example adds becomes add 
  for(i in 1:length(corpus)) {
    corrective[i] <- corpus.temp[[i]]$content
  }
  
  #create corpus
  corpus <- Corpus(DataframeSource(data.frame(correctiveStrong)))
  #stem words
  corpus.temp <- tm_map(corpus, stemDocument, language = "english")
  #replace each word by its stem. This means that every word becomes the stem. For example adds becomes add 
  for(i in 1:length(corpus)) {
    correctiveStrong[i] <- corpus.temp[[i]]$content
  }
  
  #create corpus
  corpus <- Corpus(DataframeSource(data.frame(adaptive)))
  #stem words
  corpus.temp <- tm_map(corpus, stemDocument, language = "english")
  #replace each word by its stem. This means that every word becomes the stem. For example adds becomes add 
  for(i in 1:length(corpus)) {
    adaptive[i] <- corpus.temp[[i]]$content
  }
  
  #create corpus
  corpus <- Corpus(DataframeSource(data.frame(adaptiveStrong)))
  #stem words
  corpus.temp <- tm_map(corpus, stemDocument, language = "english")
  #replace each word by its stem. This means that every word becomes the stem. For example adds becomes add 
  for(i in 1:length(corpus)) {
    adaptiveStrong[i] <- corpus.temp[[i]]$content
  }
  
  #create corpus
  corpus <- Corpus(DataframeSource(data.frame(perfective)))
  #stem words
  corpus.temp <- tm_map(corpus, stemDocument, language = "english")
  #replace each word by its stem. This means that every word becomes the stem. For example adds becomes add
  for(i in 1:length(corpus)) {
    perfective[i] <- corpus.temp[[i]]$content
  }
  
  #create corpus
  corpus <- Corpus(DataframeSource(data.frame(perfectiveStrong)))
  #stem words
  corpus.temp <- tm_map(corpus, stemDocument, language = "english")
  #replace each word by its stem. This means that every word becomes the stem. For example adds becomes add
  for(i in 1:length(corpus)) {
    perfectiveStrong[i] <- corpus.temp[[i]]$content
  }
  
  #save everything in one variable which needs to be returned
  classificationScheme <- c(adaptive, adaptiveStrong, corrective, correctiveStrong, perfective, perfectiveStrong)
  return (classificationScheme)
}

preprocessingNlp <- function(eventData) {
  
  #create a dplyr table
  eventDataTable <- tbl_df(eventData)
  
  #convert the factor variable to a character variable
  eventDataTable$message <- as.character(eventDataTable$message)
  
  #remove \n from the text
  for(i in 1:length(eventDataTable$message)) {
    eventDataTable$message[i] <- gsub("\n", " ", eventDataTable$message[i])
  }
  
  #read the corpus in, in order to do all the processing steps below
  corpus <- Corpus(DataframeSource(data.frame(eventDataTable$message)))
  #stemming the text. This means that every word becomes the stem. For example adds becomes add
  corpus.temp <- tm_map(corpus, stemDocument, language = "english")
  #remove the unnecessary whitespaces
  corpus.temp<-tm_map(corpus.temp, stripWhitespace)
  #remove stopwords
  corpus.temp<-tm_map(corpus.temp, removeWords, stopwords(kind = "en"))
  for(i in 1:length(corpus)) {
    #replace the result of the steps before with t
    eventDataTable$message[i] <- corpus.temp[[i]]$content
    #change everything to lower case letters
    eventDataTable$message[i] <- tolower(eventDataTable$message[i])
  }
  return (eventDataTable)
}

countAmountOfWordsInMessage <- function(message,type) {
  #initialise the variable sum and assign zero
  sum <- 0
  # for each word in the table type
  # count the amount of matches between each word of type and the message
  for(i in 1:length(type)) {
    sum <- sum + sum(str_count(message, type[i]))
  }
  # return the type
  return(sum)
}

countAccordingToClassificationScheme <- function(eventDataTable, classificationScheme) {
  #load the classification dictionarry
  adaptive <- classificationScheme[1]
  adaptiveStrong <- classificationScheme[2]
  corrective <- classificationScheme[3]
  correctiveStrong <- classificationScheme[4]
  perfective <- classificationScheme[5]
  perfectiveStrong <- classificationScheme[6]
  #create new columns for counting the amount of adaptive, corrective and perfective words
  eventDataTable["adaptive"] <- 0
  eventDataTable["corrective"] <- 0
  eventDataTable["perfective"] <- 0
  #count the words with the formula amount of words "normal" + 2 * amount of words "strong"
  for(i in 1:length(eventDataTable$message)) {
    eventDataTable$adaptive[i] <- countAmountOfWordsInMessage(eventDataTable$message[i], adaptive) + 2 * countAmountOfWordsInMessage(eventDataTable$message[i], adaptiveStrong)
    eventDataTable$corrective[i] <- countAmountOfWordsInMessage(eventDataTable$message[i], corrective) + 2 * countAmountOfWordsInMessage(eventDataTable$message[i], correctiveStrong)
    eventDataTable$perfective[i] <- countAmountOfWordsInMessage(eventDataTable$message[i], perfective) + 2 * countAmountOfWordsInMessage(eventDataTable$message[i], perfectiveStrong)
  }
  return (eventDataTable)
}

classifyCommit <- function(eventDataTable) {
  #create a new column with the name type
  eventDataTable["type"] <- ""
  #for each message, check which one has the highest count value and classify the message according to these count values
  #if there are two or more values which are highest, unknown is assigned to the type
  for(i in 1:length(eventDataTable$message)) {
    if((eventDataTable$adaptive[i] > eventDataTable$corrective[i]) && (eventDataTable$adaptive[i] > eventDataTable$perfective[i])) {
      eventDataTable$type[i] <- "adaptive"
    } else if ((eventDataTable$corrective[i] > eventDataTable$perfective[i]) && (eventDataTable$corrective[i] > eventDataTable$adaptive[i])) {
      eventDataTable$type[i] <- "corrective"
    } else if ((eventDataTable$perfective[i] > eventDataTable$adaptive[i]) && (eventDataTable$perfective[i] > eventDataTable$corrective[i])) {
      eventDataTable$type[i] <- "perfective"
    } else {
      eventDataTable$type[i] <- "unknown"
    }
  }
  #return the event data table
  return (eventDataTable)
}

