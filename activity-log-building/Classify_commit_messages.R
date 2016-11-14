loadClassificationScheme <- function() {
  corrective <- c("active", "against", "already", "bad", "block", "bug", "build", "call", "case", "catch",  "character", "compile", "correctly", "create", "different", "dump", "except", "exist", "explicitly", "fail", "fast", "format", "good", "hack", "hard", "help", "init", "instead", "introduce", "issue", "lock", "log", "logic", "look", "merge", "operation" , "pass", "previous", "previously", "probably", "problem", "properly", "random", "recent", "request", "reset", "review", "run", "safe", "set", "similar", "simplify", "special", "test", "think", "try", "turn", "valid", "wait", "warning")
  correctiveStrong <- c("warn", "wrong", "cause", "error", "failure", "fix", "miss", "null", "oops")
  adaptive <- c("active", "against", "already", "bad", "behavior", "block", "build", "call", "case", "catch", "character", "compile",  "correctly", "create", "different", "dump", "except", "exist", "explicitly", "fail", "fast", "format", "good", "hack", "hard", "header", "help", "include", "init", "inline", "instead", "introduce", "issue", "lock", "log", "logic", "look", "merge", "operation", "operations", "pass",  "previous", "previously", "probably", "properly", "random", "recent", "request", "reset", "review", "run", "safe", "set", "similar", "simplify", "special", "test", "think",  "try", "turn", "valid", "wait")
  adaptiveStrong <- c("add", "additional", "appropriate", "available", "change", "compatibility", "config", "configuration", "context", "currently", "default", "documentation", "easier", "feature", "future", "information", "install", "internal", "method", "necessary", "new", "old", "patch", "protocol", "provide", "release", "replace", "require", "security", "simple", "structure", "switch", "text", "trunk", "useful", "user", "version")
  perfectiveStrong <- c("cleanup", "consistent", "declaration", "definition", "move", "prototype", "removal", "static", "style", "unused", "variable", "whitespace")
  perfective <- c("header", "include", "inline", "warning")
  
  corpus <- Corpus(DataframeSource(data.frame(corrective)))
  corpus.temp <- tm_map(corpus, stemDocument, language = "english")
  for(i in 1:length(corpus)) {
    corrective[i] <- corpus.temp[[i]]$content
  }
  
  corpus <- Corpus(DataframeSource(data.frame(correctiveStrong)))
  corpus.temp <- tm_map(corpus, stemDocument, language = "english")
  for(i in 1:length(corpus)) {
    correctiveStrong[i] <- corpus.temp[[i]]$content
  }
  
  corpus <- Corpus(DataframeSource(data.frame(adaptive)))
  corpus.temp <- tm_map(corpus, stemDocument, language = "english")
  for(i in 1:length(corpus)) {
    adaptive[i] <- corpus.temp[[i]]$content
  }
  
  corpus <- Corpus(DataframeSource(data.frame(adaptiveStrong)))
  corpus.temp <- tm_map(corpus, stemDocument, language = "english")
  for(i in 1:length(corpus)) {
    adaptiveStrong[i] <- corpus.temp[[i]]$content
  }
  
  corpus <- Corpus(DataframeSource(data.frame(perfective)))
  corpus.temp <- tm_map(corpus, stemDocument, language = "english")
  for(i in 1:length(corpus)) {
    perfective[i] <- corpus.temp[[i]]$content
  }
  
  corpus <- Corpus(DataframeSource(data.frame(perfectiveStrong)))
  corpus.temp <- tm_map(corpus, stemDocument, language = "english")
  for(i in 1:length(corpus)) {
    perfectiveStrong[i] <- corpus.temp[[i]]$content
  }
  
  classificationScheme <- c(adaptive, adaptiveStrong, corrective, correctiveStrong, perfective, perfectiveStrong)
  return (classificationScheme)
}

preprocessingNlp <- function(eventData) {
  
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
    eventDataTable$message[i] <- corpus.temp[[i]]$content
    #change everything to lower case letters
    eventDataTable$message[i] <- tolower(eventDataTable$message[i])
  }
  return (eventDataTable)
}

countAmountOfWordsInMessage <- function(message,type) {
  sum <- 0
  for(i in 1:length(type)) {
    sum <- sum + sum(str_count(message, type[i]))
  }
  return(sum)
}

countAccordingToClassificationScheme <- function(eventDataTable, classificationScheme) {
  adaptive <- classificationScheme[1]
  adaptiveStrong <- classificationScheme[2]
  corrective <- classificationScheme[3]
  correctiveStrong <- classificationScheme[4]
  perfective <- classificationScheme[5]
  perfectiveStrong <- classificationScheme[6]
  eventDataTable["adaptive"] <- 0
  eventDataTable["corrective"] <- 0
  eventDataTable["perfective"] <- 0
  for(i in 1:length(eventDataTable$message)) {
    eventDataTable$adaptive[i] <- countAmountOfWordsInMessage(eventDataTable$message[i], adaptive) + 2 * countAmountOfWordsInMessage(eventDataTable$message[i], adaptiveStrong)
    eventDataTable$corrective[i] <- countAmountOfWordsInMessage(eventDataTable$message[i], corrective) + 2 * countAmountOfWordsInMessage(eventDataTable$message[i], correctiveStrong)
    eventDataTable$perfective[i] <- countAmountOfWordsInMessage(eventDataTable$message[i], perfective) + 2 * countAmountOfWordsInMessage(eventDataTable$message[i], perfectiveStrong)
  }
  return (eventDataTable)
}

classifyCommit <- function(eventDataTable) {
  eventDataTable["type"] <- ""
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
  return (eventDataTable)
}

