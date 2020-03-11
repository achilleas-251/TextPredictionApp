source("functions/process_input.R")

load("data/ngrams.RData")

next_word_bigram <- function(text){
  
  text <- process_input(text)
  
  l <- length(strsplit(text, " ")[[1]])
  
  if (l==0){
    return("...")
  } else {
    s <- tail(text, 1)
    y <- grep(paste0("^", s, " "), bigrams, value = T)
    nextWord <- gsub("^i$", "I", word(y[1],-1))
    if (!is.na(nextWord)){
      return(nextWord)
    } else{
      return("...")
    }
    }

}

next_word_trigram <- function(text){
  
  text <- process_input(text)
  
  l <- length(strsplit(text, " ")[[1]])
  
  if (l<2){
    next_word_bigram(text)
  } else {
    s <- tail(text, 2)
    y <- grep(paste0("^", s, " "), trigrams, value = T)
    nextWord <- gsub("^i$", "I", word(y[1],-1))
    if (!is.na(nextWord)){
      return(nextWord)
    } else{
      next_word_bigram(text)
    }
    }

}

next_word_tetragram <- function(text){
  
  text <- process_input(text)
  
  l <- length(strsplit(text, " ")[[1]])
  
  if (l<3){
    if (l==2){
      next_word_trigram(text)
    } else{next_word_bigram(text)}
  } else {
    s <- tail(text, 3)
    y <- grep(paste0("^", s, " "), tetragrams, value = T)
    nextWord <- gsub("^i$", "I", word(y[1],-1))
    if (!is.na(nextWord)){
      return(nextWord)
    } else{
      next_word_trigram(text)
    }
  }
  
}

next_word_pentagram <- function(text){
  
  text <- process_input(text)
  
  l <- length(strsplit(text, " ")[[1]])
  
  if (l<4){
    if (l==3){
      next_word_tetragram(text)
    } else if (l==2) {
      next_word_trigram(text)
    } else{next_word_bigram(text)}
  } else {
    s <- tail(text, 4)
    y <- grep(paste0("^", s, " "), pentagrams, value = T)
    nextWord <- gsub("^i$", "I", word(y[1],-1))
    if (!is.na(nextWord)){
      return(nextWord)
    } else{
      next_word_tetragram(text)
    }
  }
  
}

tail <- function(text, n){
  
  if (text!=""){
    x <- paste(word(text, seq(-n, -1, 1)), collapse = " ")
    return(x)
  } else if (text==""){
      return("")
    }
}
