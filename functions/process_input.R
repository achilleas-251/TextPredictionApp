library(dplyr)
library(stringr)
library(textclean)
library(tidytext)
library(tm)

load("data/vocabulary.Rdata")
load("data/contractions.Rdata")

process_input <- function(text){
  
  text <- replace_ordinal(text)
  text <- gsub("[^'[:alnum:] ]", "", text)
  text <- removePunctuation(text, preserve_intra_word_contractions = T)
  text <- edit_contractions(text)
  text <- tolower(text) 
  text <- out_of_vocabulary(text)
  text <- str_squish(text)
  
  return(text)
  
}


out_of_vocabulary <- function(text, v = vocabulary, y = " "){
  
  word_dataset <-
    tibble(line = 1:length(text), text = text) %>%
    unnest_tokens(word, text) %>%
    count(word, sort = T)
  
  words <- word_dataset$word
  
  remove(word_dataset)
  
  oov <- words[which(!(words %in% v))]
  
  remove(words)
  
  x <- paste0('(?<= |^)', oov, '(?= |$)')
  
  text <- mgsub_regex(text, x, y, perl = T)
  
  return(text)
  
}

edit_contractions <- function(text, x = contractions$contraction,
                              y = contractions$expansion){
  
  
  x <- paste0('(?<!\\w)', x, '(?!\\w)')
  
  text <- mgsub_regex(text, x, y, perl = T, ignore.case = T)
  
  
  return(text)
  
}