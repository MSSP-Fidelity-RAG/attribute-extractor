#PUT YOUR DOCUMENTS IN THE DOCS FOLDER
#PUT YOUR DOCUMENTS IN THE DOCS FOLDER
#PUT YOUR DOCUMENTS IN THE DOCS FOLDER
#PUT YOUR DOCUMENTS IN THE DOCS FOLDER
#PUT YOUR DOCUMENTS IN THE DOCS FOLDER

library(pdftools)
library(stringr)
library(hunspell)
library(quanteda)
library(quanteda.textstats)
library(dplyr)
library(words)

is_acronym <- function(word) {
  return(grepl("^[A-Z]+$", word))
}

analyze_pdf <- function(){
  
  
  files <- list.files("./docs", full.names = TRUE)
  string_to_remove <- "./docs/aaa.txt"
  files <- files[files != string_to_remove]
  
  page_number <- 0
  words <- character(0)
  
  #initialize vectors for maxs
  page_number_max <- numeric(5)
  ttr_max <- numeric(5)
  readability_max <- numeric(5)
  special_char_max <- numeric(5)
  acronym_max <- numeric(5)
  
  # Loop through each file
  for (filename in files) {
    i<-1
    pdf_text <- pdf_text(filename)
    page_number <- page_number + length(pdf_text)
    text <- paste(pdf_text, collapse = " ")
    text_clean <- gsub("[[:punct:]]", "", text)
    text_clean <- gsub("[[:digit:]]", "", text_clean)
    words_file <- strsplit(text_clean, "\\s+")
    
    #get attributes per file
    page_number_max[i] <- length(words_file)
    ttr_max[i] <- length(unique(words_file))/length(words_file)
    readability <- textstat_readability(text, measure = "Flesch.Kincaid")
    
    
    
    
    words <- c(words, words_file)
    i<-i+1
  }
  
  
  
  # pdf_text <- pdf_text("nbarules2021.pdf")
  #parse pdf and put into one big thing
  # pdf_text <- pdf_text(filename)
  # text <- paste(pdf_text, collapse = " ")
  # text_clean <- gsub("[[:punct:]]", "", text)
  # text_clean <- gsub("[[:digit:]]", "", text_clean)
  # words <- strsplit(text_clean, "\\s+")[[1]]
  word_lengths <- nchar(words)
  
  #get ttr
  total_tokens <- length(words)
  unique_types <- length(unique(words))
  ttr <- unique_types / total_tokens
  
  #get reading scores
  readability_scores <- textstat_readability(text, measure = "Flesch.Kincaid")
  
  #find ratio of special characters to regular characters
  special_characters <- gsub("[[:alnum:][:space:]]", "", text)
  num_special_characters <- nchar(special_characters)
  num_regular_characters <- nchar(gsub("[^[:alnum:]]", "", text))
  ratio_special_to_regular <- num_special_characters / num_regular_characters

  #find number of acronyms
  acronyms <- sapply(words, is_acronym)
  acronyms <- acronyms[nchar(acronyms) > 1]
  is_english_word <- hunspell_check(acronyms)
  acronyms_char <- as.character(acronyms)
  acronyms <- acronyms[!is_english_word]
  prop_acronym <- length(acronyms)/length(words)
  
  result <- data.frame(
    pages = page_number,
    ttr = ttr,
    readability = readability_scores,
    prop_special_char = ratio_special_to_regular,
    prop_acronym = prop_acronym
  )
  
  return(result)
}

#================================================================
stats <- analyze_pdf()

stats <- stats |>
  rename("readability" = `readability.Flesch.Kincaid`) |>
  select(-`readability.document`)

stats

