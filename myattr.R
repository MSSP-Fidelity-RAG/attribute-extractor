#you must pull the repository from GitHub, or else you have to set up the file system yourself with the "docs" folder with aaa.txt in it 
#put all of your files into the docs folder then just click run
#the two dataframes created will contain all the stats you need to fill into the google sheet
#make sure you install all necessary packages

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
  text_total <- ""
  
  #initialize vectors for maxs
  page_number_max <- numeric(100)
  ttr_max <- numeric(100)
  readability_max <- numeric(100)
  special_char_max <- numeric(100)
  acronym_max <- numeric(100)
  i<-1
  # Loop through each file
  for (filename in files) {
    pdf_text <- pdf_text(filename)
    page_number <- page_number + length(pdf_text)
    text <- paste(pdf_text, collapse = " ")
    
    combined_text <- paste(pdf_text, collapse = " ")  # Collapse pages into one string
    text_total <- paste(text_total, combined_text, sep = " ")
    
    text_clean <- gsub("[[:punct:]]", "", text)
    text_clean <- gsub("[[:digit:]]", "", text_clean)
    words_file <- strsplit(text_clean, "\\s+")
    words_file <- unlist(words_file)
    
    #get attributes per file
    page_number_max[i] <- length(pdf_text)
    ttr_max[i] <- length(unique(words_file))/length(words_file)
    readability_result <- textstat_readability(text, measure = "Flesch.Kincaid")
    readability_max[i] <- unlist(readability_result$`Flesch.Kincaid`)
    
    print(filename)
    print(readability_max[i])
    #get acronym prop 
    acronyms <- words_file[sapply(words_file, is_acronym)]
    acronyms <- acronyms[nchar(acronyms) > 1]
    is_english_word <- hunspell_check(acronyms) 
    acronyms <- acronyms[!is_english_word] 
    length(acronyms)
    prop_acronym <- length(acronyms)/length(words_file)
    acronym_max[i] <- prop_acronym
    
    #get special character
    special_characters <- gsub("[[:alnum:][:space:]]", "", text)
    num_special_characters <- nchar(special_characters)
    num_regular_characters <- nchar(gsub("[^[:alnum:]]", "", text))
    ratio_special_to_regular <- num_special_characters / num_regular_characters
    special_char_max[i] <- ratio_special_to_regular
    
    words <- c(words, words_file)
    i<-i+1
  }
  # print(page_number_max[1:10])
  # print(ttr_max[1:10])
   print(readability_max[1:10])
  # print(acronym_max[1:10])
  # print(special_char_max[1:10])
  
  maxes <- data.frame(
    pages = max(page_number_max),
    ttr = max(ttr_max),
    readability <- max(readability_max),
    special_chars <- max(special_char_max),
    acronym <- max(acronym_max)
  )
  
  word_lengths <- nchar(words)
  #get ttr
  total_tokens <- length(words)
  unique_types <- length(unique(words))
  ttr <- unique_types / total_tokens
  
  #get reading scores
  readability_scores <- textstat_readability(text_total, measure = "Flesch.Kincaid")
  print(readability_scores)
  #find ratio of special characters to regular characters
  special_characters <- gsub("[[:alnum:][:space:]]", "", text_total)
  num_special_characters <- nchar(special_characters)
  num_regular_characters <- nchar(gsub("[^[:alnum:]]", "", text_total))
  ratio_special_to_regular <- num_special_characters / num_regular_characters

  acronyms <- words[sapply(words, is_acronym)]
  acronyms <- acronyms[nchar(acronyms) > 1]
  is_english_word <- hunspell_check(acronyms) 
  acronyms <- acronyms[!is_english_word] 
  length(acronyms)
  prop_acronym <- length(acronyms)/length(words)
  
  result <- data.frame(
    pages = page_number,
    ttr = ttr,
    readability = readability_scores,
    prop_special_char = ratio_special_to_regular,
    prop_acronym = prop_acronym
  )
  
  return(list(results = result, maxes = maxes))
}

#================================================================
results <- analyze_pdf()

doc_set_stats <- results$result
maxes <- results$maxes

doc_set_stats <- doc_set_stats |>
  rename("readability" = `readability.Flesch.Kincaid`) |>
  select(-`readability.document`)

colnames(maxes) <- c("max_pages", "max_ttr", "max_readability", "max_special_chars_prop", "max_acronym_prop")
rm(results)