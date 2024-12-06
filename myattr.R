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

analyze_pdf <- function(filename){
  
  #parse pdf and put into one big thing
  pdf_text <- pdf_text(filename)
  text <- paste(pdf_text, collapse = " ")
  text_clean <- gsub("[[:punct:]]", "", text)
  text_clean <- gsub("[[:digit:]]", "", text_clean)
  words <- strsplit(text_clean, "\\s+")[[1]]
  word_lengths <- nchar(words)
  
  #get average word length
  average_word_length <- mean(word_lengths, na.rm = TRUE)
  
  #get ttr
  total_tokens <- length(words)
  unique_types <- length(unique(words))
  ttr <- unique_types / total_tokens
  
  #get number of pages
  page_number <- length(pdf_text)
  
  #get reading scores
  readability_scores <- textstat_readability(text, measure = "Flesch.Kincaid")
  
  #find ratio of special characters to regular characters
  special_characters <- gsub("[[:alnum:][:space:]]", "", text)
  num_special_characters <- nchar(special_characters)
  num_regular_characters <- nchar(gsub("[^[:alnum:]]", "", text))
  ratio_special_to_regular <- num_special_characters / num_regular_characters

  #find number of acronyms
  acronyms <- words[sapply(words, is_acronym)]
  acronyms <- acronyms[nchar(acronyms) > 1]
  is_english_word <- hunspell_check(acronyms)
  acronyms <- acronyms[!is_english_word]
  prop_acronym <- length(acronyms)/length(words)
  
  result <- data.frame(
    filename = filename,
    pages = page_number,
    ttr = ttr,
    readability = readability_scores,
    avg_word_length = average_word_length,
    prop_special_char = ratio_special_to_regular,
    prop_acronym = prop_acronym
  )
  
  return(result)
}
#doc_set <- c("nbafinancial.pdf","nbaschedule.pdf","nbarules2023.pdf","nbarules2022.pdf","nbarules2021.pdf","nbabargaining.pdf")
#scikit_docs <- c("welcome.pdf","tutorial.pdf","guide.pdf","glossary.pdf","ex.pdf","API.pdf")


#=======================================================================================================================
#UNCOMMENT THE BELOW LINE AND ADD YOUR FILENAME(S)
#doc_set <- c("your.pdf", "docs.pdf")

stats <- analyze_pdf(doc_set[1])

if(length(doc_set) >1){
  for(i in 2:length(doc_set)){
    tmp <- analyze_pdf(doc_set[i])
    stats <- rbind(stats, tmp)
  }
}

stats <- stats |>
  rename("readability" = `readability.Flesch.Kincaid`) |>
  select(-`readability.document`)

df <- stats |> select(-filename)
max_values <- apply(df, 2, max)
mean_values <- apply(df, 2, mean)

rm(tmp, df)

summary_df <- data.frame(
  rbind(max_values, mean_values)
)

stats
summary_df

