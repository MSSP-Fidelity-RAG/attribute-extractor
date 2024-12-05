library(pdftools)
library(stringr)
library(hunspell)
library(quanteda)
library(quanteda.textstats)
library(dplyr)

is_roman_numeral <- function(x) {
  grepl("^(?i)M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$", x)
}

check_type <- function(string){
  return(hunspell_check(string))
}

# Function to process PDF and compute statistics
analyze_pdf <- function(filename) {
  # Read PDF text by page
  pdf_text <- pdf_text(filename)
  
  # Initialize vectors to store results
  avg_word_length <- numeric(length(pdf_text))
  avg_special_chars <- numeric(length(pdf_text))
  avg_acronyms <- numeric(length(pdf_text))
  
  # Process each page
  # for (i in seq_along(pdf_text)) {
  #   page_text <- pdf_text[i]
  #   
  #   words <- unlist(str_split(page_text, "\\s+"))
  #   words <- words[words != ""]
  # 
  #   avg_word_length[i] <- mean(nchar(words))
  #   
  #   special_chars <- str_extract_all(page_text, "[^a-zA-Z0-9\\s]")[[1]]
  #   avg_special_chars[i] <- length(special_chars)
  #   
  #   acronyms <- str_extract_all(page_text, "\\b[A-Z]{2,}\\b")[[1]]
  #   avg_acronyms[i] <- length(acronyms)
  # }

  avg_special_chars <- sum(avg_special_chars)/length(pdf_text)
  avg_acronyms <- sum(avg_acronyms)/length(pdf_text)
  
  text <- paste(pdf_text, collapse = " ")
  readability_scores <- textstat_readability(text, measure = "Flesch.Kincaid")
  
  tokens <- str_split(text, "\\s+")[[1]]
  unique_words <- length(unique(tokens))
  total_words <- length(tokens)           
  ttr <- unique_words / total_words 
  
  result <- data.frame(
    filename = filename,
    pages = length(pdf_text),
    ttr = ttr,
    readability = readability_scores,
    avg_word_length = mean(avg_word_length, na.rm = TRUE),
    avg_special_chars = mean(avg_special_chars, na.rm = TRUE),
    avg_acronyms = mean(avg_acronyms, na.rm = TRUE)
  )
  return(result)
}
doc_set <- c("nbafinancial.pdf","nbaschedule.pdf","nbarules2023.pdf","nbarules2022.pdf","nbarules2021.pdf","nbabargaining.pdf")
scikit_docs <- c("welcome.pdf","tutorial.pdf","guide.pdf","glossary.pdf","ex.pdf","API.pdf")

#UNCOMMENT THE BELOW LINE AND ADD YOUR FILENAME
#doc_set <- c("your_filename.pdf")

stats <- analyze_pdf(doc_set[1])
for(i in 2:length(doc_set)){
  tmp <- analyze_pdf(doc_set[i])
  stats <- rbind(stats, tmp)
}

stats <- stats |>
  rename("readability" = `readability.Flesch.Kincaid`) |>
  select(-`readability.document`)

stats


pdf_text <- pdf_text("API.pdf")
text <- paste(pdf_text, collapse = " ")
text_clean <- gsub("[[:punct:]]", "", text)
text_clean <- gsub("[[:digit:]]", "", text_clean)
words <- strsplit(text_clean, "\\s+")[[1]]
word_lengths <- nchar(words)
average_word_length <- mean(word_lengths, na.rm = TRUE)
max_word_length <- max(word_lengths, na.rm = TRUE)







result <- data.frame(
  filename = filename,
  pages = length(pdf_text),
  ttr = ttr,
  readability = readability_scores,
  avg_word_length = mean(avg_word_length, na.rm = TRUE),
  avg_special_chars = mean(avg_special_chars, na.rm = TRUE),
  avg_acronyms = mean(avg_acronyms, na.rm = TRUE)
)



