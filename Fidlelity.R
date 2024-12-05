library(pdftools)
library(quanteda)
library(tm)
library(koRpus)
library(stringr)
text <- pdf_text("C1_P7-156_L1_Welcome-to-scikit-learn.pdf")
clean_text <- str_replace_all(text, "[\r\n]", " ") %>% str_squish()
word_lengths <- sapply(clean_text, function(txt) {
  words <- str_split(txt, "\\s+")[[1]]
  mean(nchar(words))
})

# 统计特殊字符
special_chars_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[[:punct:]]"))
})

# 统计大写字母
capital_letters_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[A-Z]"))
})

# 首字母缩写词数量
acronyms_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "\\b[A-Z]{2,}\\b"))
})

results1 <- data.frame(
  AverageWordLength = word_lengths,
  SpecialCharsCount = special_chars_count,
  CapitalLettersCount = capital_letters_count,
  AcronymsCount = acronyms_count
)





text <- pdf_text("C2_P157-202_L1_scikit-learn-Tutorials.pdf")
clean_text <- str_replace_all(text, "[\r\n]", " ") %>% str_squish()
word_lengths <- sapply(clean_text, function(txt) {
  words <- str_split(txt, "\\s+")[[1]]
  mean(nchar(words))
})

# 统计特殊字符
special_chars_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[[:punct:]]"))
})

# 统计大写字母
capital_letters_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[A-Z]"))
})

# 首字母缩写词数量
acronyms_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "\\b[A-Z]{2,}\\b"))
})

results2 <- data.frame(
  AverageWordLength = word_lengths,
  SpecialCharsCount = special_chars_count,
  CapitalLettersCount = capital_letters_count,
  AcronymsCount = acronyms_count
)




text <- pdf_text("C3_P203-668_L1_User-Guide.pdf")
clean_text <- str_replace_all(text, "[\r\n]", " ") %>% str_squish()
word_lengths <- sapply(clean_text, function(txt) {
  words <- str_split(txt, "\\s+")[[1]]
  mean(nchar(words))
})

# 统计特殊字符
special_chars_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[[:punct:]]"))
})

# 统计大写字母
capital_letters_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[A-Z]"))
})

# 首字母缩写词数量
acronyms_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "\\b[A-Z]{2,}\\b"))
})

results3 <- data.frame(
  AverageWordLength = word_lengths,
  SpecialCharsCount = special_chars_count,
  CapitalLettersCount = capital_letters_count,
  AcronymsCount = acronyms_count
)




text <- pdf_text("C4_P669-688_L1_Glossary-of-Common-Terms-and-API-Elements.pdf")
clean_text <- str_replace_all(text, "[\r\n]", " ") %>% str_squish()
word_lengths <- sapply(clean_text, function(txt) {
  words <- str_split(txt, "\\s+")[[1]]
  mean(nchar(words))
})

# 统计特殊字符
special_chars_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[[:punct:]]"))
})

# 统计大写字母
capital_letters_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[A-Z]"))
})

# 首字母缩写词数量
acronyms_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "\\b[A-Z]{2,}\\b"))
})

results4 <- data.frame(
  AverageWordLength = word_lengths,
  SpecialCharsCount = special_chars_count,
  CapitalLettersCount = capital_letters_count,
  AcronymsCount = acronyms_count
)




text <- pdf_text("C5_P689-1464_L1_Examples.pdf")
clean_text <- str_replace_all(text, "[\r\n]", " ") %>% str_squish()
word_lengths <- sapply(clean_text, function(txt) {
  words <- str_split(txt, "\\s+")[[1]]
  mean(nchar(words))
})

# 统计特殊字符
special_chars_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[[:punct:]]"))
})

# 统计大写字母
capital_letters_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[A-Z]"))
})

# 首字母缩写词数量
acronyms_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "\\b[A-Z]{2,}\\b"))
})

results5 <- data.frame(
  AverageWordLength = word_lengths,
  SpecialCharsCount = special_chars_count,
  CapitalLettersCount = capital_letters_count,
  AcronymsCount = acronyms_count
)



text <- pdf_text("C6_P1465-2412_L1_API-Reference.pdf")
clean_text <- str_replace_all(text, "[\r\n]", " ") %>% str_squish()
word_lengths <- sapply(clean_text, function(txt) {
  words <- str_split(txt, "\\s+")[[1]]
  mean(nchar(words))
})

# 统计特殊字符
special_chars_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[[:punct:]]"))
})

# 统计大写字母
capital_letters_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[A-Z]"))
})

# 首字母缩写词数量
acronyms_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "\\b[A-Z]{2,}\\b"))
})

results6 <- data.frame(
  AverageWordLength = word_lengths,
  SpecialCharsCount = special_chars_count,
  CapitalLettersCount = capital_letters_count,
  AcronymsCount = acronyms_count
)




text <- pdf_text("C7_P2413-2456_L1_Developer8217s-Guide.pdf")
clean_text <- str_replace_all(text, "[\r\n]", " ") %>% str_squish()
word_lengths <- sapply(clean_text, function(txt) {
  words <- str_split(txt, "\\s+")[[1]]
  mean(nchar(words))
})

# 统计特殊字符
special_chars_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[[:punct:]]"))
})

# 统计大写字母
capital_letters_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[A-Z]"))
})

# 首字母缩写词数量
acronyms_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "\\b[A-Z]{2,}\\b"))
})

results7 <- data.frame(
  AverageWordLength = word_lengths,
  SpecialCharsCount = special_chars_count,
  CapitalLettersCount = capital_letters_count,
  AcronymsCount = acronyms_count
)





text <- pdf_text("C8_P2457-2464_L1_Bibliography.pdf")
clean_text <- str_replace_all(text, "[\r\n]", " ") %>% str_squish()
word_lengths <- sapply(clean_text, function(txt) {
  words <- str_split(txt, "\\s+")[[1]]
  mean(nchar(words))
})

# 统计特殊字符
special_chars_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[[:punct:]]"))
})

# 统计大写字母
capital_letters_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[A-Z]"))
})

# 首字母缩写词数量
acronyms_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "\\b[A-Z]{2,}\\b"))
})

results8 <- data.frame(
  AverageWordLength = word_lengths,
  SpecialCharsCount = special_chars_count,
  CapitalLettersCount = capital_letters_count,
  AcronymsCount = acronyms_count
)





text <- pdf_text("C9_P2465-2503_L1_Index.pdf")
clean_text <- str_replace_all(text, "[\r\n]", " ") %>% str_squish()
word_lengths <- sapply(clean_text, function(txt) {
  words <- str_split(txt, "\\s+")[[1]]
  mean(nchar(words))
})

# 统计特殊字符
special_chars_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[[:punct:]]"))
})

# 统计大写字母
capital_letters_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "[A-Z]"))
})

# 首字母缩写词数量
acronyms_count <- sapply(clean_text, function(txt) {
  sum(str_count(txt, "\\b[A-Z]{2,}\\b"))
})

results9 <- data.frame(
  AverageWordLength = word_lengths,
  SpecialCharsCount = special_chars_count,
  CapitalLettersCount = capital_letters_count,
  AcronymsCount = acronyms_count
)

