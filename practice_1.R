# ---- 4장 ----

# 1
lw_text <- scan("https://www.gutenberg.org/cache/epub/514/pg514.txt",
                what = "character", encoding = "UTF-8", sep = "\n")
head(lw_text)
lw_part <- grep(lw_text, pattern = "PART")
lw_part
lw_end <- grep(lw_text, pattern = "END OF THE PROJECT GUTENBERG EBOOK")-2
lw_body <- lw_text[(lw_part[3]):lw_end]
head(lw_body)
tail(lw_body)

lw_all <- paste(lw_body, collapse = " ")
lw_all

# 2
lw_lower <- tolower(lw_all)

# 3
lw_lower <- gsub(lw_lower, pattern = "’s", replacement="")
lw_lower <- gsub(lw_lower, pattern = "([^[:alnum:][:blank:]’-])", 
                 replacement = "")
lw_token <- unlist(strsplit(lw_lower, " "))
lw_token

# 4
install.packages("textstem")
library(textstem)

lw_lemma <- lemmatize_strings(lw_token)
lw_lemma

# 5
install.packages("stopwords")
library(stopwords)

lw_stwd <- lw_lemma[!lw_lemma %in% c(stopwords(), "")]
lw_stwd