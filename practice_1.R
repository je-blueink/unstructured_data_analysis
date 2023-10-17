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

# ---- 5장 ----
rm(list = ls())

install.packages("tidytext")
install.packages("wordcloud")

library(dplyr)
library(textstem)
library(stopwords)

lw_text <- scan("https://www.gutenberg.org/cache/epub/514/pg514.txt",
                what = "character", encoding = "UTF-8", sep = "\n") #가져오기
lw_part <- grep(lw_text, pattern = "PART") 
lw_end <- grep(lw_text, pattern = "END OF THE PROJECT GUTENBERG EBOOK")-2
lw_body <- lw_text[(lw_part[3]):lw_end] #본문 추출
lw_body %>% paste(collapse = " ") %>% tolower() %>%  #이어 붙이고 대문자 변환
  gsub(pattern = "’s", replacement="") %>%  #'s 삭제
  gsub(pattern = "([^[:alnum:][:blank:]’-])", replacement = "") #문장부호 제거

lw_token <- unlist(strsplit(lw_body, " ")) #토큰화
lw_lemma <- lemmatize_strings(lw_token) #원형복원
lw_words <- lw_lemma[!lw_lemma %in% c(stopwords(), "")] #불용어 삭제(단어 벡터)
lw_table <- sort(table(lw_words), decreasing = T) #단어 도수분포표 만들기
lw_words
lw_table

library(tidytext)
bingsent <- get_sentiments("bing") # 감성 어휘 사전
bingpos <- bingsent[bingsent$sentiment=="positive",] #긍정 어휘 추출
bingneg <- bingsent[bingsent$sentiment=="negative",] #부정 어휘 추출

lw_sent <- ifelse(lw_words %in% bingpos$word, 1, 
                  ifelse(lw_words %in% bingneg$word, -1, 0)) #단어별 감성 표시
head(lw_sent)
barplot(tapply(lw_sent, (seq_along(lw_sent)-1) %/% 1000, sum))