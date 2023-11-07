rm(list = ls())
install.packages("rvest")

library(rvest)
library(dplyr)

exp_url <- "https://www.mcctcarbide.com/ko/tungsten-carbide/"
exp_html <- read_html(exp_url, encoding = "UTF-8")
exp_html %>% html_nodes(".content p") %>% html_text()