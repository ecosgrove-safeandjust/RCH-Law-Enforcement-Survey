#### CODE FOR VALUE LABELS FROM CENTIMENT

rm(list = ls())

library(haven)
library(labelled)
library(tidyverse)
library(tidylog)
library(janitor)
library(stringi)
library(data.table)
library(ggplot2)
library(scales)

setwd("~/Documents/GitHub/RCH-Law-Enforcement-Survey")

options(dplyr.print_max = 1e9)

data <- read_sav("input/Law-enforcement-survey-character.sav")

data <- clean_names(data)
data_labels <- clean_names(data_labels)

questions <- sapply(data, attr, "label")
questions <- as.data.frame(questions)
questions <- cbind(names(data), questions)
questions <- questions[1:2]
colnames(questions) <- c("variable", "question")
write.csv(questions, "output/questions as a list.csv")



tabs <- function(data, v1) {
  total <- nrow(data) 
  
  
  df <- data %>%
    group_by(as_factor({{v1}})) %>%
    summarise(n_cases = n()) |>
    mutate(pct = n_cases/total
    )
  
  return(df)
  
}

#le_type <- tabs(data, q4)


ttabs <- function(data, v1) {
  total <- nrow(data) 
  
  
  df <- data %>%
    group_by(as_factor({{v1}})) %>%
    summarise(n_cases = n()) |>
    mutate(pct = n_cases/total
    ) %>%
    arrange(-pct)
  
  df <- df %>%
    mutate(descr = paste0(sub("^.*\\] ?", "", attributes(df[[1]])$label), " "))
  
  
  colnames(df) <- c("question", "n_cases", "pct", "descr")
  
  return(df)
  
}

#le_type <- ttabs(data, q4)


crosstabs <- function (data, v1, v2) {
  
  total_df <- data %>%
    group_by(as_factor({{v2}})) %>%
    summarise(total = n())
  
  df <- data %>%
    group_by(as_factor({{v1}}), as_factor({{v2}})) %>%
    summarise(n_cases = n()) %>%
    left_join(total_df) %>%
    mutate(pct = n_cases/total)
  
  return(df)
  
}

#test <- crosstabs(data, q4, q57)
# 
# 
# ccrosstabs <- function (data, v1, v2) {
#   
#   var2 <- as.character(substitute(v2))
#   
#   labels <- data %>%
#     group_by({{v1}}, {{v2}}) %>%
#     summarise(n_from_data = n()) %>%
#     mutate(answer = as.character(as_factor({{v1}})),
#            subcategory = as.character(as_factor({{v2}})),
#            q = var2) %>%
#     select(1,2,4,5, 6)
#   
#   colnames(labels) <- c("v1", "v2", "answer", "subcategory", "q")
#   
#   
#   total_df <- data %>%
#     group_by(({{v2}})) %>%
#     summarise(total = n())
#   
#   df <- data %>%
#     group_by(({{v1}}), ({{v2}})) %>%
#     summarise(n_cases = n(),
#               weighted_subtotal = sum(wt)) %>%
#     left_join(weighted_total_df) %>%
#     mutate(pct = weighted_subtotal/weighted_total) %>%
#     arrange(-pct)
#   
#   
#   colnames(df) <- c("v1", "v2", "n_cases", "weighted_subtotal", "weighted_total", "pct")
#   
#   df <- left_join(df, labels) %>%
#     select("answer", "subcategory", "pct", "weighted_subtotal", "weighted_total", "n_cases", "q")
#   
#   return(df)
#   
# }
# 
# 
# ccrosstabs <- function (data, v1, v2) {
#   
#   var2 <- as.character(substitute(v2))
#   
#   labels <- data %>%
#     group_by({{v1}}, {{v2}}) %>%
#     summarise(n_from_data = n()) %>%
#     mutate(answer = as.character(as_factor({{v1}})),
#            subcategory = as.character(as_factor({{v2}})),
#            q = var2) %>%
#     select(1,2,4,5, 6)
#   
#   colnames(labels) <- c("v1", "v2", "answer", "subcategory", "q")
#   
#   
#   weighted_total_df <- data %>%
#     group_by(({{v2}})) %>%
#     summarise(weighted_total = sum(wt))
#   
#   df <- data %>%
#     group_by(({{v1}}), ({{v2}})) %>%
#     summarise(n_cases = n(),
#               weighted_subtotal = sum(wt)) %>%
#     left_join(weighted_total_df) %>%
#     mutate(pct = weighted_subtotal/weighted_total) %>%
#     arrange(-pct)
#   
#   
#   colnames(df) <- c("v1", "v2", "n_cases", "weighted_subtotal", "weighted_total", "pct")
#   
#   df <- left_join(df, labels) %>%
#     select("answer", "subcategory", "pct", "weighted_subtotal", "weighted_total", "n_cases", "q")
#   
#   return(df)
#   
# }
