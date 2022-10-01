# Processing Text of Sustainability Reports

library(pdftools)
library(dplyr)
library(tidytext)
library(tm)
library(textstem)
library(stringr)
library(tidyverse)

# Initiating work directory
setwd("C:/Users/Jeremy Chia/Documents/GitHub/NLP-SustainabilityReports-FinancialPerformance/")

# Defining variables
reports <- read.csv("sustainability_reports_sgx_sti.csv")


setwd("C:/Users/Jeremy Chia/Documents/GitHub/NLP-SustainabilityReports-FinancialPerformance/Sustainability Reports or Annual Reports")


# Reading in Text
for (i in 1:nrow(reports)){
  if (i == 1){
    text <- pdftools::pdf_text(reports$DocumentName[i])
    # Initiate the first dataframe
    text_df <- tibble(page = 1:length(text), text = text)
    text_df$Company <- reports$Company[i]
    text_df$Year <- reports$Year[i]
  }
  else{
    text <- pdftools::pdf_text(reports$DocumentName[i])
    # Create a temporary dataframe
    text_df_temp <- tibble(page = 1:length(text), text = text)
    text_df_temp$Company <- reports$Company[i]
    text_df_temp$Year <- reports$Year[i]
    
    # Append to the original dataframe
    text_df <- rbind(text_df,text_df_temp)
  }
}

## Text Cleaning

text_df$text <- str_replace_all(text_df$text, "\n", " ")
text_df$text <- str_replace_all(text_df$text, "/", " ")
text_df$text <- str_replace_all(text_df$text, "@", " ")
text_df$text <- str_replace_all(text_df$text, "\\|", " ")
text_df$text <- str_replace_all(text_df$text, "•", " ")

## Unnesting Tokens

text_tokenised <- text_df %>% 
  unnest_tokens (word, text)

## Counting Number of Words

setwd("C:/Users/Jeremy Chia/Desktop/2022/Research/Code/4. Sentiment Analysis")

write.csv(text_tokenised %>% 
  group_by(Company,Year) %>% 
  count(), "number_words.csv", row.names = F)

## 