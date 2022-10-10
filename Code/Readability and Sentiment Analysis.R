# Processing Text of Sustainability Reports

library(pdftools)
library(dplyr)
library(tidytext)
library(tm)
library(textstem)
library(stringr)
library(tidyverse)
library(quanteda.textstats)

# Initiating work directory
setwd("C:/Users/Jeremy Chia/Documents/GitHub/NLP-SustainabilityReports-FinancialPerformance/")

# Defining variables
reports <- read.csv("sustainability_reports_sgx_sti.csv")


setwd("C:/Users/Jeremy Chia/Documents/GitHub/NLP-SustainabilityReports-FinancialPerformance/Sustainability Reports or Annual Reports")


# Reading in Text
for (i in 1:nrow(reports)){
  if (i == 1){
    text <- pdftools::pdf_text(reports$DocumentName[i])
    text_joined <- paste(text, collapse = ' ')
    # Initiate the first dataframe
    text_df <- tibble(text = text_joined)
    text_df$Company <- reports$Company[i]
    text_df$Year <- reports$Year[i]
  }
  else{
    text <- pdftools::pdf_text(reports$DocumentName[i])
    text_joined <- paste(text, collapse = ' ')
    # Create a temporary dataframe
    text_df_temp <- tibble(text = text_joined)
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

## Add readability scores

for (i in 1:nrow(text_df)){
  flesch_temp <- textstat_readability(
    text_df[i,]$text,
    measure = "Flesch",
    remove_hyphens = TRUE,
    min_sentence_length = 1,
    max_sentence_length = 10000,
    intermediate = FALSE
  )$Flesch
  if (i == 1) {
    flesch_scores <- c(flesch_temp)
  }
  else {
    flesch_scores <- c(flesch_scores, flesch_temp)
  }
}

text_df_with_flesch <- text_df
text_df_with_flesch$flesch <- flesch_scores

setwd("C:/Users/Jeremy Chia/Desktop/2022/Research/Code/3. Readability")

write.csv(text_df_with_flesch %>% select(Company, Year, flesch), "readability.csv",row.names = F)

## GRAPH

read.csv("readability.csv") %>% 
  mutate(
    Year = as.character(Year)
  ) %>% 
  ggplot(aes(x=Year,y=flesch)) +
  geom_boxplot(aes(x=Year), fill = "grey", alpha = 0.2) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  theme_minimal() +
  ggtitle("Readability Ease by Year") +
  ylab("Readability Ease") +
  xlab("Disclosure for Year Ended In")
  theme(legend.position="none") +
  scale_fill_brewer(palette="Blues")


