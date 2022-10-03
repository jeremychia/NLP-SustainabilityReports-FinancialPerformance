# Processing Text of Sustainability Reports

library(pdftools)
library(dplyr)
library(tidytext)
library(tm)
library(textstem)
library(stringr)
library(tidyverse)
library(ggplot2)
library(pals)
library(SnowballC)

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

text_tokenised %>% 
  group_by(Company,Year) %>% 
  count() %>% 
  mutate(
    Year = as.character(Year)
  ) %>% 
  ggplot(aes(x=Year,y=n)) +
  geom_boxplot(aes(x=Year), fill = "grey", alpha = 0.2) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  theme_minimal() +
  ggtitle("Number of Words by Year") +
  ylab("Number of Words") +
  xlab("Disclosure for Year Ended In")
  theme(legend.position="none")

## Exploring Sentiments

get_sentiments("afinn")
get_sentiments("nrc")
get_sentiments("bing")
get_sentiments("loughran")

library(lexicon)
key_sentiment_jockers

## Joining sentiments

afinn <- text_tokenised %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(Company, Year, page) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

summary((afinn %>% group_by(Company, Year))$sentiment)

nrc <- text_tokenised %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment %in% c("positive","negative")) %>% 
  mutate(method = "NRC") %>% 
  count(Company, Year, page, sentiment) %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

summary((nrc %>% group_by(Company, Year))$sentiment)

bing <- text_tokenised %>% 
  inner_join(get_sentiments("bing")) %>% 
  filter(sentiment %in% c("positive","negative")) %>% 
  mutate(method = "Bing et al.") %>% 
  count(Company, Year, page, sentiment) %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

summary((bing %>% group_by(Company, Year))$sentiment)

loughran <- text_tokenised %>% 
  inner_join(get_sentiments("loughran")) %>% 
  filter(sentiment %in% c("positive","negative")) %>% 
  mutate(method = "Loughran") %>% 
  count(Company, Year, page, sentiment) %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

summary((loughran %>% group_by(Company, Year))$sentiment)

jockers <- text_tokenised %>% 
  inner_join(key_sentiment_jockers) %>% 
  group_by(Company, Year, page) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "Jockers")

summary((jockers %>% group_by(Company, Year))$sentiment)
