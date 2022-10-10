# Processing Text of Sustainability Reports 

library(pdftools)
library(dplyr)
library(tidytext)
library(tm)
library(textstem)
library(topicmodels)
library(lda)
library(ldatuning)
library(ggplot2)
library(wordcloud)
library(pals)
library(SnowballC)
library(stringr)
library(quanteda)
library(reshape2)


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

# Converting to a Corpus
docs <- Corpus(VectorSource(text_df$text))

# Text Cleaning
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "\n")
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# docs <- tm_map(docs, toSpace, "*")
docs <- tm_map(docs, toSpace, "[^[:alnum:] ]")


# Text Cleaning
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove english (SMART) common stopwords
docs <- tm_map(docs, removeWords, stopwords("SMART"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("reporting","period","report","fy")) # reporting words
## Remove additional stopwords as identified
setwd("C:/Users/Jeremy Chia/Desktop/2022/Research/Code/1. Before removing additional stopwords")
to_remove <- as.vector(read.csv("2. to_remove.csv",header=T)[[1]])
docs <- tm_map(docs, removeWords, to_remove) # remove company, country, irrelevant words
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text lemmization
docs <- lemmatize_words(docs, dictionary = lexicon::hash_lemmas)

# Compute Document Term Matrix where Word >= minimumFrequency
minimumFrequency <- 10
DTM <- DocumentTermMatrix(docs, control = list(bounds = list(global = c(minimumFrequency, Inf))))

DTM <- DTM[apply(DTM,1,FUN=sum)!=0,]

# number of topics
K <- 7
# set random number generator seed
set.seed(1234)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))


### FORM TOPICS

tmResult <- posterior(topicModel)
beta <- tmResult$terms
theta <- tmResult$topics

terms(topicModel, 10)
exampleTermData <- terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

# text_df %>% select(Company, Year)

theta_df <- data.frame(theta)

# Filter for pages which are in the document corpus
index_nonzero <- data.frame(index_nonzero,header=0)$index_nonzero

theta_df$Company <- text_df[index_nonzero,]$Company
theta_df$Year <- text_df[index_nonzero,]$Year
theta_df$page <- text_df[index_nonzero,]$page

setwd("C:/Users/Jeremy Chia/Desktop/2022/Research/Code/2. After removing additional stopwords")
write.csv(theta_df, "topic_distribution_v6_7topics.csv",row.names=F)

#####

# graph distribution of topics

# get topic proportions form example documents
topicProportionExamples <- theta[1:3,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- reshape2::melt(cbind(data.frame(topicProportionExamples), document = factor(1:3)), variable.name = "topic", id.vars = "document")  

ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = 3)


#####

# re-rank top topic terms for topic names
topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")

# What are the most probable topics in the entire collection?
topicProportions <- colSums(theta) / nDocs(DTM)  # mean probablities over all paragraphs
names(topicProportions) <- topicNames     # assign the topic names we created before
sort(topicProportions, decreasing = TRUE) # show summed proportions in decreased order

# We count how often a topic appears as a primary topic within a paragraph This method is also called Rank-1.
countsOfPrimaryTopics <- rep(0, K)
names(countsOfPrimaryTopics) <- topicNames
for (i in 1:nDocs(DTM)) {
  topicsPerDoc <- theta[i, ] # select topic distribution for document i
  # get first element position from ordered list
  primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1] 
  countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] + 1
}
sort(countsOfPrimaryTopics, decreasing = TRUE)
so <- sort(countsOfPrimaryTopics, decreasing = TRUE)
paste(so, ":", names(so))
