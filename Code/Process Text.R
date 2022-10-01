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

# Summarising Results
number_pages <- text_df %>% 
  group_by(Company, Year) %>% 
  count()

number_pages %>% 
  mutate(
    Year = as.character(Year)
  ) %>% 
  ggplot(aes(x=Year,y=n)) +
  geom_boxplot(aes(x=Year), fill = "grey", alpha = 0.2) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  theme_minimal() +
  ggtitle("Number of Pages by Year") +
  ylab("Number of Pages") +
  xlab("Disclosure for Year Ended In")
  theme(legend.position="none") +
  scale_fill_brewer(palette="Blues")

setwd("C:/Users/Jeremy Chia/Documents/GitHub/NLP-SustainabilityReports-FinancialPerformance/")
write.csv(number_pages, "number_of_pages.csv", row.names=F)

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


# setwd("C:/Users/Jeremy Chia/Documents/GitHub/NLP-SustainabilityReports-FinancialPerformance/")
# characters_remove <- read.csv("characters.csv")
# 
# for (i in 1:nrow(characters_remove)){
#   docs <- tm_map(docs, toSpace, characters_remove$X.[i])
#   print(i)
# }

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

#####

# Compute Document Term Matrix where Word >= minimumFrequency
minimumFrequency <- 10
DTM <- DocumentTermMatrix(docs, control = list(bounds = list(global = c(minimumFrequency, Inf))))

DTM <- DTM[apply(DTM,1,FUN=sum)!=0,]

# create models with different number of topics
result <- ldatuning::FindTopicsNumber(
  DTM,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

setwd("C:/Users/Jeremy Chia/Desktop/2022/Research/Code/2. After removing additional stopwords")
write.csv(result,"metrics_v3.csv")

FindTopicsNumber_plot(result)

# number of topics
K <- 9
# set random number generator seed
set.seed(1234)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))

topics <- tidy(topicModel, matrix = "beta")

# get the top ten terms for each topic
top_terms <- topics  %>% # take the topics data frame and..
  group_by(topic) %>% # treat each topic as a different group
  top_n(10, beta) %>% # get the top 10 most informative words
  ungroup() %>% # ungroup
  arrange(topic, -beta) # arrange words in descending informativeness

### GRAPH OUT TOP TERMS

top_terms %>% # take the top terms
  mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
  ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
  geom_col(show.legend = FALSE) + # as a bar plot
  facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
  labs(x = NULL, y = "Beta") + # no x label, change y label 
  coord_flip() # turn bars sideways


write.csv(top_terms,"top_words_v3.csv",fileEncoding = "utf8")

### FORM TOPICS

tmResult <- posterior(topicModel)
beta <- tmResult$terms
theta <- tmResult$topics

terms(topicModel, 10)
exampleTermData <- terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

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

for (i in 1:K){
  ## Topic Wordcloud
  topicToViz <- i
  top100terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:100]
  words <- names(top100terms)
  # extract the probabilites of each of the 40 terms
  probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:100]
  # visualize the terms as wordcloud
  mycolors <- brewer.pal(8, "Dark2")
  
  # Generate the Word Cloud
  # setwd("C:/Users/Jeremy Chia/Documents/GitHub/NLP-SustainabilityReports-FinancialPerformance/Sustainability Reports or Annual Reports")
  png(paste(i,".png",sep=""), width=480,height=480)
  wordcloud(words, probabilities, random.order = FALSE, color = mycolors, rot.per=0)
  dev.off()
}
