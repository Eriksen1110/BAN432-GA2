# BAN 432
# Group Assignment 2
# Group 14

# Load libraires 
require(tm)
require(topicmodels)
require(wordcloud)
require(udpipe)
require(dplyr)
require(slam)
require(ggplot2)
# Load data
load("congressional_records.RData")
tagger <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

# Function to clean the documents
process_document <- function(doc_text) {
  doc_text %>%
    removePunctuation() %>%
    removeNumbers() %>%
    removeWords(stopwords("en")) %>%
    tolower() %>%
    stripWhitespace() %>%
    str_replace_all("\\b\\w{1,2}\\b|\\b\\w{21,}\\b", "") %>%
    stripWhitespace() -> cleaned_text
  
  annotations <- udpipe_annotate(tagger, x=cleaned_text)
  data_frame <- as.data.frame(annotations)
  filtered_data <- data_frame %>%
    filter(upos %in% c("NOUN", "VERB")) %>%
    pull(lemma) %>%
    paste(collapse = " ")
  
 return(filtered_data)
}
# Preproccessing of data

#processed_docs <- lapply(crec$strText, process_document)
#save(processed_docs, file = "ProcessedData.RData")
load("ProcessedData.Rdata")

#Creating a DTM
corpus <- Corpus(VectorSource(processed_docs))
dtm <- DocumentTermMatrix(corpus,
                          control = list(
                            wordLengths = c(6, 30),
                            bounds = list(global = c(500,10000))))


dtm <- dtm[row_sums(dtm) > 10,]
dim(dtm)

# 2. Model Estimation
num_topics <- 20 # Decide on the number
topic <- LDA(dtm,  # document term matrix
             k = num_topics, # specifify number of topics
             method = "Gibbs",
             control = list(
               seed = 1234, # eases replication
               burnin = 100,  # how often sampled before estimation recorded
               iter = 300,  # number of iterations
               keep = 1,    # saves additional data per iteration (such as logLik)
               save = F,     # saves logLiklihood of all iterations
               verbose = 10  # report progress
             ))

# Display top 10 terms per topic
apply(topic@beta, 1, function(x) head(topic@terms[order(x, decreasing = T)],10))

# Get the indices of the documents that were kept after data cleaning
kept_indices <- rownames(as.matrix(dtm))

# Subset the crec dataframe using these indices
subset_crec <- crec[as.numeric(kept_indices), ]

topic_assignments <- topics(topic, 1)

# Add the topic assignments to the subsetted dataframe
subset_crec$topic <- topic_assignments

# View the first few rows of the subsetted dataframe to confirm
head(subset_crec)

#filtering for year
early_period <- subset_crec %>%
  filter(substr(date, 1, 4) %in% c("1995", "1996"))

late_period <- subset_crec %>%
  filter(substr(date, 1, 4) %in% c("2021", "2022"))

# Load ggplot2
library(ggplot2)

# Counts the topics in the different periods
topic_counts_early <- early_period %>%
  group_by(topic) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

topic_counts_late <- late_period %>%
  group_by(topic) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Create the bar chart
ggplot(topic_counts_early, aes(x = factor(topic), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Prevalence of Topics in Documents",
       x = "Topic",
       y = "Number of Documents") +
  theme_minimal() +
  coord_flip()  # Display topics on the y-axis for better readability

ggplot(topic_counts_late, aes(x = factor(topic), y = count)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Prevalence of Topics in Documents",
       x = "Topic",
       y = "Number of Documents") +
  theme_minimal() +
  coord_flip()  # Display topics on the y-axis for better readability

# As we can see from the barcharts above there are clear differences between topics
# in the different periods. For simplicities sake i will make a wordcloud of the top two 
# topics from each era. IA topic 8 and 1 for the early period, and topic 2 and 20 for the late period

Topics <- c(1,8, 2, 20)
beta <- exp(topic@beta)
for(top in Topics){
  terms.top.10 <- head(topic@terms[order(beta[top,], decreasing = T)], 10)
  prob.top.10 <- head(sort(beta[top,], decreasing = T), 10)
  wordcloud(words = terms.top.10,
            freq = prob.top.10,
            random.order = F,
            scale = c(3, 0.3))
  title(paste("Topic", top))
  
}

# Summarizing count for each of the parties in the early period
party_count_early <- early_period %>%
  group_by(topic, party) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plotting the party count for the topics
ggplot(party_count_early, aes(x = factor(topic), y = count, fill = party)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Republican" = "red", "Democrat" = "blue")) +
  labs(title = "Topics by Party",
       x = "Topic",
       y = "Number of Documents") +
  theme_minimal() +
  theme(legend.title = element_blank())
# In the early period is hard to see a clear dominant party for any of the topics
# Topic 18 might be the closest with about twice as many republicant documents.
# Topic 1 might be the closest to a democratic dominant topic

# Repeat for late period
party_count_late <- late_period %>%
  group_by(topic, party) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(party_count_late, aes(x = factor(topic), y = count, fill = party)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Republican" = "red", "Democrat" = "blue")) +
  labs(title = "Topics by Party",
       x = "Topic",
       y = "Number of Documents") +
  theme_minimal() +
  theme(legend.title = element_blank())


# In the late period there is a clear distinction for a few of the topics. 
# It is now clear that topic 18 is a republicant topic. 
# Topic 20 is now also clearly a democratic topic.

Topics2 <- c(18, 20, 1)
for(top in Topics2){
  terms.top.10 <- head(topic@terms[order(beta[top,], decreasing = T)], 10)
  prob.top.10 <- head(sort(beta[top,], decreasing = T), 10)
  wordcloud(words = terms.top.10,
            freq = prob.top.10,
            random.order = F,
            scale = c(3, 0.3))
  title(paste("Topic", top))
  
}
