library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(SnowballC)
library(textdata)
library(syuzhet)
library(wordcloud)
library(tm)
library(topicmodels)
library(slam)
library(ldatuning)
library(reshape2)
library(RTextTools)

# 3
setwd("c:/Users/Jake/Downloads")
getwd()
data <- read.csv("psychcentral_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NaN","","?","NA"))
View(data)
# 3.1
colnames(data)
# 3.2
nrow(data)





# 4
data$q_content <- as.character(data$q_content)
tidy_text <- data %>% 
  unnest_tokens(word, q_content)

# remove stop words 
data("stop_words")
stop_words
tidy_text <- tidy_text %>% anti_join(stop_words, by ="word")

# 4.1
tidy_text %>% count(word, sort = TRUE)

# 4.2
tidy_text %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 2000) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_bar(stat = "identity") + 
  xlab(NULL) + 
  coord_flip()

# 4.3
# 4.4.1
View(tidy_text)
tidy_text <- tidy_text %>% 
  mutate(word = wordStem(word), languages = "english")
tidy_text %>% count(word, sort = TRUE)


# 4.4.2
tidy_text %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 4000) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_bar(stat = "identity") + 
  xlab(NULL) + 
  coord_flip()

# 4.4.3
tidy_text %>% 
  count(word, sort = TRUE) %>% 
  with(wordcloud(word, n, max.words = 200)) 

# 4.4.4
tidy_text %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 200)



# 5
data$answers <- as.character(data$answers)
tidy_text2 <- data %>% 
  unnest_tokens(word, answers)
# 5.1
tidy_text2 %>% count(word, sort = TRUE)
# 5.2
tidy_text2 %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 4000) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_bar(stat = "identity") + 
  xlab(NULL) + 
  coord_flip()
# 5.3.1
tidy_text2 <- tidy_text2 %>% anti_join(stop_words, by ="word")
tidy_text2 <- tidy_text2 %>% 
  mutate(word = wordStem(word), languages = "english")
tidy_text2 %>% count(word, sort = TRUE)


# 5.3.2
tidy_text2 %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 6000) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_bar(stat = "identity") + 
  xlab(NULL) + 
  coord_flip()
# 5.3.3
tidy_text2 %>% 
  count(word, sort = TRUE) %>% 
  with(wordcloud(word, n, max.words = 200)) 
# 5.3.4
tidy_text2 %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 5) %>% 
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)

# 6
data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$q_content), readerControl=list(language="en"))
summary(corpus)
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 5) # k is the number of topics to be found.

# 6.1
lda_td <- tidy(lda) 

top_terms <- lda_td %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

# 6.3.1
lda <- LDA(dtm.new, k = 2)
lda_td <- tidy(lda) 

top_terms <- lda_td %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

# 6.3.2
lda <- LDA(dtm.new, k = 3)
lda_td <- tidy(lda) 

top_terms <- lda_td %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

# 6.3.3
lda <- LDA(dtm.new, k = 4)
lda_td <- tidy(lda) 

top_terms <- lda_td %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

# 6.3.4
lda <- LDA(dtm.new, k = 10)
lda_td <- tidy(lda) 

top_terms <- lda_td %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

# 7.1
lda <- LDA(dtm.new, k = 10)
lda_td <- tidy(lda) 

top_terms <- lda_td %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

# 7.3.1
lda <- LDA(dtm.new, k = 2)
lda_td <- tidy(lda) 

top_terms <- lda_td %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

# 7.3.2
lda <- LDA(dtm.new, k = 8)
lda_td <- tidy(lda) 

top_terms <- lda_td %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

# 7.3.3
lda <- LDA(dtm.new, k = 11)
lda_td <- tidy(lda) 

top_terms <- lda_td %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

# 7.3.4
lda <- LDA(dtm.new, k = 14)
lda_td <- tidy(lda) 

top_terms <- lda_td %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()
