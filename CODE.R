
#importing data1
library(tidyverse)
setwd("C:/Users/gisele/Desktop/Introduction to Data Science")
file<-read_csv("MN-DS-news-classification.csv")#News Category Dataset

#importing data2
raw_data <- readLines("News_Category_Dataset_v3.json")#The Multilabled News Dataset
raw_data_cleaned <- paste(raw_data, collapse = "")#combine the all line and ensure no separator
raw_data_cleaned <- gsub("\\}\\{", "},{", raw_data_cleaned) #ensure the data separte by "}{"
raw_data_cleaned <- paste("[", raw_data_cleaned, "]", sep = "")#wrapping the json data

library(jsonlite)
file2 <- fromJSON(raw_data_cleaned)

sum(is.na(file))
sum(is.na(file2))

library(tm)
library(glmnet)
#convert into corpus
#file1
corpus<-Corpus(VectorSource(file$title))
summary(corpus)
class(corpus)

#file2
corpus2<-Corpus(VectorSource(file2$headline))
summary(corpus2)

#binomial classification
binomial<-c(as.list(corpus),as.list(corpus2))#list
binomial_corpus<-Corpus(VectorSource(binomial))#corpus

label<-replicate(length(corpus),"file1"); label2<-replicate(length(corpus2),"file2")#character
lable_binomial<-c(label,label2)
length(binomial) == length(lable_binomial)

#data cleaning
cleaned_binomial<-tm_map(binomial_corpus,tolower)
cleaned_binomial<-tm_map(cleaned_binomial,removePunctuation)
cleaned_binomial<-tm_map(cleaned_binomial,removeWords,stopwords('en'))
cleaned_binomial<-tm_map(cleaned_binomial,stemDocument) #corpus


tokens <- Boost_tokenizer(binomial)#character
clean_tokens<-removePunctuation(tokens)
clean_tokens<-tolower(clean_tokens)
stops<-stopwords('en')
clean_tokens<-removeWords(clean_tokens,stops)
#class(tokens) #character


# Document-Term Matrix
binomial_dtm <- DocumentTermMatrix(cleaned_binomial)
class(binomial_dtm)


# frequency
library(jiebaR)
library(dplyr)
wordfreqs<-jiebaR::freq(clean_tokens)
wordfreqs <- dplyr::arrange(wordfreqs, -freq)#sort
wordfreqs

library(wordcloud2)
wordcloud2(wordfreqs[1:200,], 
           size = 50,
           fontFamily = 'Segoe UI',
           fontWeight = 'bold',
           color = 'random-dark',
           backgroundColor = "white",
           minRotation = -pi/4,
           maxRotation = pi/4,
           rotateRatio = 0.4,
           shape = "circle"
)#wordcloud


#LDA model
library(topicmodels)
library(slam)
empty_docs <- slam::row_sums(binomial_dtm) > 0
binomial_dtm_cleaned <- binomial_dtm[empty_docs, ] #dele empty doc
lda <- LDA(binomial_dtm_cleaned, k = 5)
terms(lda, 10)#topic

 
#file1
tokens1 <- Boost_tokenizer(corpus)
clean_tokens1<-removePunctuation(tokens1)
clean_tokens1<-tolower(clean_tokens1)
stops<-stopwords('en')
clean_tokens1<-removeWords(clean_tokens1,stops)

wordfreqs1<-jiebaR::freq(clean_tokens1)
wordfreqs1<-dplyr::arrange(wordfreqs1, -freq)#sort
wordfreqs1


wordcloud2(wordfreqs1[1:200,], 
           size = 50,
           fontFamily = 'Segoe UI',
           fontWeight = 'bold',
           color = 'random-dark',
           backgroundColor = "white",
           minRotation = -pi/4,
           maxRotation = pi/4,
           rotateRatio = 0.4,
           shape = "circle"
)#wordcloud

cleaned_corpus1<-tm_map(corpus,tolower)
cleaned_corpus1<-tm_map(cleaned_corpus1,removePunctuation)
cleaned_corpus1<-tm_map(cleaned_corpus1,removeWords,stopwords('en'))
cleaned_corpus1<-tm_map(cleaned_corpus1,stemDocument) #corpus
dtm1 <- DocumentTermMatrix(cleaned_corpus1)

#LDA model
empty_docs <- slam::row_sums(binomial_dtm) > 0

binomial_dtm_cleaned <- binomial_dtm[empty_docs, ]
dtm1_cleaned <- dtm1[!empty_docs, ] #dele empty doc
lda1 <- LDA(dtm1_cleaned, k = 17)
terms(lda1, 10)#topic

#file2
tokens2 <- Boost_tokenizer(corpus2)
clean_tokens2<-removePunctuation(tokens2)
clean_tokens2<-tolower(clean_tokens2)
stops<-stopwords('en')
clean_tokens2<-removeWords(clean_tokens2,stops)

wordfreqs2<-jiebaR::freq(clean_tokens2)
wordfreqs2<-dplyr::arrange(wordfreqs2, -freq)#sort
wordfreqs2

wordcloud2(wordfreqs2[1:200,], 
           size = 50,
           fontFamily = 'Segoe UI',
           fontWeight = 'bold',
           color = 'random-dark',
           backgroundColor = "white",
           minRotation = -pi/4,
           maxRotation = pi/4,
           rotateRatio = 0.4,
           shape = "circle"
)#wordcloud

cleaned_corpus2<-tm_map(corpus2,tolower)
cleaned_corpus2<-tm_map(cleaned_corpus2,removePunctuation)
cleaned_corpus2<-tm_map(cleaned_corpus2,removeWords,stopwords('en'))
cleaned_corpus2<-tm_map(cleaned_corpus2,stemDocument) #corpus
dtm2 <- DocumentTermMatrix(cleaned_corpus2)

#clustering
library(cluster)
corpus2_cate<-Corpus(VectorSource(file2$category))
corpus2_cate_clean <- tm_map(corpus2_cate, content_transformer(tolower))  
corpus2_cate_clean <- tm_map(corpus2_cate_clean, removePunctuation)      
corpus2_cate_clean <- tm_map(corpus2_cate_clean, removeWords, stopwords("en")) 
corpus2_cate_clean <- tm_map(corpus2_cate_clean, stripWhitespace)
dtm2_cate<-DocumentTermMatrix(corpus2_cate_clean)

k <- kmeans(as.matrix(dtm2_cate), centers = 4)
summary(k)
cluster_labels <- k$cluster
library(ggplot2)

pca <- prcomp(as.matrix(dtm2_cate), scale. = TRUE)
pca_data <- data.frame(pca$x)

ggplot(pca_data, aes(PC1, PC2, color = factor(cluster_labels))) +
  geom_point() +
  labs(title = "K-means Clustering of categories of News from HuffPost")

