
# library
library(dplyr)
library(stringr)
library(tm)
library(sf)
library(wordcloud)

# 1.0 Place of interest name wordcloud
# Read data
poi_data = readRDS("data/poi_kv_full_data.RDS")
# filter data
poi_data = poi_data %>% select(poi_name,poi_categ,poi_categ_sub)
# cleaning text
poi_data$poi_name = gsub("[^\x01-\x7F]"," ",poi_data$poi_name)
poi_data$poi_name = gsub("[^0-9A-Za-z///']"," ",poi_data$poi_name)
poi_data$poi_name = str_squish(poi_data$poi_name)
# create corpus
corpus = VCorpus(VectorSource(poi_data$poi_name))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)
# create document-term matrix
dtm = TermDocumentMatrix(corpus)
dtm = removeSparseTerms(dtm,0.999)
matrix = as.matrix(dtm)
words = sort(rowSums(matrix),decreasing = T)
df = data.frame(word = names(words), freq=words)
# word cloud
set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, 
          random.order = F, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))  

# 1.1 Place of interest name wordcloud according to sub-categories
poi_name_wordcloud <- function(sub_category){
  # Read data
  poi_data = readRDS("data/poi_kv_full_data.RDS")
  # filter data
  poi_data = poi_data %>% select(poi_name,poi_categ,poi_categ_sub)
  poi_data = poi_data %>% filter(poi_categ_sub==sub_category)
  # cleaning text
  poi_data$poi_name = gsub("[^\x01-\x7F]"," ",poi_data$poi_name)
  poi_data$poi_name = gsub("[^0-9A-Za-z///']"," ",poi_data$poi_name)
  poi_data$poi_name = str_squish(poi_data$poi_name)
  # create corpus
  corpus = VCorpus(VectorSource(poi_data$poi_name))
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords())
  corpus = tm_map(corpus, stemDocument)
  corpus = tm_map(corpus, stripWhitespace)
  # create document-term matrix
  dtm = TermDocumentMatrix(corpus)
  dtm = removeSparseTerms(dtm,0.9999)
  matrix = as.matrix(dtm)
  words = sort(rowSums(matrix),decreasing = T)
  df = data.frame(word = names(words), freq=words)
  # word cloud
  set.seed(1234)
  wordcloud(words = df$word, freq = df$freq, min.freq = 1, 
            random.order = F, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))  
}
poi_name_wordcloud("restaurant")
poi_name_wordcloud("apartments")
poi_name_wordcloud("company")
poi_name_wordcloud("road")

# 2.0 Place of interest category wordcloud
# 2.1.1 Creating category wordcloud 1
poi_data = readRDS("poi_kv_full_data.RDS")
corpus = VCorpus(VectorSource(poi_data$poi_categ_sub))
matrix = as.matrix(TermDocumentMatrix(corpus))
words = sort(rowSums(matrix),decreasing = T)
df = data.frame(word = names(words), freq=words)
category_freq = df
rownames(df) = NULL
set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, 
          random.order = F, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
# 2.1.2 Creating category wordcloud 2
set.seed(1234)
df = df[7:nrow(df),]
wordcloud(words = df$word, freq = df$freq, min.freq = 1, 
          random.order = F, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))

# 2.2 Category statistics
# 2.2.1 First stats
cap = df$freq
cap = sort(cap)
qt = c(length(cap)/4,length(cap)/2,length(cap)*3/4,length(cap))
# Divide quartile
q1 = cap[1:qt[1]]
q2 = cap[(qt[1]+1):qt[2]]
q3 = cap[(qt[2]+1):qt[3]]
q4 = cap[(qt[3]+1):qt[4]]
# Create table
mini = c(min(q1),min(q2),min(q3),min(q4))
maxi = c(max(q1),max(q2),max(q3),max(q4))
meani = c(mean(q1),mean(q2),mean(q3),mean(q4))
sumi = c(sum(q1),sum(q2),sum(q3),sum(q4))
stat = data.frame(quartile = 1:4, total_category = qt, min_freq = mini, max_freq = maxi, mean_freq = meani)
plot(cap)

# 2.2.2 First filtered stats
df = data.frame(word = names(words), freq=words)
rownames(df) = NULL
df = df[order(df$freq),]
df = df[ceiling(nrow(df)*3/4):nrow(df),]
cap = df$freq
cap = sort(cap)
qt = c(length(cap)/4,length(cap)/2,length(cap)*3/4,length(cap))
# Divide quartile
q1 = cap[1:qt[1]]
q2 = cap[(qt[1]+1):qt[2]]
q3 = cap[(qt[2]+1):qt[3]]
q4 = cap[(qt[3]+1):qt[4]]
# Create table
mini = c(min(q1),min(q2),min(q3),min(q4))
maxi = c(max(q1),max(q2),max(q3),max(q4))
meani = c(mean(q1),mean(q2),mean(q3),mean(q4))
sumi = c(sum(q1),sum(q2),sum(q3),sum(q4))
stat = data.frame(quartile = 1:4, total_category = qt, min_freq = mini, max_freq = maxi, mean_freq = meani)
plot(cap)

# 2.2.3 Second filtered stats
df = df[1:ceiling(nrow(df)*3/4),]
cap = df$freq
cap = sort(cap)
qt = c(length(cap)/4,length(cap)/2,length(cap)*3/4,length(cap))
# Divide quartile
q1 = cap[1:qt[1]]
q2 = cap[(qt[1]+1):qt[2]]
q3 = cap[(qt[2]+1):qt[3]]
q4 = cap[(qt[3]+1):qt[4]]
# Create table
mini = c(min(q1),min(q2),min(q3),min(q4))
maxi = c(max(q1),max(q2),max(q3),max(q4))
meani = c(mean(q1),mean(q2),mean(q3),mean(q4))
sumi = c(sum(q1),sum(q2),sum(q3),sum(q4))
stat = data.frame(quartile = 1:4, total_category = qt, min_freq = mini, max_freq = maxi, mean_freq = meani)
plot(cap)