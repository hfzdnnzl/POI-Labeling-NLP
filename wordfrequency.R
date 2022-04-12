

# 1.0 Place of interest name wordcloud
poi_name_freq <- function(sub_category){
  # library
  library(dplyr)
  library(stringr)
  library(tm)
  library(sf)
  
  # Read data
  poi_data = readRDS("poi_kv_full_data.RDS")
  
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
  dtm = removeSparseTerms(dtm,0.999)
  matrix = as.matrix(dtm)
  words = sort(rowSums(matrix),decreasing = T)
  df = data.frame(word = names(words), freq=words, rank=1:length(words), category=sub_category)
  if(nrow(df)<10){df = df[1:nrow(df),]
  }else{df = df[1:10,]}
  
  return(df)
}

# Get category frequency
library(tm)
poi_data = readRDS("poi_kv_full_data.RDS")
corpus = VCorpus(VectorSource(poi_data$poi_categ_sub))
matrix = as.matrix(TermDocumentMatrix(corpus))
words = sort(rowSums(matrix),decreasing = T)
df = data.frame(word = names(words), freq=words)
rownames(df) = NULL
remove(corpus,matrix)
saveRDS(df,"data/sub_category_freq.RDS")

# Get word frequency in each category
df = df[df$freq>10,]
wordfreq_df = data.frame(stringsAsFactors = F)
for(i in 1:nrow(df)){
  x = poi_name_freq(df$word[i])
  wordfreq_df = rbind(wordfreq_df,x)
}
rownames(wordfreq_df) = NULL

# Get the top word frequency in each category
topwords_df = wordfreq_df[wordfreq_df$rank==1,]
topwords_df$new_category = NA
for(i in 1:(nrow(topwords_df)-1)){
  one = topwords_df$word[i]
  if(is.na(topwords_df$new_category[i])){topwords_df$new_category[i] = topwords_df$category[i]}
  for(j in (i+1):nrow(topwords_df)){
    two = topwords_df$word[j]
    if(!is.na(topwords_df$new_category[j])){next}
    if(one==two){topwords_df$new_category[j]=topwords_df$category[i]}
  }
}
