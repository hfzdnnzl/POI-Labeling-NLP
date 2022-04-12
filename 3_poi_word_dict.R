# 1.0 Naive Bayes Dictionary
nlp_naivebayes_dictionary = function(poi_data,class){
  # library
  library(dplyr)
  library(sf)
  library(stringr)
  library(tm)  
  # 1.0 Create dictionary
  # filter data
  poi_data = poi_data %>% select(poi_name,category)
  pos_data = poi_data %>% filter(category==class)
  neg_data = poi_data %>% filter(category!=class)
  # splitting data
  set.seed(123)
  size = nrow(pos_data)
  pos_data = pos_data[sample(nrow(pos_data),size = size),]
  neg_data = neg_data[sample(nrow(neg_data),size = ceiling(size*2.5)),]
  # labelling the data
  pos_data$category = 1
  neg_data$category = 0
  rownames(poi_data) = NULL
  
  # 2.0 create word frequency for positive and negative words
  wordfreq_dictionary_df = function(poi_name,word_freq){
    # create corpus
    corpus = VCorpus(VectorSource(poi_name))
    # create document-term matrix
    dtm = TermDocumentMatrix(corpus)
    dtm = removeSparseTerms(dtm,word_freq)
    matrix = as.matrix(dtm)
    words = sort(rowSums(matrix),decreasing = T)
    df = data.frame(word = names(words), freq=words)
    rownames(df) = NULL
    
    return(df)
  }
  poswords_df = wordfreq_dictionary_df(pos_data$poi_name,0.9999)
  negwords_df = wordfreq_dictionary_df(neg_data$poi_name,0.9999)
  
  # 3.0 arranging the dictionary
  words = append(poswords_df$word,negwords_df$word)
  words = words[!duplicated(words)]
  allwords_df = data.frame(stringsAsFactors = F,
                           word = words, pos_freq = 0, neg_freq = 0,
                           pos_prob = 0, neg_prob = 0, lambda = 0)
  # word frequency in the dataset according to classes
  for(i in 1:nrow(allwords_df)){
    w = paste0("^",allwords_df$word[i],"$")
    if(length(grep(w,poswords_df$word))>0){
      allwords_df$pos_freq[i] = poswords_df$freq[poswords_df$word==allwords_df$word[i]]
    }
    if(length(grep(w,negwords_df$word))>0){
      allwords_df$neg_freq[i] = negwords_df$freq[negwords_df$word==allwords_df$word[i]]
    }
  }
  # conditional probability with laplacian smoothing
  Npos = sum(allwords_df$pos_freq)
  Nneg = sum(allwords_df$neg_freq)
  V = nrow(allwords_df)
  for(i in 1:nrow(allwords_df)){
    allwords_df$pos_prob[i] = (allwords_df$pos_freq[i]+1)/(Npos+V)
    allwords_df$neg_prob[i] = (allwords_df$neg_freq[i]+1)/(Nneg+V)
  }
  # calculating loglikelihood
  for(i in 1:nrow(allwords_df)){
    allwords_df$lambda[i] = log(allwords_df$pos_prob[i]/allwords_df$neg_prob[i])
  }
  # return data
  logprior = log(nrow(pos_data)/nrow(neg_data))
  nlp = list(category=class,lambda=allwords_df,logprior=logprior)
  return(nlp)
}
# Collecting all naive bayes dictionary
library(dplyr)
# read data
category_freq = readRDS("data/sub_category_freq.RDS")
category_freq = category_freq[category_freq$freq>10,]
poi_data = readRDS("data/poi_clean_name.RDS")
# start building library
i = 1
# progress bar
pb = txtProgressBar(min = i,max = nrow(category_freq),style = 3,char = "=")
dict_list = list()
for(i in i:nrow(category_freq)){
  dict = nlp_naivebayes_dictionary(poi_data = poi_data,class = category_freq$word[i])
  dict_list = append(dict_list,list(dict))
  # progress bar
  setTxtProgressBar(pb,i)
}
path = paste0("model.RDS")
saveRDS(dict,path)