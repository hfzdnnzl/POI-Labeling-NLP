# library
library(dplyr)
library(tm)
library(stringr)
# read data
predicted_df = readRDS("data/naivebayes_result.RDS")
predicted_df = predicted_df %>% filter(p_score>0)
wrong_pred = predicted_df %>% filter(accuracy==0)
# clean for counting frequency
y_test = gsub(" ","_",wrong_pred$class)
y_pred = gsub(" ","_",wrong_pred$p_class)
y_false = paste(y_test,y_pred,sep = ";")  
# counting frequency
corpus = VCorpus(VectorSource(y_false))
matrix = as.matrix(TermDocumentMatrix(corpus))
words = sort(rowSums(matrix),decreasing = T)
ypred_freq = data.frame(word = names(words), freq=words)
rownames(ypred_freq) = NULL
# split words
splt_class = str_split(ypred_freq$word,";")
splt_df = data.frame(matrix(unlist(splt_class), nrow=nrow(ypred_freq), byrow=T),stringsAsFactors=FALSE)
ypred_freq = cbind(splt_df,ypred_freq)
names(ypred_freq)[1] = "class"
names(ypred_freq)[2] = "predicted"
ypred_freq = ypred_freq %>% select(-word)
# labeling similar class
# confused_class = ypred_freq
# for(i in 1:nrow(confused_class)){
#   copy = confused_class %>% filter(class==confused_class$predicted[i] & predicted==confused_class$class[i])
#   confused_class$
#   if()
# }
# adjusted accuracy
accuracy = (sum(predicted_df$accuracy))/nrow(predicted_df)
accuracy = (sum(predicted_df$accuracy)+sum(ypred_freq$freq[ypred_freq$freq>100]) )/nrow(predicted_df)
accuracy = (sum(predicted_df$accuracy)+sum(ypred_freq$freq[ypred_freq$freq>10]) )/nrow(predicted_df)
# save dataframe
ypred_freq = ypred_freq %>% select(-similar)
saveRDS(ypred_freq,"data/naivebayes_wrongclass_freq.RDS")
