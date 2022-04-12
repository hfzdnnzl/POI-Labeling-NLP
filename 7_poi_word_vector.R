# library
library(ggplot2)
# read data
wrongpred = readRDS("data/naivebayes_wrongclass_freq.RDS")
# clean data
wrongpred$class = gsub("_"," ",wrongpred$class)
wrongpred$predicted = gsub("_"," ",wrongpred$predicted)
# word tendency plot
word_tendency = function(num){
  # get word frequency dictionary
  models = readRDS('model.RDS')
  class = NA
  pred = NA
  for(i in 1:length(models)){
    dict = models[[i]]
    if(dict$category==wrongpred$class[num]){class = dict}
    if(dict$category==wrongpred$predicted[num]){pred = dict}
    if(!is.na(class)&!is.na(pred)){break}
  }
  # get the most frequent words
  classname = class$category
  predname = pred$category
  class = class$lambda
  pred = pred$lambda
  top20 = append(class$word[1:100],pred$word[1:100])
  top20 = top20[!duplicated(top20)]
  # initialize dataframe
  top20 = data.frame(word=top20,classfreq=0,predfreq=0,stringsAsFactors = F)
  # place in dataframe
  for(i in 1:nrow(top20)){
    pattern = paste0("^",top20$word[i],"$")
    if(length(grep(pattern,class$word))==1){
      top20$classfreq[i] = class$pos_prob[class$word==top20$word[i]]}
    if(length(grep(pattern,pred$word))==1){
      top20$predfreq[i] = pred$pos_prob[pred$word==top20$word[i]]}
  }  
  # return plot data
  plt_list = list(top20=top20,classname=classname,predname=predname)
}
plt_list = word_tendency(5)
top20 = plt_list$top20
top20$lambda = log10(top20$classfreq/top20$predfreq)
# plotting the words
ggplot(data = top20, aes(x=classfreq, y=predfreq, label=word)) +
  geom_point(colour="#00abff") + geom_text(size=3) + geom_abline(slope = 1,intercept = 0,colour="#00abff") +
  xlab(plt_list$classname) + ylab(plt_list$predname)
