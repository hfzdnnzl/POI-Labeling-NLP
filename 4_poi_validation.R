# 1.0 validation of model
nlp_naivebayes_predict = function(name,dict){
  # initialize library
  library(stringr)
  # initialize progress bar
  cat("\nPredicting category:",dict$category,
      "\nNumber of rows:",length(name),"\n")
  pb = txtProgressBar(min = 1,max = length(name),style = 3,char = "=")
  # initialize data
  logprior = dict$logprior
  lambda = dict$lambda
  pred = vector()
  # starting prediction
  for(i in 1:length(name)){
    # creating list of words
    w_list = str_split(name[i]," ")[[1]]
    # calculating score
    score = logprior
    for(j in 1:length(w_list)){
      w = w_list[j]
      if(length(grep(paste0("^",w,"$"),lambda$word)) > 0){
        score = score + lambda$lambda[lambda$word==w]
      }
    }
    # predicting
    pred[i] = ifelse(score>0,1,0)
    # updating progress bar
    setTxtProgressBar(pb,i)
  }
  return(pred)
}
# 2.0 Calculating accuracy for each model
# library
library(dplyr)
library(sf)
# read data
# models = list.files("nlp_model")
# models = paste0("nlp_",1:length(models),".RDS")
models = readRDS('model.RDS')
poi_data = readRDS("data/poi_clean_name.RDS")
poi_data = poi_data %>% select(poi_name,category)
# analyze model
model_accuracy = data.frame(stringsAsFactors = F)
for(i in 1:length(models)){
  # read model
  # path = paste0("nlp_model/",models[i])
  # dict = readRDS(path)
  dict = models[[i]]
  # prediction testing
  test = poi_data[sample(nrow(poi_data),1000),]
  test$pred = nlp_naivebayes_predict(name = test$poi_name,
                                     dict = dict)
  # labeling data
  test$category[test$category==dict$category] = 1
  test$category[test$category!=1] = 0  
  # accuracy
  correct = ifelse(test$category==test$pred,1,0)
  accuracy = sum(correct)/length(correct)
  # save data
  df = data.frame(category = dict$category, accuracy = accuracy)
  model_accuracy = rbind(model_accuracy,df)
}
saveRDS(model_accuracy,"data/naivebayes_accuracy.RDS")
