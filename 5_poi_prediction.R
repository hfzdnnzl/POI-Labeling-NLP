# 1.0 prediction of model
nlp_naivebayes_predict = function(name,class,p_score,p_class,dict){
  # initilize progress bar
  cat("\nPredicting category:",dict$category,
      "\nNumber of rows:",length(name),"\n")
  # opts = list(progress=progress)
  # initialize libraries
  library(stringr)
  library(foreach)
  library(doParallel)
  # initialize cluster of cores
  numCores = detectCores()*3/4
  mycluster = makeCluster(numCores)
  registerDoParallel(mycluster)
  # initialize data
  logprior = dict$logprior
  lambda = dict$lambda
  # initialize dataframe
  df = data.frame(stringsAsFactors = F)
  # starting prediction
  df = foreach(i = 1:length(name), .combine = rbind, .packages = "stringr") %dopar% {
    # splitting words
    w_list = str_split(name[i]," ")[[1]]
    # calculating score
    score = logprior
    for(j in 1:length(w_list)){
      w = w_list[j]
      if(length(grep(paste0("^",w,"$"),lambda$word)) > 0){
        score = score + lambda$lambda[lambda$word==w]
      }
    }
    # setting score
    if(score>p_score[i]){
      p_score[i] = score
      p_class[i] = dict$category
    }
    # return dataframe
    df = data.frame(name=name[i],class=class[i],p_score=p_score[i],p_class=p_class[i])
  }
  # exit cluster
  on.exit(stopCluster(mycluster))
  # return dataframe
  return(df)
}
# 2.0 Accuracy for all models combined
# library
library(dplyr)
library(ggplot2)
# read data
models = readRDS('model.RDS')
poi_data = readRDS("data/poi_clean_name.RDS")
# filter data for prediction
predicted_df = data.frame(name = poi_data$poi_name,
                          class = poi_data$category,
                          p_score = 0,
                          p_class = 0,
                          stringsAsFactors = F)
predicted_df = predicted_df[sample(nrow(predicted_df),500000),]
# accumulative accuracy initialize dataframe
accu_df = data.frame(stringsAsFactors = F)
# starting prediction
i = 1
for(i in i:length(models)){
  # display info
  cat("\nCompletion:",round(i*100/length(models), digits = 2),"%")
  # read model
  dict = models[[i]]
  # run prediction
  new_pred = nlp_naivebayes_predict(name = predicted_df$name,
                                    class = predicted_df$class,
                                    p_score = predicted_df$p_score,
                                    p_class = predicted_df$p_class,
                                    dict = dict)
  # save in dataframe
  predicted_df = new_pred
  # calculate accumulative accuracy
  # label unpredicted class
  predicted_class = predicted_df$p_class[!duplicated(predicted_df$p_class)]
  unpred_df = predicted_df
  for(i in 1:length(predicted_class)){
    unpred_df = unpred_df %>% filter(class!=predicted_class[i])
  }
  if(nrow(unpred_df)>0){unpred_df$class = 0}
  for(i in 1:length(predicted_class)){
    unpred_df = rbind(unpred_df,predicted_df[predicted_df$class==predicted_class[i],])
  }
  # calculate accuracy
  correct = ifelse(unpred_df$class==unpred_df$p_class,1,0)
  accuracy = sum(correct)/length(correct)
  # accumulative accuracy dataframe
  df = data.frame(class = dict$category, accuracy = accuracy, stringsAsFactors = F)
  accu_df = rbind(accu_df,df)
}
# Total accuracy
predicted_df$accuracy = ifelse(predicted_df$class==predicted_df$p_class,1,0)
accuracy = sum(predicted_df$accuracy)/nrow(predicted_df)
# plot accuracy changes over category
ggplot(data = accu_df,aes(x=1:nrow(accu_df),y=accuracy)) + geom_point(colour = "red") +
  xlab("Number of Class Predicted") + ylab("Accuracy Changes")
# plot prediction changes
predicted_df$accu_accuracy = 0
predicted_df = predicted_df[order(predicted_df$p_score),]
for(i in 2:nrow(predicted_df)){
  predicted_df$accu_accuracy[i] = predicted_df$accu_accuracy[i-1] + predicted_df$accuracy[i]
}
ggplot(data = predicted_df,aes(x=p_score,y=accu_accuracy)) + geom_point(colour = "red") +
  xlab("Predicted Score") + ylab("Prediction Changes")
ggplot(data = predicted_df,aes(x=p_score,y=accuracy)) + geom_point(colour = "red") +
  xlab("Predicted Score") + ylab("Predictions")
# Adjusted Minimum Score
adjusted_min = data.frame(stringsAsFactors = F)
for(i in 0:90){
  min_score = ifelse(i==0,0,i/10)
  new_acc = predicted_df[predicted_df$p_score>min_score,]
  accuracy = sum(new_acc$accuracy)/nrow(new_acc)
  df = data.frame(min_score=min_score,accuracy=accuracy,
                  rows_predicted=nrow(new_acc),
                  total_rows=nrow(predicted_df),
                  predicted_ratio=nrow(new_acc)/nrow(predicted_df))
  adjusted_min = rbind(adjusted_min,df)
}
ggplot(data = adjusted_min,aes(x=min_score)) + 
  geom_line(aes(y=accuracy),colour="blue",size=1.2) +
  geom_line(aes(y=predicted_ratio),colour="red",size=1.2) +
  geom_text(aes(x=8,y=0.4),label="Predicted Rows Ratio",colour="red") +
  geom_text(aes(x=8,y=0.95),label="Accuracy Ratio",colour="blue") +
  ylab("Ratio") + xlab("Minimum Score")
# minimum score should be 0.35
new_acc = predicted_df[predicted_df$p_score>0.35,]
# save data
saveRDS(predicted_df,"data/naivebayes_result.RDS")
saveRDS(accu_df,"data/accumulative_accuracy.RDS")
saveRDS(adjusted_min,"data/accuracy_different_minscore.RDS")
