##########################
# Cleaning Name Function #
##########################
clean_names = function(names){
  # 1.2.1 Library
  library(stringr)
  library(tm)
  library(foreach)
  library(doParallel)
  # initialize cluster of cores
  numCores = detectCores()/2
  mycluster = makeCluster(numCores)
  registerDoParallel(mycluster)
  # Clean name loop
  df = foreach(i = 1:length(names), .combine = rbind, .packages = c("stringr","tm")) %dopar% {
    # Remove non-ASCII characters
    words = names[i]
    words = gsub("[^\x01-\x7F]"," ",words)
    words = gsub("[^0-9A-Za-z///']"," ",words)
    words = str_squish(words)
    # Cleaning using corpus
    corpus = VCorpus(VectorSource(words))
    corpus = tm_map(corpus, content_transformer(tolower))
    corpus = tm_map(corpus, removeNumbers)
    corpus = tm_map(corpus, removePunctuation)
    corpus = tm_map(corpus, removeWords, stopwords())
    corpus = tm_map(corpus, stemDocument)
    corpus = tm_map(corpus, stripWhitespace)
    # return dataframe
    df = data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)
  }
  # exit cluster
  on.exit(stopCluster(mycluster))
  # Save as vector
  names = df$text
  return(names)
}
##########################
# 1.0 prediction of model#
##########################
nlp_naivebayes_predict = function(name,p_score,p_class,order,dict){
  # initilize progress bar
  cat("\nPredicting category:",dict$category,
      "\nNumber of rows:",length(name),"\n")
  # opts = list(progress=progress)
  # initialize libraries
  library(stringr)
  library(foreach)
  library(doParallel)
  # initialize cluster of cores
  numCores = detectCores()/2
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
    df = data.frame(name=name[i],p_score=p_score[i],p_class=p_class[i],order=order[i])
  }
  # exit cluster
  on.exit(stopCluster(mycluster))
  # return dataframe
  return(df)
}
##########################
# Looping prediction     #
##########################
predict_poi = function(names){
  # library
  library(dplyr)
  # read data
  models = readRDS('model.RDS')
  # filter data for prediction
  predicted_df = data.frame(name = names,
                            p_score = 0,
                            p_class = 0,
                            order = 1:length(names),
                            stringsAsFactors = F)
  # starting prediction
  looping_pred = function(models,predicted_df,min_score){
    for(i in 1:length(models)){
      # display info
      cat("\nCompletion:",round(i*100/length(models), digits = 2),"%")
      cat("\nMinimum score:",min_score)
      # read model
      dict = models[[i]]
      # filter predicted
      below_score = predicted_df[predicted_df$p_score<=min_score,]
      predicted_df = predicted_df[predicted_df$p_score>min_score,]
      # exit if number of rows is zero
      if(nrow(below_score)==0){break}
      # run prediction
      new_pred = nlp_naivebayes_predict(name = below_score$name,
                                        p_score = below_score$p_score,
                                        p_class = below_score$p_class,
                                        order = below_score$order,
                                        dict = dict)
      # save in dataframe
      predicted_df = rbind(predicted_df,new_pred)
    }
    return(predicted_df)
  }
  # predict
  above_sc = predicted_df
  below_sc = data.frame(stringsAsFactors = F)
  for(i in 0:5){
    pred = looping_pred(models = models,predicted_df = above_sc,min_score = i)
    below_sc = rbind(below_sc,pred[pred$p_score<=i,])
    above_sc = pred[pred$p_score>i,]  
  }
  predicted_df = rbind(below_sc,above_sc)
  # return dataframe
  predicted_df = predicted_df[order(predicted_df$order,decreasing = F),]
  rownames(predicted_df) = NULL
  predicted_df = predicted_df %>% select(-order)
  return(predicted_df)
}