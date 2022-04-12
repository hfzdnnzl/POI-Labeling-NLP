
# read data
############
# edit below
fsq = readRDS("data/fsq_all.RDS")
fsq_name = fsq$name
# fsq_name = fsq$name[1:1000]
# edit above
############

# Getting function source
source("predict_function.R")
# clean data
fsq_name = clean_names(fsq_name)
# predict
pred_df = predict_poi(fsq_name)
# clean dataframe
pred_df$fsq_name = fsq$name
pred_df$fsq_class = fsq$type
# pred_df$fsq_name = fsq$name[1:1000]
# pred_df$fsq_class = fsq$type[1:1000]
names(pred_df)[1] = "cleaned_name"
# save dataframe
fsq$p_class = pred_df$p_class
fsq$p_score = pred_df$p_score
saveRDS(fsq,"data/fsq_predicted.RDS")
saveRDS(pred_df,"data/fsq_pred_score.RDS")
# unpredicted length
nrow(pred_df[pred_df$p_score==0,])
nrow(pred_df[pred_df$p_score>0&pred_df$p_score<=1,])
nrow(pred_df[pred_df$p_score>1&pred_df$p_score<=2,])
nrow(pred_df[pred_df$p_score>2&pred_df$p_score<=3,])
nrow(pred_df[pred_df$p_score>3&pred_df$p_score<=4,])
nrow(pred_df[pred_df$p_score>4&pred_df$p_score<=5,])
