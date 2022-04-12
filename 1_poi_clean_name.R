# 1.0 Data cleaning function
# 1.1 Call library
library(stringr)
library(tm)
library(sf)
library(dplyr)
# 1.2 Remove non-ASCII characters
poi_data = readRDS("data/poi_kv_full_data.RDS")
poi_data$poi_name = gsub("[^\x01-\x7F]"," ",poi_data$poi_name)
poi_data$poi_name = gsub("[^0-9A-Za-z///']"," ",poi_data$poi_name)
poi_data$poi_name = str_squish(poi_data$poi_name)
# 1.3 Cleaning using corpus
corpus = VCorpus(VectorSource(poi_data$poi_name))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)
# 1.4 Save in dataframe
name = data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)
poi_data$poi_name = name$text
# 1.5 Filter data
poi_data = poi_data %>% filter(poi_name!="")
poi_data = poi_data %>% filter(!is.na(poi_categ))
poi_data = poi_data %>% filter(!is.na(poi_categ_sub))
# 1.6 Confirm categories with osm keys and values
# osm data
osm = read.csv("data/osm_keys_and_values.csv")
osm = paste(osm$key,osm$value)
# training data
category = paste(poi_data$poi_categ,poi_data$poi_categ_sub)
category = category[!duplicated(category)]
# confirming data
category_df = data.frame(category=category,osm_category=NA,new_category=NA,stringsAsFactors = F)
for(i in 1:nrow(category_df)){
  catname = paste0("^",category_df$category[i],"$")
  if(length(grep(catname,osm))==1){
    category_df$osm_category[i] = category_df$category[i]
  }
}
# joining data
poi_data$category = paste(poi_data$poi_categ,poi_data$poi_categ_sub)
# filter data
osm_poi = category_df$category[is.na(category_df$osm_category)]
df = data.frame(stringsAsFactors = F)
for(i in 1:length(osm_poi)){
  poi_data = poi_data %>% filter(category!=osm_poi[i])
}
# save data
saveRDS(poi_data,"data/poi_clean_name.RDS")
