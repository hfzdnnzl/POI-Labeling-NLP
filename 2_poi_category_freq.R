# 1.0 Get category frequency
library(tm)
poi_data = readRDS("data/poi_clean_name.RDS")
# sub category
category = gsub(" ","__",poi_data$category)
corpus = VCorpus(VectorSource(category))
matrix = as.matrix(TermDocumentMatrix(corpus))
words = sort(rowSums(matrix),decreasing = T)
df = data.frame(word = names(words), freq=words)
rownames(df) = NULL
remove(corpus,matrix)
df$word = gsub("__"," ",df$word)
saveRDS(df,"data/sub_category_freq.RDS")
