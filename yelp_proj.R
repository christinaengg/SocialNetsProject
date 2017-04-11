library(dplyr)
#Bakeries in PA-review and business data
pa_reviews = read.csv('C:/R/Yelp/bakeries_pa_reviews.csv')
pa_bakeries = read.csv('C:/R/Yelp/yelp_bakeries_pa.csv')

#Merge dataframes based on review data set and make subset with only relevant data
df_pa = merge(x=pa_reviews, y=pa_bakeries, by = "business_id", all.x = TRUE)
df_pa = subset(df_pa, select = c("business_id", "name", "user_id"))

#Index user_id and add to dataframe
u = unique(df_pa['user_id'])
u$user_index = seq_len(nrow(u)) + 5000
df_pa = merge(x=df_pa, y=u, by = "user_id", all.x=TRUE)
df_pa$user_id <- NULL

#Index business_id and add to dataframe
b = unique(df_pa['business_id'])
b$business_index = seq_len(nrow(b)) 
df_pa = merge(x=df_pa, y=b, by="business_id", all.x=TRUE)
df_pa$business_id <- NULL

#Create a subset of df_pa
df_pa1 = subset(df_pa, select=c('user_index', 'business_index', 'name'))

#Join dataframes
df_pa2 = merge(x=df_pa, y=df_pa1, by="user_index", all.x = TRUE)
df_pa2 = df_pa2 %>% arrange(user_index, business_index.x, business_index.y)
                           
#Group by business_index to obtain count
df_pa3 = df_pa2 %>% arrange(business_index.x,business_index.y)  %>% group_by(business_index.x, business_index.y, name.x, name.y) %>% summarise(user_count = n())

#Clean dataframe from duplicates
#http://stackoverflow.com/questions/32035865/r-remove-rows-from-a-data-frame-that-contain-a-duplicate-of-either-combination-o
df_pa3 = df_pa3 %>% filter(business_index.x != business_index.y) 
df_pa3 = df_pa3[!duplicated(t(apply(df_pa3, 1, sort))),]

#Filter out weights-optional
df_pa4 = df_pa3 %>% filter(user_count>1)

#Create adjacency matrix
#http://www.shizukalab.com/toolkits/sna/weighted-edgelists
library(igraph)
m = as.matrix(df_pa3)
g = graph.edgelist(m[, 3:4], directed = FALSE)
E(g)$weight=as.numeric(m[, 5])
adj = get.adjacency(g, attr='weight')


