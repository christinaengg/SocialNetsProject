library(dplyr)
#Bakeries in PA-review and business data
pa_reviews = read.csv('C:/R/Yelp/bakeries_pa_reviews.csv')
pa_bakeries = read.csv('C:/R/Yelp/yelp_bakeries_pa.csv')

#Merge dataframes based on review data set and make subset with only relevant data
df_pa = merge(x=pa_reviews, y=pa_bakeries, by = "business_id", all.x = TRUE)
df_pa = subset(df_pa, select = c("business_id", "name", "user_id"))

#Index user_id and add to dataframe
u = unique(df_pa['user_id'])

#Index business_id and add to dataframe
b = unique(df_pa['business_id'])

#Create a subset of df_pa
df_pa1 = subset(df_pa, select=c('user_id', 'business_id', 'name'))

#Join dataframes
df_pa2 = merge(x=df_pa, y=df_pa1, by="user_id", all.x = TRUE)
df_pa2 = df_pa2 %>% arrange(user_id, business_id.x, business_id.y)

#Group by business_index to obtain count
df_pa3 = df_pa2 %>% arrange(business_id.x,business_id.y)  %>% group_by(business_id.x, business_id.y, name.x, name.y) %>% summarise(user_count = n())

#Clean dataframe from duplicates
#http://stackoverflow.com/questions/32035865/r-remove-rows-from-a-data-frame-that-contain-a-duplicate-of-either-combination-o
df_pa3 = df_pa3 %>% filter(business_id.x != business_id.y) 
df_pa3 = df_pa3[!duplicated(t(apply(df_pa3, 1, sort))),]

#All attributes of business
pa_bakeries = merge(x=b, y=pa_bakeries, by="business_id", all.x = TRUE)

#Create adjacency matrix
#http://www.shizukalab.com/toolkits/sna/weighted-edgelists
library(igraph)
m = as.matrix(df_pa3)
df_pa3[,1]=as.character(df_pa3[,1]) 
df_pa3[,2]=as.character(df_pa3[,2])
g = graph.edgelist(m[, 1:2], directed = FALSE)
E(g)$weight=as.numeric(m[, 5])
V(g)$label = as.character(pa_bakeries$name[match(V(g)$name, pa_bakeries$business_id)])
V(g)$is_open=as.character(pa_bakeries$is_open[match(V(g)$name,pa_bakeries$business_id)])
V(g)$review_count=as.character(pa_bakeries$review_count[match(V(g)$name,pa_bakeries$business_id)])
V(g)$stars=as.character(pa_bakeries$stars[match(V(g)$name,pa_bakeries$business_id)])

summary(g)
V(g)$name
E(g)$weight
V(g)$label
V(g)$city
V(g)$is_open
V(g)$review_count
V(g)$stars


