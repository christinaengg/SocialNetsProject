library(dplyr)
#Bakeries in PA-review and business data
pa_reviews = read.csv('bakeries_pa_reviews.csv')
pa_bakeries = read.csv('yelp_bakeries_pa.csv')

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
as.list(pa_bakeries$attributes)
levels(pa_bakeries$attributes)
as.data.frame(l)

#Create adjacency matrix
#http://www.shizukalab.com/toolkits/sna/weighted-edgelists
library(igraph)
m = as.matrix(df_pa3)
df_pa3[,1]=as.character(df_pa3[,1]) 
df_pa3[,2]=as.character(df_pa3[,2])
g = graph.edgelist(m[, 1:2], directed = FALSE)
E(g)$weight=as.numeric(m[, 5])
V(g)$label = as.character(pa_bakeries$name[match(V(g)$name, pa_bakeries$business_id)])
V(g)$city = as.character(pa_bakeries$city[match(V(g)$name, pa_bakeries$business_id)])
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

# installing and loading rgexf package to create the gephi object		
# install.packages('rgexf')		
library(rgexf)		
# saveAsGEXF(g, filepath = "g.gexf") not working		

# manually creating the rgexf object from igraph nodes and edges		
# http://gopalakrishna.palem.in/iGraphExport.html#GexfExport		
nodes <- data.frame(cbind(V(g), as.character(V(g))))		
edges <- t(Vectorize(get.edge, vectorize.args='id')(g, 1:ecount(g)))		

# combine all node attributes into a matrix (and take care of & for xml)		
vAttrNames <- setdiff(list.vertex.attributes(g), "label")		
nodesAtt <- data.frame(sapply(vAttrNames, function(attr) sub("&", "&#038;",get.vertex.attribute(g, attr))))		

# combine all edge attributes into a matrix (and take care of & for xml)		
eAttrNames <- setdiff(list.edge.attributes(g), "weight") 		
edgesAtt <- data.frame(sapply(eAttrNames, function(attr) sub("&", "&#038;",get.edge.attribute(g, attr))))		

# combine all graph attributes into a meta-data		+summary(g)
graphAtt <- sapply(list.graph.attributes(g), function(attr) sub("&", "&#038;",get.graph.attribute(g, attr)))
