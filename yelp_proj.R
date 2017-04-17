library(dplyr)
#Bakeries in PA-review and business data
pa_reviews = read.csv('bakeries_pa_reviews.csv')
pa_bakeries = read.csv('yelp_bakeries_pa.csv')

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
adj
plot(g)

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

# combine all graph attributes into a meta-data
graphAtt <- sapply(list.graph.attributes(g), function(attr) sub("&", "&#038;",get.graph.attribute(g, attr)))

# generate the gexf object
write.gexf(nodes, edges, edgesWeight=E(g)$weight, edgesAtt = edgesAtt, nodesAtt = nodesAtt, output = "g.gexf")

# exploring alternatives
# to be done after gexf object is perfect
# library(network)
# install.packages("sna")
# library(sna)
# library(ggplot2)
# install.packages("GGally")
# library(GGally)
# ggnet2(g)
