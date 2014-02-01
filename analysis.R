library(igraph)
library(plyr)
library(ggplot2)

# Get files names
setwd("/Users/Bill/Dropbox/math119/enron_emails")
files <- list.files(pattern="-[a-z]{1}.csv")

# Construct data, add columns
dataset <- ldply(files[1:2], read.csv, header=T, sep  = ",") # this will take a while
dataset <- mutate(dataset, Message = as.character(Message))
dataset <- mutate(dataset, MessageLength = nchar(Message))
dataset <- mutate(dataset, Date = strptime(dataset[, "Date"], 
                  format = "%a, %d %b %Y %X"))

edges <- dataset[c("From", "To")] # select all rows, only From, To columns
g <- graph.data.frame(edges, directed = FALSE)
lc <- largest.cliques(g)
g.lc <- induced.subgraph(g, lc[[2]])
plot(g.lc, layout=layout.fruchterman.reingold, vertex.color="gray60", 
     vertex.size = 1, edge.arrow.size = 0.5, edge.color = "gray80")

# Simple plots
ggplot(dataset) + geom_bar(aes(x = Date$hour), binwidth = 1)
ggplot(dataset) + geom_bar(aes(x = MessageLength), binwidth = 100)
