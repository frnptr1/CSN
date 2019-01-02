setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Libraries included ------------------------------------------------------

library(igraph)
library(data.table)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(dplyr)
library(magrittr)

# Function for task 1 -----------------------------------------------------

community_analysis <- function(g) {
  # all the function names that we are going to use
  funcList <- c("edge.betweenness.community", "fastgreedy.community", "label.propagation.community", "leading.eigenvector.community", 
                   "multilevel.community", "optimal.community", "spinglass.community", "walktrap.community", "infomap.community")
  
  # initializing the matrix 
  result <- matrix(rep(0, 6*length(funcList)), nrow=length(funcList))
  # assigning names to columns of result matrix
  colnames(result) <- c("Algorithm", "TPT", "expansion", "conductance", "modularity", "#cluster")
  # locate function names we are going to use in the first column
  result[, 1] <- funcList
  number.nodes = length(V(g))
  # for each algorithm
  for (algId in seq(length(funcList))) {
    func <- get(funcList[algId])
    com <- func(g)
    ngroup <- length(unique(com$membership))
    
    # to know whether a edge connects two different communities
    crossing_edge <- crossing(com, g)
    
    # only store id of edges that connect two different communities
    true_crossing_edge <- which(crossing_edge == TRUE)
    
    # count number of edges per node leaving the cluster
    leaving_edge_per_node <- rep(0, length(V(g)))
    for (edgeId in true_crossing_edge) {
      nodes <- as.vector(ends(g, edgeId, names = FALSE))
      #print(nodes)
      leaving_edge_per_node[nodes[1]] <- leaving_edge_per_node[nodes[1]] + 1
      if (!is.directed(g)) leaving_edge_per_node[nodes[2]] <- leaving_edge_per_node[nodes[2]] + 1
    }
    
    TPT <- rep(0, ngroup)
    EXP <- rep(0, ngroup)
    COND <- rep(0, ngroup)
    # for each cluster
    for (i in seq(ngroup)) {
      # obtain the vertices of this cluster
      subvertices <- V(g)[com$membership == i]
      nc <- length(subvertices)
      # create subgraph
      subg <- induced_subgraph(g, subvertices)
      # count how many triangles a vertex is part of
      participaed_triangles <- count_triangles(subg)
      #print(participaed_triangles)
      tc <- 0
      for (pt in participaed_triangles) {
        if (pt > 0) {
          tc <- tc + 1
        }
      }
      #plot(subg)  
      frac.number.nodes = length(subvertices)/number.nodes
      TPT[i] <- tc/nc * frac.number.nodes
      fc <- sum(leaving_edge_per_node[subvertices])
      EXP[i] <- fc/nc * frac.number.nodes
      mc <- length(E(subg))
      COND[i] <- fc/(2*mc + fc) * frac.number.nodes
    }
    
    # TPT
    result[algId, "TPT"] <- sum(TPT)
    # expansion
    result[algId, "expansion"] <- sum(EXP)
    # conductance
    result[algId, "conductance"] <- sum(COND)
    # modularity
    result[algId, "modularity"] <- modularity(g, com$membership)
    result[algId, "#cluster"] <- ngroup
  }
  return(result)
}



# Test graph --------------------------------------------------------------


g <- read.graph("graph.txt", format="edgelist")
g <- as.undirected(g, mode = "collapse") 
plot(g)

result <- community_analysis(g)


# Zachary graph -----------------------------------------------------------


karate <- graph.famous("Zachary")
set.seed(1234567)
result <- community_analysis(karate)
result

# wikipedia network task 2 ------------------------------------------------

# load the network through the url
u = url("https://www.cs.upc.edu/~CSN/lab/data/wikipedia.gml")
gml = read.graph(u, format = "gml")

# run walktrap clustering algorithm to find communities
commu = cluster_walktrap(gml)
ngroup = length(unique(commu$membership))

# build a dataframe with communities and name of pages 
wiki = as.data.frame(cbind(commu$membership, vertex.attributes(gml)$label))
# add label to dataframe columns
colnames(wiki) = c("Communities", "Pages")
# convert factors in integer to sort them and check the communities created
wiki$Communities = as.numeric(as.character(wiki$Communities))
wiki$Pages = as.character(wiki$Pages)

# focus only in communities with at least 200 components
bigger_communities = as.numeric(which(table(wiki$Communities)>200))


# Text Mining -------------------------------------------------------------

# function to replace symbols with space
toSpace = content_transformer(function (x , pattern ) gsub(pattern, " ", x))


wiki_communities = function(x) {
  # pick the name of pages of a community x
  comm = wiki[wiki$Communities==x,]$Pages
  # merge the titles of all the pages in one text
  comm = paste(comm, collapse = " ")
  
  # pipeline for preparing the text 
  m = comm %>% VectorSource() %>%
    Corpus() %>% # transform the text in corpus
    try(tm_map(docs, toSpace, "("), silent = T) %>% # remove various symbols found
    try(tm_map(docs, toSpace, "("), silent = T) %>% # ...
    try(tm_map(docs, toSpace, "-"), silent = T) %>% # ...
    try(tm_map(docs, toSpace, "\\"), silent = T) %>% # ...
    try(tm_map(docs, toSpace, "/"), silent = T) %>% # ...
    try(tm_map(docs, toSpace, "."), silent = T) %>% # ...
    tm_map(content_transformer(tolower)) %>% # transform to lower case all the words
    tm_map(removeNumbers) %>% # remove numbers
    tm_map(removeWords, stopwords("english")) %>% # remove stopwords
    tm_map(removePunctuation) %>% # remove punctuation
    tm_map(stripWhitespace) %>% # remove exceeding white space
    tm_map(stemDocument) %>% # stemming the words
    TermDocumentMatrix()  %>% # create a matrix of counts
    as.matrix() # ...
  
  v = sort(rowSums(m),decreasing=TRUE) # sort the matrix
  d = data.frame(word = as.character(names(v)),freq=v) # and build a dataframe
  # finally wordcloud..
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=400, random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(8, "Dark2"))
}

lapply(X = bigger_communities, FUN = wiki_communities)

#https://www.overleaf.com/project/5bec51f9858d6b7df4c21459