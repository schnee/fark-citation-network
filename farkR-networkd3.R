library(networkD3)

d3network <- igraph_to_networkD3(network, group = cl$membership)

d3network$nodes <- d3network$nodes %>% left_join(y = pageRanks, by = c("name" = "comment"))

d3network$nodes$pageRank <- rescale(d3network$nodes$pageRank, to=c(20,500))

forceNetwork(Links = d3network$links, Nodes = d3network$nodes,
             NodeID = 'name', Group = 'group', Nodesize = 'pageRank',
             opacity = .8, fontSize = 20)

