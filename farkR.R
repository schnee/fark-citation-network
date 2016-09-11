library(rvest)
library(dplyr)

#theUrl <- "http://www.fark.com/comments/9285456?cpp=1"

#theUrl <- "http://www.fark.com/comments/9284915?cpp=1"

#theUrl <-"http://www.fark.com/comments/9284533?cpp=1"

# this one has a double quote near the end - dig into this
# commentTable[[100]]
# theUrl <- "http://www.fark.com/comments/9286319?cpp=1"

#theUrl <- "http://www.fark.com/comments/9286230?cpp=1"

theUrl <- paste("http://www.fark.com/comments/9288789/Bondi-is-Trumps-Benghazi", "?cpp=1", sep="")

theUrl <- paste("http://www.fark.com/comments/9290006/Trump-claims-Clinton-could-shoot-someone-not-get-prosecuted-which-is-basically-what-he-said-about-himself-six-months-ago", "?cpp=1", sep="")

theUrl <- paste("http://www.fark.com/comments/9290296", "?cpp=1", sep="")

theUrl <- paste("http://www.fark.com/comments/9290998/Trump-campaign-We-demand-an-apology-from-Hillary-for-saying-half-of-our-supporters-are-racist-Hillary-Fine-Im-sorry-I-shouldnt-have-said-half-drops-mic", "?cpp=1", sep="")

# Because most of these threads wind up counting to 'potato'
potato <- read_html(theUrl)

# get all the comments
commentTables <- potato %>% html_nodes(".notctable,.notctableTF")


extractLinkingFromNode <- function(quotedNode, commentAuthor, commentId) {
  list(
    #the path to the comment that was quoted
    quotedComment=quotedNode %>%
      html_nodes(xpath="strong/a") %>%
      html_attr("href") %>%
      sub("http.*#","",., perl=TRUE ),
    
    #comment id
    comment = commentId,
    
    #who was quoted
    quotedAuthor = quotedNode %>%
      html_nodes(xpath="strong/a") %>%
      html_text(),
    
    #who quoted
    author = commentAuthor
  )
}

makeLink <- function(ctab) {
  
  commentAuthor <- ctab %>% 
    html_node(".clogin .cloginLeft a:first-of-type" ) %>% 
    html_text()
  
  commentId <- ctab %>% 
    html_attr("id") %>% 
    sub("ctable", "c", .)
  
  quotedNodes <- ctab %>% 
    html_nodes(".quotedcomment")
  
  if(length(quotedNodes) > 0) {
    # this comment contains quotes
    
    lapply(quotedNodes, extractLinkingFromNode, commentAuthor, commentId)

  } else {
    list(
      # this comment does not contain quotes
      
      # here we're creating a loopback to the comment. this is a bit of hack,
      # but it does allow for us put the solitary nodes in the network.
      quotedComment=commentId,
      comment = commentId,
      quotedAuthor = commentAuthor,
      author = commentAuthor
    )
  } 
}

links = lapply(commentTables, makeLink)
df <- data.frame(matrix(unlist(links), ncol=4, byrow=T), stringsAsFactors=F)
colnames(df) <- c("quotedComment", "comment", "quotedAuthor", "author")


library(igraph)
library(scales)

# ensure that the order of columns is such that the direction is from the
# quoting to the quoted - this helps with PageRank

# simplify will remove the looping edges that we inserted in the 
# hack in makeLinks
authorNetwork <- simplify(graph_from_data_frame(df[4:3]))
commentNetwork <- simplify(graph_from_data_frame(df[2:1]))

commentNetwork <- set_vertex_attr(commentNetwork, name = "author", 
                                  index = df$comment, value = as.vector(df$author))

network <- commentNetwork
cl <- clusters(network)
pr <- page.rank(network)$vector

# if the page ranks are over the comment network, the dataframe should have "comment"
pageRanks <- data.frame(comment=names(pr), pageRank = pr, stringsAsFactors = F)

df <- df %>% left_join(pageRanks)

topN <- df %>% top_n(0.05 * nrow(df), pageRank)

# if the network is the comment network, the index should be top25$comment
network <- set_vertex_attr(network, name = "topAuthor",
                                  index = topN$comment, value = topN$author)

mainTitle = paste(strtrim(potato %>% html_nodes("head title") %>% html_text(),
                      width = 60), "...", sep="")

plot(network, 
     vertex.color=cl$membership+1L,
     edge.arrow.size=.2,
     vertex.label=V(network)$topAuthor,
     vertex.label.cex=0.9,
     vertex.size=rescale(pr, to=c(2,10)),
     edge.arrow.width=0.5)
title(main = mainTitle,
      sub = "Comment Network (sized by page rank)")


dg <- decompose.graph(network)

lsg <- dg[[which.max(sapply(dg,vcount))]]

plot(lsg, 
     edge.arrow.size=.2,
     vertex.label=V(lsg)$topAuthor,
     vertex.label.cex=0.9,
     vertex.size=rescale(pr, to=c(2,10)),
     edge.arrow.width=0.5)
