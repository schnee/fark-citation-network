library(rvest)
library(dplyr)

#theUrl <- "http://www.fark.com/comments/9285456?cpp=1"

#theUrl <- "http://www.fark.com/comments/9284915?cpp=1"

theUrl <-"http://www.fark.com/comments/9284533?cpp=1"

# this one has a double quote near the end - dig into this
# commentTable[[100]]
#theUrl <- "http://www.fark.com/comments/9286319?cpp=1"

#theUrl <- "http://www.fark.com/comments/9286230?cpp=1"

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
    quotor = commentAuthor
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
    
    lapply(quotedNodes, extractLinkingFromNode, commentAuthor, commentId)
    # 
    # list(
    #   #the path to the comment that was quoted
    #   quotedComment=quotedNodes %>%
    #     html_nodes(xpath="strong/a") %>%
    #     html_attr("href") %>%
    #     sub("http.*#","",., perl=TRUE ),
    # 
    #   #comment id
    #   comment = commentId,
    # 
    #   #who was quoted
    #   quotedAuthor = quotedNodes %>%
    #     html_nodes(xpath="strong/a") %>%
    #     html_text(),
    # 
    #   #who quoted
    #   quotor = commentAuthor
    # )
  } else {
    list(
      #here we're creating a loopback to the comment. this is a bit
      #of hack
      quotedComment=ctab %>% 
        html_attr("id") %>% 
        sub("ctable", "c", .),
      #comment id
      comment = commentId,
      #who was quoted
      quotedAuthor = NA,
      #who quoted
      quotor = commentAuthor
    )
  } 
}

links = lapply(commentTables, makeLink)
df <- data.frame(matrix(unlist(links), ncol=4, byrow=T))
colnames(df) <- c("quotedComment", "comment", "quotedAuthor", "quotor")
#df <- bind_rows(links)

library(igraph)
library(scales)

# reverse the order so that the direction is from the quoting to the quoted
# simplify will remove the looping edges that we inserted in the 
# hack in makeLinks
network <- simplify(graph_from_data_frame(df[2:1]))

cl <- clusters(network)
pr <- page.rank(network)$vector

vertex_attr(network, "label") <- df$quotor

plot(network, 
     vertex.color=cl$membership+1L,
     edge.arrow.size=.2,
     vertex.label=NA, #V(network)$label,
     vertex.label.cex=0.9,
     vertex.size=rescale(pr, to=c(2,10)),
     edge.arrow.width=0.5)

dg <- decompose.graph(network)

lsg <- dg[[which.max(sapply(dg,vcount))]]

plot(lsg, 
     edge.arrow.size=.2,
     vertex.label=V(lsg)$label,
     vertex.label.cex=0.9,
     vertex.size=2,
     edge.arrow.width=0.5)
