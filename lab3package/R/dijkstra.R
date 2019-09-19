#' Finds the shortest path from initial node to every other node in the graph using Dijkstra's Algorithm.
#' @param graph,init_node Takes in two arguments, the first one is a dataframe passed to graph argument and second one is the starting node to find the shortest distance from.
#' @examples
#' \dontrun{ 
#' wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#' v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#' w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph,3)
#' dijkstra(wiki_graph,1)
#' }
#' @return Returns the shortest path to all the other nodes available from the initial node.
#' @export




dijkstra <- function(graph,init_node){
  if(is.data.frame(graph) && is.numeric(init_node) 
    && which(graph[,"v1"] == init_node) && which(graph[,"v2"] == init_node)) {
    
    # creating node set
    nodes <- union(graph[,"v1"],graph[,"v2"])
    dist <- rep(Inf,length(nodes))
    prev <- rep(FALSE,length(nodes))
    
    dist[which(nodes == init_node)] <- 0
    
    i <- 1
    while(length(nodes) != 0){
      
      #node with minimun distance
      x <- which(dist == min(dist[nodes]))
      
      #remove u from node
      nodes <- nodes[-which(nodes == x)]
      
      for(y in nodes){
        
        next_nodes <- graph[which(graph[,"v1"] == x), c("v2","w")]
        
        if(any(next_nodes[,"v2"] == y)){
          
          alt <- dist[x] + next_nodes[which(next_nodes[,"v2"]==y ), "w" ]
          if(alt < dist[y]){
            dist[y] <- alt
            prev[y] <- x
          }
          
        }
                            
      }
      i <- i + 1
      
    }
    return(dist)
    
  } 
  else{
    stop("Invalid inputs entered")
  }
}
