#' Finds the shortest path from initial node to every other node in the graph using Dijkstra's Algorithm.
#' @param graph,source_node Takes in two arguments, the first one is a dataframe passed to graph argument and second one is the starting node to find the shortest minimal_pathance from.
#' @author Vinod kumar Dasari and Gowtham Kukkemane Mahalingabhat
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s algorithm
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




dijkstra <- function(graph,source_node){
  if(is.data.frame(graph) && is.numeric(source_node) 
    && which(graph[,"v1"] == source_node) && which(graph[,"v2"] == source_node)) {
    

    nodes <- union(graph[,"v1"],graph[,"v2"])
    minimal_path <- rep(Inf,length(nodes))
    prev <- rep(FALSE,length(nodes))
    
    minimal_path[which(nodes == source_node)] <- 0
    
    i <- 1
    while(length(nodes) != 0){
      
      x <- which(minimal_path == min(minimal_path[nodes]))
      
      nodes <- nodes[-which(nodes == x)]
      
      for(y in nodes){
        
        next_nodes <- graph[which(graph[,"v1"] == x), c("v2","w")]
        
        if(any(next_nodes[,"v2"] == y)){
          
          alt <- minimal_path[x] + next_nodes[which(next_nodes[,"v2"]==y ), "w" ]
          if(alt < minimal_path[y]){
            minimal_path[y] <- alt
            prev[y] <- x
          }
          
        }
                            
      }
      i <- i + 1
      
    }
    return(minimal_path)
    
  } 
  else{
    stop("Given arguments are invalid")
  }
}
