#' @export
minigraph = function(man, sur){
  if (sur >0){
    rEG <- methods::new("graphNEL", nodes=c("Manage", "Survey", "Surrender"), edgemode="directed")
    rEG <- graph::addEdge("Manage", "Survey", rEG, 1)
    rEG <- graph::addEdge("Survey", "Manage",rEG, 1)
    rEG <- graph::addEdge("Survey", "Surrender",rEG, 1)
    rEG <- graph::addEdge("Surrender", "Manage",rEG, 1)
    eAttrs <- list()
    l1 = paste('Not seen for ', man, ' years', sep = '')
    l2 = paste('Not seen for ', sur, ' years', sep = '')
    eAttrs$label <- c("Manage~Survey"=l1, "Survey~Manage"="Seen", "Survey~Surrender"=l2,"Surrender~Manage"="Seen")
    plot(rEG, recipEdges="distinct",edgeAttrs=eAttrs)
  } else {
    rEG <- graph::new("graphNEL", nodes=c("Manage", "Survey", "Surrender"), edgemode="directed")
    rEG <- graph::addEdge("Manage", "Surrender", rEG, 1)
    rEG <- graph::addEdge("Surrender", "Manage",rEG, 1)
    eAttrs <- list()
    l1 = paste('Not seen for ', man, ' years', sep = '')
    eAttrs$label <- c("Manage~Surrender"=l1,"Surrender~Manage"="Seen")
    plot(rEG, recipEdges="distinct",edgeAttrs=eAttrs)

  }

}
