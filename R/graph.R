#' @export
graph = function(p0, pm, d0, d, V, Cm, Cs, disc=0.95, size = 1){

  #tests the inputs
  stopifnot(p0>=0,p0<=1) #checks if p0 is a probability
  stopifnot(pm>=0,pm<=1) #checks if pm is a probability
  stopifnot(d0>=0,d0<=1) #checks if d0 is a probability
  stopifnot(d>=0,d<=1) #checks if d is a probability
  stopifnot(V>=0, Cm >= 0, Cs >= 0) #checks if values and costs are positif
  stopifnot(disc>=0, disc <= 1) #checks if the discount factor is between 0 and 1

  #buiding the matrices of the problem
  t = TigerPOMDP::tr(p0, pm, d0, d, V, Cm, Cs) #transition matrix
  o = TigerPOMDP::obs(p0, pm, d0, d, V, Cm, Cs)#observation matrix
  r = TigerPOMDP::rew(p0, pm, d0, d, V, Cm, Cs) #reward matrix

  state_prior = c(1,0) #initial belief state
  log_dir = tempdir()
  id <- digest::digest(match.call())
  infile <- paste0(log_dir, "/", id, ".pomdpx")
  outfile <- paste0(log_dir, "/", id, ".policyx")
  stdout <- paste0(log_dir, "/", id, ".log")
  graphout <- paste0(log_dir, "/", id, ".dot")

  sarsop::write_pomdpx(t, o, r, disc, state_prior, file = infile)
  status <- sarsop::pomdpsol(infile, outfile, stdout = stdout)
  g = sarsop::polgraph(infile, outfile, max_depth = 100, min_prob = 0.0001,
                       max_branches = 100, output = graphout)
  #analysing the graph and removing useless information
  g = readChar(graphout, file.info(graphout)$size)
  g = stringr::str_replace(g, 'shape=doublecircle', '')
  g = stringr::str_replace(g, '  labeljust=\"l\"', '')
  g = strsplit(g, split = '\r\n')
  g = unlist(g)

  #limit between nodes and edges in the file
  i = which(grepl('root -> root', g))

  s = function(char){return(strsplit(char, split = ' '))}
  nodes = g[3:(i-1)]
  nodes = unlist(lapply(nodes,s))
  nodes = matrix(nodes, ncol = 5, byrow = T)
  action = substr(nodes[,5], 2, 3)
  nodes = data.frame(name = nodes[,1], action = action)
  edges = g[i:(length(g)-1)]
  edges = unlist(lapply(edges,s))
  edges = matrix(edges, ncol = 6, byrow = T)
  obs = substr(edges[,5], 2, 3)
  edges = data.frame(from = edges[,1], dest = edges[,3], obs =obs)

  #looking for the number of years of management before survey
  act = as.character(nodes$action[1])
  node = as.character(nodes$name[1])
  compt = 0
  while(act == 'a1'){
    q = edges[which(edges$from == node),]
    node = as.character(q[which(q$obs == 'o1'),]$dest)
    act = as.character(nodes[which(nodes$name == node),]$action)
    compt = compt+1
  }
  compt2 = 0
  while(act == 'a2'){
    q = edges[which(edges$from == node),]
    node = as.character(q[which(q$obs == 'o1'),]$dest)
    act = as.character(nodes[which(nodes$name == node),]$action)
    compt2 = compt2+1
  }
  return(TigerPOMDP::minigraph(compt, compt2, size))
}
