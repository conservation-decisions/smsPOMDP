\name{Interp_policy}
\alias{Interp_policy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes the best policy
}
\description{
Given the policy and the initial belief state, returns the action which maximizes the value
}
\usage{
Interp_policy(initial, alpha, alpha_action)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initial}{
Initial belief state
}
  \item{alpha}{
alpha vector
}
 \item{alpha_action}{
list of actions corresponding to the alpha vector
}
}
\details{
The alpha vector and the list of actions can be computed by using the package 'sarsop' (functions pomdpsol and read_policyx)
}
\value{
List of 2 elements : optimal value and action
}
\author{
Milad Memarzadeh
}

\examples{
\dontrun{
#requires prior installation of the package sarsop and devtools
list.of.packages <- c("sarsop")
new.packages <- list.of.packages[!(list.of.packages %in% utils::installed.packages()[,"Package"])]
if(length(new.packages)>0) {
  devtools::install_github("boettiger-lab/sarsop", host = "https://api.github.com")
}
#values for Sumatran tigers
pen = 0.1
p0 = 1-pen
pem = 0.05816
pm = 1 - pem
V = 175.133
Cm = 18.784
Cs = 10.840
d0 = 0.01
d = 0.78193

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

  sarsop::write_pomdpx(t, o, r, disc, state_prior, file = infile)
  status <- sarsop::pomdpsol(infile, outfile, stdout = stdout)
  policy <- sarsop::read_policyx(file = outfile)
  output <- TigerPOMDP::Interp_policy(state_prior,policy$vectors,policy$action)

  }
}

