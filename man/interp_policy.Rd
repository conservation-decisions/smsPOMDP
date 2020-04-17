\name{interp_policy}
\alias{interp_policy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes the best policy
}
\description{
Given a policy and an initial belief state, returns the action which maximizes the value function, and its corresponding value
}
\usage{
interp_policy(state_prior, alpha, alpha_action)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state_prior}{
Initial belief state, vector of 2 values (belief state extant and extinct), between 0 and 1.
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
#values for Sumatran tigers
pen <- 0.1
p0 <- 1-pen
pem <- 0.05816
pm <- 1 - pem
V <- 175.133
Cm <- 18.784
Cs <- 10.840
d0 <- 0.01
dm <- 0.01
ds <- 0.78193

#buiding the matrices of the problem
t <- smsPOMDP::tr(p0, pm, d0, dm, ds, V, Cm, Cs) #transition matrix
o <- smsPOMDP::obs(p0, pm, d0, dm, ds, V, Cm, Cs)#observation matrix
r <- smsPOMDP::rew(p0, pm, d0, dm, ds, V, Cm, Cs)#reward matrix

state_prior <- c(1,0) #initial belief state
log_dir <- tempdir()
id <- digest::digest(match.call())
infile <- paste0(log_dir, "/", id, ".pomdpx")
outfile <- paste0(log_dir, "/", id, ".policyx")
stdout <- paste0(log_dir, "/", id, ".log")

sarsop::write_pomdpx(t, o, r, disc, state_prior, file = infile)
status <- sarsop::pomdpsol(infile, outfile, stdout = stdout)
policy <- sarsop::read_policyx(file = outfile)
output <- smsPOMDP::interp_policy(state_prior,policy$vectors,policy$action)

  }
}

