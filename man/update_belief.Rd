\name{update_belief}
\alias{update_belief}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update belief state
}
\description{
Computes the following belief state given the current belief state, transition and observation matrices, the observation and the action
}
\usage{
update_belief(state_prior, transition, observation, z0, a0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state_prior}{
Initial belief state, vector of 2 values (belief state extant and extinct), between 0 and 1.
}
  \item{transition}{
Transition matrix between states, can be computed using \code{\link{tr}}
}
  \item{observation}{
observation matrix for each action, can be computed using \code{\link{obs}}
}
  \item{z0}{
Observation, integer (1 for seen, 2 for not seen)
}
  \item{a0}{
Prior action, integer (1 for manage, 2 for survey, 3 for stop)
}

}
\value{
Vector of 2 components : extant and extinct probabilities
}
\author{
Luz Pascal
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

state_prior <- c(1,0)

z0 = 2 #Not seen
a0 = 1 #manage
update_belief(state_prior, t, o, z0, a0)
}
}

