\name{check_pomdp}
\alias{check_pomdp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Check POMDP
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Checks if input matrices are compatible to form a POMDP
}
\usage{
check_pomdp(transition, observation, reward)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{transition}{
Transition matrix between states, can be computed using \code{\link{tr}}. Array, dimensions = SxSxA with S number of states and A number of actions (positive)
}
  \item{observation}{
observation matrix for each action, can be computed using \code{\link{obs}}. Array, dimensions = SxOxA with S number of states, O the number of observations and A number of actions (positive)
}
  \item{reward}{
reward matrix, can be computed using \code{\link{rew}}. Matrix of A rows and S columns, with S number of states and A number of actions (positive)
}
}

\details{
Checks the compatibility of dimensions of the input matrices (same number of states, actions), are the transition matrices stochastic and square, 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Bool, TRUE if the inputs are compatible to form a POMDP, FALSE otherwise
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

check_pomdp(t, o, r)
}
}