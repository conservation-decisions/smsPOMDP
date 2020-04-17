\name{tab_actions}
\alias{tab_actions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute policy if species not seen
}
\description{
Computes the best policy to follow if the species is not seen. Summarized in a data.frame
}
\usage{
tab_actions(transition, observation, reward, state_prior, disc = 0.95, Tmax = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{transition}{
Transition matrix between states, can be computed using \code{\link{tr}}
}
  \item{observation}{
observation matrix for each action, can be computed using \code{\link{obs}}
}
  \item{reward}{
reward matrix, can be computed using \code{\link{rew}}
}
  \item{state_prior}{
Initial belief state, vector of 2 values (belief state extant and extinct), between 0 and 1.
}
  \item{disc}{
Discount factor used to compute the policy (default 0.95)
}
 \item{Tmax}{
Maximal horizon time step
}
}
\value{
data.frame summarising the policy if the species is not seen. actions (1 = Manage, 2 = Survey, 3 = Stop) and number of successive years per action.
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

tab_actions(t, o, r, state_prior)
}
}
