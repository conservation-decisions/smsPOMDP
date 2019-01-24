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
Prior action, integer (1 for manage, 2 for survey, 3 for nothing)
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
state_prior = c(1,0)
t = TigerPOMDP::tr(p0, pm, d0, d, V, Cm, Cs) #transition matrix
o = TigerPOMDP::obs(p0, pm, d0, d, V, Cm, Cs)#observation matrix

z0 = 2 #Not seen
a0 = 1 #manage
update_belief(state_prior, t, o, z0, a0)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
