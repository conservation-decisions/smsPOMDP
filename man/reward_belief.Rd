\name{reward_belief}
\alias{reward_belief}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Return rewards over time
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Return list of rewards over time given past actions and table of belief states
}
\usage{
reward_belief(p0, pm, d0, dm, ds, V, Cm, Cs,state_posterior, act, disc = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p0}{
Local probability of persitance : P(extant/extant, survey or stop).
}
  \item{pm}{
Local probability of persitance if manage : P(extant/extant, manage).
}
  \item{d0}{
Local probability of detection : P(present/extant, stop).
}
  \item{dm}{
Local probability of detection : P(present/extant, manage).
}
  \item{ds}{
Local probability of detection if survey : P(present/extant, survey).
}
  \item{V}{
Estimated economic value of the species ($/yr).
}
  \item{Cm}{
Estimated cost of managing ($/yr).
}
  \item{Cs}{
Estimated cost of survey ($/yr).
}
  \item{state_posterior}{
Table of belief states over time corresponding the actions. Can be computed using  \code{\link{compute_belief_list}}
}
  \item{act}{
  Vector of the past actions. Values in c('Manage', 'Survey', 'Stop')
  }
  \item{disc}{
Discount factor used to compute the policy (default 0.95)
}
}
\value{
Vector of rewards over time
}
\author{
Luz Pascal
}
\examples{
\dontrun{
#values for Sumatran tiger
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

act <- c("Manage", "Survey", "Stop")
obs <- c("Seen", "Seen", "Not seen")
state_prior <- c(1,0)

state_posterior<-smsPOMDP::compute_belief_list(p0, pm, d0, dm, ds, V, Cm, Cs, state_prior, act, obs)

rew_tab<-smsPOMDP::reward_belief(p0, pm, d0, dm, ds, V, Cm, Cs,state_posterior, act)
}
}
