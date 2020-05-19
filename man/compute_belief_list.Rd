\name{compute_belief_list}
\alias{compute_belief_list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Compute list over time of belief states
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Compute list over time of belief states given past actions, observations and initial belief state
}
\usage{
compute_belief_list(p0, pm, d0, dm, ds, V, Cm, Cs, state_prior, act, obs, disc = 0.95)
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
  \item{state_prior}{
Initial belief state, vector of 2 values (belief state extant and extinct), between 0 and 1.
}
  \item{act}{
list of actions, values in c('Manage', 'Survey', 'Stop')
}
  \item{obs}{
list of observations, values in c('Seen', 'Not seen')
}
  \item{disc}{
Discount factor used to compute the policy (default 0.95)
}
}
\value{
Current belief state, vector of 2 values (belief state extant and extinct), between 0 and 1.
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

#Initial belief state
state_prior <- c(0.9,0.1) #extant : 0.9, extinct : 0.1

#previous actions and observations
ac <- c('Manage','Survey','Stop')
ob <- c('Not_seen','Not_seen', 'Seen')

smsPOMDP::compute_belief_list(p0, pm, d0, dm, ds, V, Cm, Cs, state_prior, ac, ob)
 }
}
