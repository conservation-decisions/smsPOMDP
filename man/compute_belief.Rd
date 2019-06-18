\name{compute_belief}
\alias{compute_belief}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute current belief state
}
\description{
Compute current belief state given past actions, observations and initial belief state
}
\usage{
compute_belief(p0, pm, d0, d, V, Cm, Cs, state_prior, act, obs, disc = 0.95, size = 1)
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
Local probability of detection : P(present/extant, stop or manage).
}
  \item{d}{
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
list of observations, values in c('Seen', 'Not_seen')
}
  \item{disc}{
Discount factor used to compute the policy (default 0.95)
}
  \item{size}{
Size of the text on the graph (default 1)
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
pen = 0.1
p0 = 1-pen
pem = 0.05816
pm = 1 - pem
V = 175.133
Cm = 18.784
Cs = 10.840
d0 = 0.01
d = 0.78193

#Initial belief state
state_prior = c(0.9,0.1) #extant : 0.9, extinct : 0.1

#previous actions and observations
ac = c('Manage','Survey','Stop')
ob = c('Not_seen','Not_seen', 'Seen')

compute_belief(p0, pm, d0, d, V, Cm, Cs, state_prior, ac, ob)
 }
}
