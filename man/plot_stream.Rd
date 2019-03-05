\name{plot_stream}
\alias{plot_stream}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plot history of belief states
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Plots succession of actions, observations and belief state given the list of past actions and observations. Mostly used by  \code{\link{run_application}}
}
\usage{
plot_stream(p0, pm, d0, d, V, Cm, Cs, s, act, obs, disc = 0.95, size = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p0}{
Local probability of persitance : P(extant/extant, survey or nothing). Number between 0 and 1
}
  \item{pm}{
Local probability of persitance if managed : P(extant/extant, manage). Number between 0 and 1
}
  \item{d0}{
Local probability of detection : P(present/extant, nothing or manage). Number between 0 and 1
}
  \item{d}{
Local probability of detection if surveyed : P(present/extant, survey). Number between 0 and 1
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
  \item{s}{
Initial belief state, vector of 2 values (belief state extant and extinct), between 0 and 1.
}
  \item{act}{
List of past actions. Values in c('Manage','Survey','Nothing'). Has to have the same length as obs.
}
  \item{obs}{
List of past observations. Values in c('Seen',''Not_seen'). Has to have the same length as act.
}
  \item{disc}{
Discount factor used to compute the policy (default 0.95)
}
  \item{size}{
Size of the text on the graph (default 1)
}
\value{
Plot.
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

s = c(1,0)
act = c('Manage', 'Manage', 'Survey','Nothing')
obs = c('Not_seen','Not_seen','Not_seen','Seen')
plot_stream(p0, pm, d0, d, V, Cm, Cs, s, act, obs, disc = 0.95, size = 1)
}
}
