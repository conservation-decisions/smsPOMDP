\name{graph}
\alias{graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot decision graph
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Plots decision graph of the solution to the problem : When to stop managing or surveying cryptic threatened species ?
}
\usage{
graph(p0, pm, d0, d, V, Cm, Cs, state_prior, disc=0.95, size = 1)
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
  \item{state_prior}{
Initial belief state, vector of 2 values (belief state extant and extinct), between 0 and 1.
}
  \item{disc}{
Discount factor used to compute the policy (default 0.95)
}
  \item{size}{
Size of the text on the graph (default 1)
}
}
\details{
This function solves the main problem by calling the sarsop solver for POMDP.
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

#Initial belief state
state_prior = c(0.9,0.1) #extant : 0.9, extinct : 0.1

graph(p0, pm, d0, d, V, Cm, Cs, state_prior)}
}
