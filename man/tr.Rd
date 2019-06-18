\name{tr}
\alias{tr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transition matrix function
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Computes the transition matrix between states for each action : manage, survey and stop. State 1 : extant, state 2 : extinct
}
\usage{
tr(p0, pm, d0, d, V, Cm, Cs)
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
%%     ~~Describe \code{x} here~~
Estimated economic value of the species ($/yr).
}
  \item{Cm}{
%%     ~~Describe \code{x} here~~
Estimated cost of managing ($/yr).
}
  \item{Cs}{
%%     ~~Describe \code{x} here~~
Estimated cost of survey ($/yr).
}
}
\value{
%%  ~Describe the value returned
Array, dimensions = [2,2,3], one matrix of dimensions 2,2 per action (manage, survey, stop, in that order)
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

tr(p0, pm, d0, d, V, Cm, Cs)}
}
