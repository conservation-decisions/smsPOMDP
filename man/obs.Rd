\name{obs}
\alias{obs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Observation matrix function
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Computes the observation matrix for each action : manage, survey and nothing.
State 1 : extant, state 2 : extinct.
Observation 1 : seen, observation 2 : not seen.
}
\usage{
obs(p0, pm, d0, d, V, Cm, Cs)
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
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
Array, dimensions = [2,2,3], one matrix of dimensions 2,2 per action (manage, survey, nothing, in that order).
For each matrix : rows = states, cols = observations
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Luz Pascal
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
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

obs(p0, pm, d0, d, V, Cm, Cs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
