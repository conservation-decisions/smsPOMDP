\name{rew}
\alias{rew}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reward matrix function
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Computes the reward matrix of the problem (rows for actions and cols for states).
}
\usage{
rew(p0, pm, d0, d, V, Cm, Cs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p0}{
%%     ~~Describe \code{x} here~~
Local probability of persitance : P(extant/extant, survey or nothing). Number between 0 and 1
}
  \item{pm}{
%%     ~~Describe \code{x} here~~
Local probability of persitance if managed : P(extant/extant, manage). Number between 0 and 1
}
  \item{d0}{
%%     ~~Describe \code{x} here~~
Local probability of detection : P(present/extant, nothing or manage). Number between 0 and 1
}
  \item{d}{
%%     ~~Describe \code{x} here~~
Local probability of detection if surveyed : P(present/extant, survey). Number between 0 and 1
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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
Matrix of 3 rows and 2 columns, describing the reward (rows for actions and cols for states)
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

rew(p0, pm, d0, d, V, Cm, Cs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
