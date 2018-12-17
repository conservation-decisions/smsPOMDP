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
graph(transition, observation, reward, discount = 0.95, state_prior = c(1,0), verbose = TRUE, log_dir = tempdir(), log_data = NULL, cache = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{transition}{
  Transition matrix between states for each action : manage, survey and nothing. State 1 : extant, state 2 : extinct. Can be computed using \code{\link{tr}}
}
  \item{observation}{
   observation matrix for each action : manage, survey and nothing.
State 1 : extant, state 2 : extinct.
Observation 1 : seen, observation 2 : not seen.
Can be computed using \code{\link{obs}}
}
  \item{reward}{
  Reward matrix of the problem (rows for actions and cols for states). Can be computed using \code{\link{rew}}
}
  \item{discount}{
  Discount used to compute the value (default 0.95)
}
  \item{state_prior}{
  Initial belief state, vector of 2 values (belief state extant and extinct), between 0 and 1.
}
  \item{verbose}{
  logical, should the function include a message with pomdp diagnostics (timings, final precision, end condition)
}
\item{log_dir}{
pomdpx and policyx files will be saved here, along with a metadata file
}
\item{log_data}{
a data.frame of additional columns to include in the log, such as model parameters. A unique id value for each run can be provided as one of the columns, otherwise, a globally unique id will be generated.
}
\item{cache}{
should results from the log directory be cached? Default TRUE. Identical functional calls will quickly return previously cached alpha vectors from file rather than re-running.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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

#transition, observation and reward matrices
Tr = tr(p0, pm, d0, d, V, Cm, Cs)
Ob = obs(p0, pm, d0, d, V, Cm, Cs)
Re = rew(p0, pm, d0, d, V, Cm, Cs)


graph(Tr, Ob, Re)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
