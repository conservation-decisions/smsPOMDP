\name{sim}
\alias{sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulation function
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Generates (plot) a simulation for a given belief state : actions lead, observations, current state, posterior belief state

}
\usage{
sim(tr, obs, rew, discount, state_prior, Tmax, a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{
  Transition matrix between states for each action : manage, survey and nothing. State 1 : extant, state 2 : extinct. Can be computed using  \code{\link{tr}}
}
  \item{obs}{
   observation matrix for each action : manage, survey and nothing.
State 1 : extant, state 2 : extinct.
Observation 1 : seen, observation 2 : not seen.
Can be computed using  \code{\link{obs}}
}
  \item{rew}{
  Reward matrix of the problem (rows for actions and cols for states). Can be computed using  \code{\link{rew}}
}
  \item{discount}{
  Discount used to compute the value (default 0.95)
}
  \item{state_prior}{
  Initial belief state, vector of 2 values (belief state extant and extinct), between 0 and 1.
}
  \item{Tmax}{
  Horizon of the simulation. Integer
}
  \item{a}{
  prior action : 1 = manage, 2 = survey, 3 = nothing
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
plot of the simulation
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

#Initial belief state
b0 = c(0.9,0.1) #extant : 0.9, extinct : 0.1

#Horizon of the simulation
T = 20

#prior action
a0 = 1 #manage

sim(Tr, Ob, Re, state_prior = b0, T, a0)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
