\name{simulations_tab}
\alias{simulations_tab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Tabulate simulations
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function for tabulating the means and standard deviation of rewards and belief state if extant for nbSimul simulations, following the optimal policy.
}
\usage{
simulations_tab(p0, pm, d0, dm, ds, V, Cm, Cs, state_prior, Tmax, 
                          disc = 0.95, nbSimul = 100)
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
  \item{Tmax}{
Horizon of the simulation. Integer
}
  \item{disc}{
Discount factor used to compute the policy (default 0.95)
}
  \item{nbSimul}{
  Number of simulations to run (default 100). Integer
  }
}
\value{
A data.frame. Each row corresponds to a time step. The columns are:
\item{mean_belief}{Mean of the belief state extant}
\item{up_belief}{Upper bound (95\%) of the belief state extant}
\item{low_belief}{Lower bound (95\%) of the belief state extant}
\item{mean_reward}{Mean of the discounted intant reward}
\item{up_reward}{Upper bound (95\%) of the discounted intant reward}
\item{low_reward}{Lower bound (95\%) of the discounted intant reward}

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

#Horizon of the simulation
Tmax <- 20

simulations_tab(p0, pm, d0, dm, ds, V, Cm, Cs, state_prior, Tmax)
 }

}