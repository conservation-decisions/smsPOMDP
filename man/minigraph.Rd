\name{minigraph}
\alias{minigraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots decision graph
}
\description{
Plots optimal policy graph given a data.frame summarizing the optimal policy
}
\usage{
minigraph(tab, tab2=NULL, size = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{
data.frame resuming the actions to be lead, the order and the number of years, in which they should be lead if the species is not seen, usually used if the initial belief state is c(1,0) (species known extant). Can be computed using \code{\link{tab_actions}} with c(1,0) as initial belief state
}
  \item{tab2}{
data.frame resuming the actions to be lead, the order and the number of years, in which they should be lead if the species is not seen, usually added if the initial belief state is not c(1,0) (species not known extant). Can be computed using \code{\link{tab_actions}}
}
  \item{size}{
Size of the text on the graph
}
}
\details{
Called by graph function.
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
pen <- 0.1
p0 <- 1-pen
pem <- 0.05816
pm <- 1 - pem
V <- 175.133
Cm <- 18.784
Cs <- 10.840
d0 <- 0.01
d <- 0.78193

#buiding the matrices of the problem
t <- smsPOMDP::tr(p0, pm, d0, d, V, Cm, Cs) #transition matrix
o <- smsPOMDP::obs(p0, pm, d0, d, V, Cm, Cs)#observation matrix
r <- smsPOMDP::rew(p0, pm, d0, d, V, Cm, Cs) #reward matrix

state_prior <- c(1,0)
tab <- smsPOMDP::tab_actions(t, o, r, state_prior, disc)
smsPOMDP::minigraph(tab)

state_prior <- c(0.3,0.7)
tab <- smsPOMDP::tab_actions(t, o, r, c(1, 0), disc)
tab2 <- smsPOMDP::tab_actions(t, o, r, state_prior, disc)
smsPOMDP::minigraph(tab, tab2)
}
}
