\name{minigraph}
\alias{minigraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots decision graph
}
\description{
Plots decision graph given the number of successive years per action
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
#values of number of years of management and survey found for Sumatran tiger, if state prior is c(1,0)
state_prior = c(1,0)
tab = TigerPOMDP::tab_actions(transition, observation, reward, state_prior, disc = 0.95)
minigraph(tab)

#if state prior != c(1,0)
tab = TigerPOMDP::tab_actions(transition, observation, reward, c(1,0), disc = 0.95)
tab2 = TigerPOMDP::tab_actions(transition, observation, reward, c(0.6,0.4), disc = 0.95)
minigraph(tab, tab2)
}
}
