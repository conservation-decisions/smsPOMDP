\name{minigraph}
\alias{minigraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots decision graph
}
\description{
Plots decision graph given the number of years of management and survey
}
\usage{
minigraph(tab, size = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{
data.frame resuming the actions to be lead, the order and the number of years, in which they should be lead if the species is not seen
}
  \item{size}{
Size of the text on the graph
}
}
\details{
Called by graph function
}
\value{
Plot.
}
\author{
Luz Pascal
}
\examples{
\dontrun{
#values of number of years of management and survey found for Sumatran tiger
tab=data.frame(action = c('a1','a2','a3'), years = c(10,2,88))
minigraph(tab)}
}
