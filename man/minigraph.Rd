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
minigraph(man, sur, size = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{man}{
Number of years of management
}
  \item{sur}{
Number of years of survey
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
#values of number of years of management and survey found for Sumatran tiger
minigraph(10,2)
}
