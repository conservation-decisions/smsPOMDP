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
#values of number of years of management and survey found for Sumatran tiger
minigraph(10,2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
