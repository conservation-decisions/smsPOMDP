\name{minigraph}
\alias{minigraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
minigraph(man, sur, size = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{man}{
%%     ~~Describe \code{man} here~~
}
  \item{sur}{
%%     ~~Describe \code{sur} here~~
}
  \item{size}{
%%     ~~Describe \code{size} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (man, sur, size = 1) 
{
    library(imager)
    if (sur > 0) {
        par(mfrow = c(1, 1))
        m = paste0(man, " years")
        s = paste0(sur, " years")
        t = load.image(file = paste(path.package("TigerTest"), 
            "/extdata/im1.jpg", sep = ""))
        plot(t, axes = F)
        text(200, 200, "Not seen for", cex = size)
        text(200, 250, m, cex = size)
        text(500, 400, "Seen", cex = size)
        text(600, 700, "Not seen for", cex = size)
        text(600, 750, s, cex = size)
        text(1000, 200, "Seen", cex = size)
    }
    else {
        par(mfrow = c(1, 1))
        m = paste0(man, " years")
        t = load.image(file = paste(path.package("TigerTest"), 
            "/extdata/im2.jpg", sep = ""))
        plot(t, axes = F)
        text(550, 400, "Not seen for", cex = size)
        text(550, 450, m, cex = size)
        text(1000, 200, "Seen", cex = size)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
