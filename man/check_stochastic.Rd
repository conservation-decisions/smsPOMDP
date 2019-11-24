\name{check_stochastic}
\alias{check_stochastic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Check matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Check if a matrix is stochastic

}
\usage{
check_stochastic(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
matrix
}
}
\value{Bool:
TRUE if the matrix is stochastic, FALSE otherwise
}
\author{
Luz Pascal
}
\examples{
\dontrun{
mat = matrix(c(1,0,0,1), ncol = 2)
check_stochastic(mat)
}
}