\name{past_obs}
\alias{past_obs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List of past observations
}
\description{
Called by the shinyApp (run_application). Returns a list of the past actions given the input
}
\usage{
past_obs(input)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
input items of the shinyApp (run_application)
}
}
\value{
Vector of the past observations. Values in c('Seen', 'Not_seen')
}
\author{
Luz Pascal
}
