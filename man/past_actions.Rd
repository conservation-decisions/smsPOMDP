\name{past_actions}
\alias{past_actions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List of past actions
}
\description{
Called by the shinyApp (run_application). Returns a list of the past actions given the input
}
\usage{
past_actions(input)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
input items of the shinyApp (run_application)
}
}
\value{
Vector of the past actions. Values in c('Manage', 'Survey', 'Nothing')
}
\author{
Luz Pascal
}
