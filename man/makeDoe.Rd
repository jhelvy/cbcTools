% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeDoe.R
\name{makeDoe}
\alias{makeDoe}
\title{Make a design of experiment}
\usage{
makeDoe(levels, type = NULL, nTrials = NA)
}
\arguments{
\item{levels}{A named list of vectors defining each attribute (the names)
and each level for each attribute (the vectors). For example, a design
with two attributes, "price" and "type", that each had three levels should
be defined as \code{levels = list(price = c(1, 2, 3), type = c(1, 2, 3))}.}

\item{type}{The type of design. Defaults to \code{NULL}, which returns a full
factorial design. Set to \code{"D"}, \code{"A"}, and \code{"I"} to obtain a fractional
factorial design with either D, A, or I optimality criteria.}

\item{nTrials}{The number of trials to be used in a fractional factorial
design. Defaults to \code{NA}, but must be a number less than the number of
alternatives in the full factorial design if the \code{type} argument is anything
other than \code{NULL}.}
}
\value{
Returns a full factorial or fraction factorial design of experiment.
}
\description{
Make a design of experiment
}
\examples{
# Define the attributes and levels
levels <- list(
  price     = seq(1, 4, 0.5), # $ per pound
  type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
  freshness = c('Excellent', 'Average', 'Poor')
)

# Make a full-factorial design of experiment
doe <- makeDoe(levels)

# Make a fraction-factorial design of experiment based on D-efficiency
doe <- makeDoe(levels, type = "D", nTrials = 100)
}
