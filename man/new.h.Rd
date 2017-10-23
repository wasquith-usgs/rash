\name{new.h}
\alias{new.h}
\title{ A New Hash }
\description{
This function creates a hash, which for the \pkg{rash} package is simply a new environment in \R that is hashed.
}
\usage{
new.h(...)
}
\arguments{
  \item{...}{Advanced arguments to pass to \code{new.env()}.}
}
\value{
  A hash is returned.
}
\author{W.H. Asquith, Spring 2009}
\examples{
my.new.hash <- new.h()
}
\keyword{misc}
