\name{is.h}
\alias{is.h}
\title{ Is an Object a Hash }
\description{
This function determines whether an object is a hash. A hash in package \pkg{rash} is an \R environment.
}
\usage{
is.h(hash)
}
\arguments{
  \item{hash}{An \R environment but for purposes here a hash.}
}
\value{
  The function returns \code{TRUE} or \code{FALSE} depending upon the \code{hash} is an \R environment.}
\author{W.H. Asquith, Spring 2009}
\examples{
my.new.hash <- new.h()
is.h(my.new.hash)

another.one <- rnorm(10)
is.h(another.one)
}
\keyword{misc}
