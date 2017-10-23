\name{get.h}
\alias{get.h}
\title{ Get a Value in a Hash (a hashed environment) }
\description{
This function retrieves the value for a key from a hash.
}
\usage{
get.h(key, hash, default=NULL)
}
\arguments{
  \item{key}{A key for the hash, which will be cast into \code{as.character()}.}
  \item{hash}{An \R environment but for purposes here a hash.}
  \item{default}{A default return value in case the key is not available in the hash.}
}
\value{
  This function returns the value or \code{NULL} if it does not exist. The function does not return a logical \code{TRUE} or \code{FALSE} because such a practice could be potentially confusing in some circumstances and lead to unexpected results of dependent code.
}
\author{W.H. Asquith, Spring 2009}
\examples{
my.new.hash <- new.h()
set.h("author",  "william", my.new.hash)
set.h("keyword", "hail", my.new.hash)
get.h("publisher", my.new.hash)
}
\keyword{misc}
