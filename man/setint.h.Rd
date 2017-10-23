\name{setint.h}
\alias{setint.h}
\title{ Set an Integer Value in a Hash }
\description{
This function sets an integer value into a hash for the given key.
}
\usage{
setint.h(key, val, hash)
}
\arguments{
  \item{key}{A key for the hash, which will be cast into \code{as.character()}.}
  \item{val}{The value to associate with the key, which is coerced by \code{as.integer()}.}
  \item{hash}{An \R environment but for purposes here a hash.}
}
\value{
  This function is used for its side effects.
}
\author{W.H. Asquith, Spring 2009}
\examples{
my.new.hash <- new.h()
setint.h("author",  "324", my.new.hash)
setint.h("keyword", 212332, my.new.hash)
}
\keyword{misc}
