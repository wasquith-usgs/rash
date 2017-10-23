\name{setnum.h}
\alias{setnum.h}
\title{ Set a Numeric Value in a Hash }
\description{
This function sets a numeric value into a hash for the given key.
}
\usage{
setnum.h(key, val, hash)
}
\arguments{
  \item{key}{A key for the hash, which will be cast into \code{as.character()}.}
  \item{val}{The value to associate with the key, which is coerced by \code{as.numeric()}.}
  \item{hash}{An \R environment but for purposes here a hash.}
}
\value{
  This function is used for its side effects.
}
\author{W.H. Asquith, Spring 2009}
\examples{
my.new.hash <- new.h()
setnum.h("author",  "324.234", my.new.hash)
setnum.h("keyword", 212332, my.new.hash)
}
\keyword{misc}
