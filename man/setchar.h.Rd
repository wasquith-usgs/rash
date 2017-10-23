\name{setchar.h}
\alias{setchar.h}
\title{ Set a Character Value in a Hash }
\description{
This function sets a character value into a hash for the given key.
}
\usage{
setchar.h(key, val, hash)
}
\arguments{
  \item{key}{A key for the hash, which will be cast into \code{as.character()}.}
  \item{val}{The value to associate with the key, which is coerced by \code{as.character()}.}
  \item{hash}{An \R environment but for purposes here a hash.}
}
\value{
  This function is used for its side effects.
}
\author{W.H. Asquith, Spring 2009}
\examples{
my.new.hash <- new.h()
setchar.h("author",  "324", my.new.hash)
setchar.h("keyword", 212332, my.new.hash)
setchar.h("keyword2", "212332.dfasdf", my.new.hash)
}
\keyword{misc}
