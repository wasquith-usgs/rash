\name{set.h}
\alias{set.h}
\title{ Set a Value in a Hash }
\description{
This function sets a value into a hash.
}
\usage{
set.h(key, val, hash)
}
\arguments{
  \item{key}{A key for the hash, which will be cast into \code{as.character()}.}
  \item{val}{The value to associate with the key.}
  \item{hash}{An \R environment but for purposes here a hash.}
}
\value{
  This function is used for its side effects.
}
\author{W.H. Asquith, Spring 2009}
\examples{
my.new.hash <- new.h()
set.h("author",  "william", my.new.hash)
set.h("keyword", "hail", my.new.hash)
}
\keyword{misc}
