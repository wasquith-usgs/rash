\name{setlog.h}
\alias{setlog.h}
\title{ Set a Logical Value in a Hash }
\description{
This function sets a logical value into a rash for the given key.
}
\usage{
setlog.h(key, val, hash)
}
\arguments{
  \item{key}{A key for the hash, which will be cast into \code{as.character()}.}
  \item{val}{The value to associate with the key, which is coerced by \code{as.logical()}.}
  \item{hash}{An \R environment but for purposes here a hash.}
}
\value{
  This function is used for its side effects.
}
\author{W.H. Asquith, Spring 2009}
\examples{
my.new.hash <- new.h()
setlog.h("author",  "324", my.new.hash)
setlog.h("keyword", 212332, my.new.hash)
}
\keyword{misc}
