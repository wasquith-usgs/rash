\name{del.h}
\alias{del.h}
\title{ Delete a Key-Value Pair in a Hash }
\description{
This function deletes a key-value pairing within a hash.
}
\usage{
del.h(key, hash)
}
\arguments{
  \item{key}{A key for the hash, which will be cast into \code{as.character()} before using the \code{assign()} function.}
  \item{hash}{An \R environment but for purposes here a hash.}
}
\value{
  The function returns \code{TRUE} or \code{FALSE} depending upon the presence and presumably the effect of deletion.
}
\author{W.H. Asquith, Spring 2009}
\examples{
my.new.hash <- new.h()
set.h("author",  "william", my.new.hash)
set.h("keyword", "hail", my.new.hash)
del.h("keyword", my.new.hash)
}
\keyword{misc}
