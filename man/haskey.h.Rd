\name{haskey.h}
\alias{haskey.h}
\title{ Does a Key Exist in a Hash }
\description{
This function determines the presence of a key in a hash.
}
\usage{
haskey.h(key, hash)
}
\arguments{
  \item{key}{A key for the hash, which will be cast into \code{as.character()}.}
  \item{hash}{An \R environment but for purposes here a hash.}
}
\value{
  The function returns \code{TRUE} or \code{FALSE} depending upon the presence of the key in the hash.
}
\author{W.H. Asquith, Spring 2009}
\examples{
my.new.hash <- new.h()
set.h("author",  "william", my.new.hash)
haskey.h("publisher", my.new.hash)
haskey.h("author", my.new.hash)
}
\keyword{misc}
