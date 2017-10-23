\name{each.h}
\alias{each.h}
\title{ Extract the Key-Value Pairs of a Hash }
\description{
This function extracts all the key-value pairs of a hash.
}
\usage{
each.h(hash)
}
\arguments{
  \item{hash}{An \R environment but for purposes here a hash.}
}
\value{
  This function returns a list of the key-value pairs of the hash.
}
\author{W.H. Asquith, Spring 2009}
\examples{
my.new.hash <- new.h()
set.h("author1", "william",  my.new.hash)
set.h("author2", "george",   my.new.hash)
set.h("author3", "theodore", my.new.hash)
each.h(my.new.hash)
}
\keyword{misc}
