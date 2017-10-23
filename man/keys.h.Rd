\name{keys.h}
\alias{keys.h}
\title{ Extract the Keys of a Hash }
\description{
This function extracts the keys of a hash.
}
\usage{
keys.h(hash)
}
\arguments{
  \item{hash}{An \R environment but for purposes here a hash.}
}
\value{
  This function returns a vector of keys of the hash.
}
\author{W.H. Asquith, Spring 2009}
\examples{
my.new.hash <- new.h()
set.h("author1", "william",  my.new.hash)
set.h("author2", "george",   my.new.hash)
set.h("author3", "theodore", my.new.hash)
keys.h(my.new.hash)
}
\keyword{misc}
