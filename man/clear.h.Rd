\name{clear.h}
\alias{clear.h}
\title{ Clear (Empty) a Hash }
\description{
This function clears or empties all of the contents of a hash.
}
\usage{
clear.h(hash)
}
\arguments{
  \item{hash}{An \R environment but for purposes here a hash.}
}
\value{
  This function is used for its side effects.
}
\author{W.H. Asquith, Spring 2009}
\examples{
my.new.hash <- new.h()
set.h("author1", "william",  my.new.hash)
set.h("author2", "george",   my.new.hash)
set.h("author3", "theodore", my.new.hash)
each.h(my.new.hash)
clear.h(my.new.hash)
each.h(my.new.hash)
}
\keyword{misc}
