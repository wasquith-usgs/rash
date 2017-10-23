\name{tag.h}
\alias{tag.h}
\title{ Tag a Hash }
\description{
This function is used to tag the enternals of a rash with the hexadecimal manifestation of the hash.
}
\usage{
tag.h(hash)
}
\arguments{
  \item{hash}{An \R environment but for purposes here a hash.}
}
\value{
  This function is used for its side effects. The \code{attributes} of the hash are set with a key of \code{hexid} set equation to the hexadecimal manifestation of the hash.
}
\author{W.H. Asquith, Spring 2009}
\examples{
my.new.hash <- new.h()
set.h("author1", "william",  my.new.hash)
tag.h(my.new.hash)
attributes(my.new.hash)
}
\keyword{misc}
