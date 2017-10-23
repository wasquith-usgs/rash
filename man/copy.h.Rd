\name{copy.h}
\alias{copy.h}
\title{ Make a Copy of a Hash }
\description{
This function copys a hash by successive copying of the values on a key-by-key basis. No provision is made for situations involving values that are references themselves.
}
\usage{
copy.h(hash)
}
\arguments{
  \item{hash}{An \R environment but for purposes here a hash.}
}
\value{
  This function returns a hash.
}
\author{W.H. Asquith, June 2011}
\examples{
my.new.hash <- new.h()
set.h("author1", "william",  my.new.hash)
set.h("author2", "george",   my.new.hash)
set.h("author3", "theodore", my.new.hash)
vals.h(my.new.hash)
}
\keyword{misc}
