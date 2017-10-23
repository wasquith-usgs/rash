\name{vals.h}
\alias{vals.h}
\title{ Extract the Values of a Hash }
\description{
This function extracts the values of a hash as a simple returned vector. Such an operation is likely not appropriate if the hash itself contains values other than scalars.
}
\usage{
vals.h(hash)
}
\arguments{
  \item{hash}{An \R environment but for purposes here a hash.}
}
\value{
  This function returns a vector of the values of the hash. The vector is not coerced into particular formats such as character, numeric, integer, factor, or logical.
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
