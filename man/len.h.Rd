\name{len.h}
\alias{len.h}
\title{ Length of a Hash }
\description{
This function determines the length of a hash.
}
\usage{
len.h(hash)
}
\arguments{
  \item{hash}{An \R environment but for purposes here a hash.}
}
\value{
  A scalar value of the length of a hash is returned.
}
\author{W.H. Asquith, Spring 2009}
\examples{
my.new.hash <- new.h()
set.h("author",  "william", my.new.hash)
set.h("keyword", "hail", my.new.hash)
n <- len.h(my.new.hash)
print(n)
}
\keyword{misc}
