\name{test.summarize.auto}
\alias{test.summarize.auto}
\title{test for mean comparison}
\usage{
  test.summarize.auto(x, g)
}
\arguments{
  \item{x}{vector}

  \item{g}{another vector}
}
\value{
  a list with two componments: p.value and method
}
\description{
  Compute a oneway.test (with equal or unequal variance) or
  a kruskal.test as appropriate.
}
\author{
  David Hajage
}

