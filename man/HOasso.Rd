\name{HOasso}
\alias{HOasso}
\title{Evalutes Higer Order Assortativity of complex networks}
\usage{
HOasso(
    g,
    h = 1,
    weighted = is.weighted(g),
    x = c("sout", "dout", "lout", "sin", "din", "lin"),
    y = c("sin", "din", "lin", "sout", "dout", "lout")
)
}
\arguments{
\item{g}{an object of class \code{igraph} with two columns, listing the dominances, by rows.}
\item{h}{an integer value, the function will evaluates the assortativity from the order 1 to the order \code{h}.}
\item{weighted}{\code{logical}, if to use the weighted matrix to create the trasnition probabilities.}
\item{x}{The first centrarlity measure, out-strength by default, see \code{details}.}
\item{y}{The second centrarlity measure, in-strength by default, see \code{details}.}
}
\value{A vector \code{h} long containing the assortativity measures from the order 1 to the order \code{h}.}
\description{
The function evalutes Higer Order Assortativity of complex networks represented by objects of class \code{igraph} from the package of the same name.
}
\details{
Arguments \code{x} and {y} are \code{character} objects and can assume values   \code{"sout"}, \code{"dout"}, \code{"lout"}, \code{"sin"}, \code{"din"}, \code{"lin"} representing the out-strength, out-degree, out-log-strength, in-strength, in-degree, and in-log-strength respectively.

In case of undirected graphs in- and out- centrality measures are equal. In case of unweighted graphs the strength is equal to the degree.
}
\references{
\insertRef{arcagni2017higher}{HOasso}

\insertRef{arcagni2021extending}{HOasso}

\insertRef{arcagni2023higher}{HOasso}
}
\examples{
  g <- graph_from_data_frame(data.frame(
    from   = c("i", "j", "j", "k", "l"),
    to     = c("k", "k", "l", "l", "i"),
    weight = c( 10,  5,   2,   3,   2 )
  ))
  E(g)$label <- E(g)$weight
  plot(g, layout = layout_in_circle(g))
  a <- HOasso(g, h = 10)
  a
  plot(a, type = "b", ylim = c(-1, 1), panel.first = abline(h = 0, lty = 2))
}
