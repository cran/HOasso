\name{HOasso}
\alias{HOasso}
\alias{plot.assortativity}
\alias{print.assortativity}
\title{Evalutes Higer Order Assortativity of complex networks}
\usage{
HOasso(
    g,
    h = 1,
    weighted = is.weighted(g),
    x = c("sout", "dout", "lout", "sin", "din", "lin"),
    y = c("sin", "din", "lin", "sout", "dout", "lout")
)

  \method{plot}{assortativity}(x,
    type = "h",
    ylim = c(-1, 1),
    xlab = "Orders",
    ylab = "Assortativity",
    ...
  )

  \method{print}{assortativity}(x, ...)
}

\arguments{
\item{g}{an object of class \code{igraph} with two columns, listing the dominances, by rows.}
\item{h}{an integer value, the function will evaluates the assortativity from the order 1 to the order \code{h}.}
\item{weighted}{\code{logical}, if to use the weighted matrix to create the trasnition probabilities.}
\item{x}{In case of the \code{HOasso} function the first centrarlity measure, out-strength by default, see \code{details}. An object of class \code{assortativity} in case of the \code{print} and \code{plot} functions}
\item{y}{The second centrarlity measure, in-strength by default, see \code{details}.}
\item{type}{Type of plot, histogram-like vertical lines by default.}
\item{xlab}{A label for the x axis, \code{Orders} by default.}
\item{ylab}{A label for the x axis, \code{Assortativity} by default.}
\item{ylim}{The y limits of the plot, the assortativity index can assume only values between -1 and 1.}
\item{...}{Other arguments of the \code{plot.default} or the \code{print.default}  functions.}
}
\value{A vector \code{h} long containing the assortativity measures from the order 1 to the order \code{h}.}
\description{
The function evalutes Higer Order Assortativity of complex networks represented by objects of class \code{igraph} from the package of the same name.
}
\details{
Arguments \code{x} and {y} are \code{character} objects and can assume values   \code{"sout"}, \code{"dout"}, \code{"lout"}, \code{"sin"}, \code{"din"}, \code{"lin"} representing the out-strength, out-degree, out-log-strength, in-strength, in-degree, and in-log-strength respectively.

In case of undirected graphs in- and out- centrality measures are equal. In case of unweighted graphs the strength is equal to the degree.

The function returns an object of class \code{assortativity} subclass of a \code{numeric} vector.

\code{plot.assortativity} is identical to \code{plot.default} but with different defaults in order to get a plot coherent with the assortativity index.

\code{print.assortativity} is a method to show the assortativity values and the order side by syde.
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
  a <- HOasso(g, h = 10)
  print(a)
  plot(a, lwd = 3, panel.first = abline(h = 0, lty = 2))
}
