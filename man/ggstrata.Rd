\name{ggstrata}

\alias{ggstrata}

\title{
  ggstrata() plots a stratigraphic column by using ggplot().
}

\description{
  ggstrata() reads two data frames and plot a stratigraphic column.
}

\usage{
ggstrata(df1, df2)
ggstrata(df1 = df1, df2 = df2, resolution = 10)
}

\arguments{
  \item{df1}{
  The first data frame df1 is the description of the section, with horizons of beds and their corresponding lithology being its first and second columns.
}
  \item{bed}{
  Specific the bed column in the first data frame (1 by default).
}
  \item{lithology}{
  Specific the lithology column in the first data frame (2 by default).
}
  \item{df2}{
  The second data frame contains the configuration (width and color) for each lithology shown in the first data frame.
}
  \item{lith}{
  Specific the lithology column in the second data frame (1 by default). The values should correspond exactly to those in the first data frame.
}
  \item{width}{
  Specific the width for each lithology in the second data frame (2 by default).
}
  \item{color}{
  Specific the color for each lithology in the second data frame (3 by default).
}
  \item{xlab}{
  Specific the lab for the x axis ("Lithology" by default).
}
  \item{ylab}{
  Specific the lab for the y axis ("" by default).
}
}

\details{
  More options is under development.
}

\value{
  Return a ggplot object.
}

\references{
%% ~H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016. ~
}
\author{
%%  ~~Han Zeng~~
}
\note{
%%  ~~further notes~~
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Random example
bed <- runif(101, -50, 150)
lith <- c("Conglomerate", "Sandstone", "Siltstone", "Claystone")
width <- c(2.5, 2, 1.5, 1)
color <- palette()[2:5]
lithology <- sample(lith, 101, replace = T)

df1 <- data.frame(bed, lithology)
df2 <- data.frame(lith, width, color)

ggstrata(df1 = df1, df2 = df2)
}

\keyword{ ~stratigraphy }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~ggplot2 }% __ONLY ONE__ keyword per line
