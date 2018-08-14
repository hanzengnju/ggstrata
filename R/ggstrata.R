#' ggstrata
#'
#' ggstrata() reads two data frames and plots a stratigraphic column by using ggplot().
#'
#' @param df1 The first data frame df1 is the description of the section, with horizons of beds and their corresponding lithology being its first and second columns.
#' @param bed Specific the bed column in the first data frame (1 by default).
#' @param lithology Specific the lithology column in the first data frame (2 by default).
#' @param df2 The second data frame contains the configuration (width and color) for each lithology shown in the first data frame.
#' @param lith Specific the lithology column in the second data frame (1 by default). The values should correspond exactly to those in the first data frame.
#' @param width Specific the width for each lithology in the second data frame (2 by default).
#' @param color Specific the color for each lithology in the second data frame (3 by default).
#' @param xlab Specific the lab for the x axis ("Lithology" by default).
#' @param ylab Specific the lab for the y axis ("" by default).
#'
#' @details See above.
#'
#' @return Return a ggplot object.
#'
#' @note More options is under development.
#'
#' @author Han Zeng \email{hzeng@@nigpas.ac.cn}
#'
#' @references H. Wickham. 2016. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York.
#'
#' @seealso \link[ggplot2]{ggplot}
#'
#' @examples
#' ## Random example
#' bed <- runif(101, -50, 150)
#' lith <- c("Conglomerate", "Sandstone", "Siltstone", "Claystone")
#' width <- c(2.5, 2, 1.5, 1)
#' color <- palette()[2:5]
#' lithology <- sample(lith, 101, replace = TRUE)
#'
#' df1 <- data.frame(bed, lithology)
#' df2 <- data.frame(lith, width, color)
#'
#' ggstrata(df1 = df1, df2 = df2)
#'
#' @import ggplot2
#' @import dplyr
#' @export
ggstrata <- function(df1,
                     bed = 1,
                     lithology = 2,
                     df2,
                     lith = 1,
                     width = 2,
                     color = 3,
                     resolution = 10,
                     xlab = "Lithology",
                     ylab = "") {
  ## Force data types and arrange data.
  require(dplyr)
  df1 <- as.data.frame(df1) # Force to tibble object for dplyr.
  df1 <- df1 %>%
    arrange(df1[, 1]) # Arrange by thickness of beds.

  df2 <- as.data.frame(df2) # Force to tibble object for dplyr.
  df2 <- df2 %>%
    arrange(df2[, 2]) # Arrange by widths of beds.

  ## Check if data meet requirements.
  if (sum(is.na(df1[, 1:2])) > 0) {
    stop("Data frame 1 contains NAs.")
  }

  if (sum(is.na(df2[, 1:2])) > 0) {
    stop("Data frame 2 contains NAs.")
  }

  if (sum(!is.numeric(df1[, 1])) > 0) {
    stop("Non-numeric horizons.")
  }

  ## Get the bottom and top of the stratigraphic column
  bottom <- min(df1[, 1])
  top <-  max(df1[, 1])

  bot.major.tick <- ((ceiling(min(df1[, 1])) %/% 50) + 1) * 50
  top.major.tick <- (floor(max(df1[, 1])) %/% 50) * 50
  major.tick <- seq(bot.major.tick, top.major.tick, by = 50)

  bot.tick <- ((ceiling(min(df1[, 1])) %/% 10) + 1) * 10
  top.tick <- (floor(max(df1[, 1])) %/% 10) * 10
  minor.tick0 <- seq(bot.tick, top.tick, by = resolution)
  minor.tick <- minor.tick0[-match(major.tick, minor.tick0)]

  ## Establish plot framework
  require(ggplot2)
  p <- ggplot()

  ## Add beds to the stratigraphic colum
  n.horizon <- nrow(df1) - 1
  for (i in 1:n.horizon) {
    lower <- df1[i, 1]
    upper <- df1[i + 1, 1]
    selected <- which(df2[, 1] == df1[i, 2])
    width <- df2[selected, 2]
    col <- df2[selected, 3]
    p <-
      p + annotate(
        "rect",
        xmin = 0,
        xmax = width,
        ymin = lower,
        ymax = upper,
        fill = col,
        color = "black",
        size = 1
      )
  }

  p <- p +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = df2[, 2],
      labels = df2[, 1],
      position = "bottom"
    ) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = major.tick,
                       minor_breaks = minor.tick) +
    coord_cartesian(xlim = c(0, max(df2[, 2]) + 0.5), ylim = c(bottom, top)) +
    labs(x = xlab, y = ylab) +
    theme_classic() +
    theme(
      axis.text.x = element_text(colour = "black"),
      axis.text.y = element_text(colour = "black"),
      axis.ticks.length = unit(2, "mm")
    )

  return(p)
}
