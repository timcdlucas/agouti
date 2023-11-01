#' A grouped histogram multi plot.
#'
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 aes
#' @param y A character vector of the variable names to be plotted (first variable listed is how the data gets ordered).
#' @param ID A character or factor vector to be used as a grouping variable.
#' @param data A dataframe containing the variables to be plotted
#' @param weights A numeric vector giving the weights.
#' @param breaks How many breaks to use.
#' @export
#' @examples
#' data(madagascar_malaria)
#' heat_multi_hist(c("Elevation","EVI","LSTmean"), madagascar_malaria$ID, data=madagascar_malaria)

heat_multi_hist <- function(y, ID, data,weights = 1, breaks = 50){

  x <- data.frame(cbind(data[,names(data) %in% y]))
  z <- ncol(x)
  ID <- factor(ID)
  ID <- as.numeric(forcats::fct_reorder(ID, x[,1]))

  d <- data.frame(x, ID = ID, weights = weights)

  if(z==1){
    bins <- diff(range(d$x[!(is.na(d$x))]))
    p1 <-
      ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$ID, weights = .data$weights)) +
      ggplot2::geom_bin_2d(
        #binwidth = c(diff(range(x)) / breaks, 1)
        binwidth = c(bins / breaks, 1)
      ) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank()
      )

    return(p1)
  }
  else if(z!=1){
    dd <- tidyr::pivot_longer(data=d,cols=tidyselect::all_of(y))
    bins <- diff(range(d$x[!(is.na(d$x))]))
    p1 <-
      ggplot2::ggplot(dd, ggplot2::aes(x = value, y = ID, weights = weights)) +
      ggplot2::geom_bin_2d(
        #binwidth = c(diff(range(x)) / breaks, 1)
        binwidth = c(bins / breaks, 1)
      ) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::facet_wrap(~name, scales="free") +
      ggplot2::theme_bw()+
      ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank()
      )

    return(p1)
  }
}
