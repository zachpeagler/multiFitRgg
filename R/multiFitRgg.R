#' Multiple PDF Plot For Continuous Variables
#' Using ggplot2
#'
#' This function returns a ggplot output showing the PDFs for selected distributions
#' against a continuous, non-negative input variable. Possible distributions include "normal",
#' "lognormal", "gamma", "exponential", "cauchy", "t", "weibull", "logistic",
#' and "all".
#'
#' @param x The variable to for which to plot PDFs
#' @param seq_length The number of points over which to fit x
#' @param distributions The distributions to fit x against
#' @returns A dataframe with x, the real density, and the pdf of the desired
#'  distributions with length(nrow) equal to seq_length +1.
#' @export
multiPDF_plot <- function (x, seq_length, distributions) {
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential",
                       "cauchy",
                       "t",
                       "weibull",
                       "logistic")
  }
  # calculate PDFs
  data <- multiPDF_cont(x, seq_length, distributions)
  # create plot with real density
  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(aes(x=x_seq, y=dens, color="Real Density"))+
    ggplot2::xlab("x")+
    ggplot2::ylab("PDF")+
    ggplot2::labs(title=paste("PDF plot for x over selected distributions"))+
    ggplot2::guides(color=guide_legend(title="Distribution"))+
    ggplot2::theme_bw()
  # check for each type of distribution in the distributions, and add it if present
  if ("normal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=pdf_normal, color='Normal'))
  }
  if ("lognormal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=pdf_lognormal, color='Lognormal'))
  }
  if ("gamma" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=pdf_gamma, color='Gamma'))
  }
  if ("exponential" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=pdf_exponential, color='Exponential'))
  }
  if ("cauchy" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=pdf_cauchy, color='Cauchy'))
  }
  if ("t" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=pdf_t, color='t'))
  }
  if ("weibull" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=pdf_weibull, color='Weibull'))
  }
  if ("logistic" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=pdf_logistic, color='Loglogistic'))
  }
  return(p)
}

#' Multiple CDF Plot For Continuous Variables
#' Using ggplot2
#'
#' This function returns a ggplot output showing the CDFs for selected distributions
#' against a continuous, non-negative input variable. Possible distributions include "normal",
#' "lognormal", "gamma", "exponential", "cauchy", "t", "weibull", "logistic",
#' and "all".
#'
#' @param x The variable to for which to plot PDFs
#' @param seq_length The number of points over which to fit x
#' @param distributions The distributions to fit x against
#' @returns A dataframe with x, the real density, and the pdf of the desired
#'  distributions with length(nrow) equal to seq_length +1.
#' @export
multiCDF_plot <- function (x, seq_length, distributions) {
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential",
                       "cauchy",
                       "t",
                       "weibull",
                       "logistic")
  }
  # calculate PDFs
  data <- multiCDF_cont(x, seq_length, distributions)
  # create plot with real density
  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(aes(x=x_seq, y=dens, color="Real Distribution"))+
    ggplot2::xlab("x")+
    ggplot2::ylab("CDF")+
    ggplot2::labs(title=paste("CDF plot for x over selected distributions"))+
    ggplot2::guides(color=guide_legend(title="Distribution"))+
    ggplot2::theme_bw()
  # check for each type of distribution in the distributions, and add it if present
  if ("normal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=cdf_normal, color='Normal'))
  }
  if ("lognormal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=cdf_lognormal, color='Lognormal'))
  }
  if ("gamma" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=cdf_gamma, color='Gamma'))
  }
  if ("exponential" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=cdf_exponential, color='Exponential'))
  }
  if ("cauchy" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=cdf_cauchy, color='Cauchy'))
  }
  if ("t" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=cdf_t, color='t'))
  }
  if ("weibull" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=cdf_weibull, color='Weibull'))
  }
  if ("logistic" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=x_seq, y=cdf_logistic, color='Loglogistic'))
  }
  return(p)
}

