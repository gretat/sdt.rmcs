#' Create data for one sample
#'
#' Creates a dataset with 1000 participants to be used with the main function \code{\link{rate.statistics}}.
#' The parameters can be changed or it can be ran with the defaults by calling sdt.data()
#'
#' @param tot.signal A number - The total number of signals i.e. blue dots that have to be detected Default = 85
#' @param tot.lure A number - The total number of lures i.e. number green dots that should not be responded to, Default = 100
#' @param min.hits A number - Minimum proportion of hits. Dafalt to 0.5
#' @param min.cr A number - Minimum proportion of correct rejections.  Dafalt to 0.6
#'
#'@return
#'A data table with the following columns
#'\itme{hits}{total number of hits per participant randomly chosen between tot.signal and the minimum proportion hits specified}
#'\itme{corRej}{total number of correct rejection per participant randomly chosen between tot.lure and the minimum proportion critical rejections specified}
#'\itme{miss}{total number of misses per participant - left from the total signals after removing hits}
#'\itme{falarm}{total number of false alarms per participant - left from the total lures after removing correct rejections}
#'@examples
#'sdt.data(tot.signal = 85, tot.lure = 100, min.hits = 0.5, min.cr = 0.6)
#'

sdt.data <- function(tot.signal = 85, tot.lure = 100, min.hits = 0.5, min.cr = 0.6){

  mh <- round((tot.signal*min.hits), digits = 0)
  mcr <- round((tot.lure*min.cr), digits = 0)
  sdt <- data.frame(hits = sample(c(mh:tot.signal), size = 1000, replace = T),
                    corRej = sample(c(mcr:tot.lure), size = 1000, replace = T))
  sdt$miss   <- (tot.signal - sdt$hits)
  sdt$falarm <- (tot.lure - sdt$corRej)

  return(sdt)
}
