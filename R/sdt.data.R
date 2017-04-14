#' Create data for one sample
#'
#' This creates a dataset with 1000 participants to be used with the main function \code{\link{rate.statistics}}.
#'
#'
#' @param tot.signal A number - The total number of signals i.e. blue dots that have to be detected Default = 85
#' @param tot.lure A number - The total number of lures i.e. number green dots that should not be responded to, Default = 100
#' @param min.hits A number - Minimum ammount of hits. Dafalt to 0.5
#' @param min.cr A number - Minimum ammount of correct rejections.  Dafalt to 0.6
#'
#'@examples
#'\dontrun{
#'sdt.data(tot.signal = 85, tot.lure = 100, min.hits = 0.5, min.cr = 0.6)
#'}

sdt.data <- function(tot.signal = 85, tot.lure = 100, min.hits = 0.5, min.cr = 0.6){

  mh <- round((tot.signal*min.hits), digits = 0)
  mcr <- round((tot.lure*min.cr), digits = 0)
  sdt <- data.frame(ID = rep(1:1000, each=1), hits = sample(c(mh:tot.signal), size = 1000, replace = T),
                    corRej = sample(c(mcr:tot.lure), size = 1000, replace = T))
  sdt$miss   <- (tot.signal - sdt$hits)
  sdt$falarm <- (tot.lure - sdt$corRej)

  return(sdt)
}
