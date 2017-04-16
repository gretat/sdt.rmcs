#' Create data for 2 samples/groups
#'
#' This creates a dataset with 1000 participants to be used with the main function.
#' \code{\link{rate.statistics2}}. It can be run with its defaults sdt.rmcs() or by specifying parameters.
#'
#'
#' @param tot.signal A number - The total number of signals i.e. blue dots that have to be detected Default = 85
#' @param tot.lure A number - The total number of lures i.e. number green dots that should not be responded to, Default = 100
#' @param min.hits A number - Minimum ammount of hits for group 1. Dafalt to 0.7
#' @param min.cr A number - Minimum ammount of correct rejections for group 1.  Dafalt to 0.8
#' @param min.hits2 A number - Minimum ammount of hits for group 2. Dafalt to 0.3
#' @param min.cr2 A number - Minimum ammount of correct rejections for group 2.  Dafalt to 0.3
#'
#'@return
#'A data table with the following columns
#'\itme{hits}{total number of hits per participant randomly chosen between tot.signal and the minimum proportion hits specified}
#'\itme{corRej}{total number of correct rejection per participant randomly chosen between tot.lure and the minimum proportion critical rejections specified}
#'\itme{miss}{total number of misses per participant - left from the total signals after removing hits}
#'\itme{falarm}{total number of false alarms per participant - left from the total lures after removing correct rejections}
#'\itme{hits2}{total number of hits per participant for group 2 randomly chosen between tot.signal and the minimum proportion hits specified}
#'\itme{corRej2}{total number of correct rejection per participant for group 2 randomly chosen between tot.lure and the minimum proportion critical rejections specified}
#'\itme{miss2}{total number of misses per participant for group 2- left from the total signals after removing hits}
#'\itme{falarm2}{total number of false alarms per participant for group 2 - left from the total lures after removing correct rejections}
#'@examples
#' sdt.data2(tot.signal = 85, tot.lure = 100,
#'           min.hits = 0.7, min.cr = 0.8,
#'           min.hits2 = 0.3, min.cr2 = 0.3)

sdt.data2 <- function(tot.signal = 85, tot.lure = 100, min.hits = 0.7, min.cr = 0.8,
                      min.hits2 = 0.3, min.cr2 = 0.3){

  mh <- round((tot.signal*min.hits), digits = 0)
  mcr <- round((tot.lure*min.cr), digits = 0)
  mh2 <- round((tot.signal*min.hits2), digits = 0)
  mcr2 <- round((tot.lure*min.cr2), digits = 0)
  sdt <- data.frame(ID = rep(1:1000, each=1), hits = sample(c(mh:tot.signal), size = 1000, replace = T),
                    corRej = sample(c(mcr:tot.lure), size = 1000, replace = T),
                    hits2 = sample(c(mh2:tot.signal), size = 1000, replace = T),
                    corRej2 = sample(c(mcr2:tot.lure), size = 1000, replace = T))
  sdt$miss   <- (tot.signal - sdt$hits)
  sdt$falarm <- (tot.lure - sdt$corRej)
  sdt$miss2   <- (tot.signal - sdt$hits2)
  sdt$falarm2 <- (tot.lure - sdt$corRej2)

  return(sdt)
}
