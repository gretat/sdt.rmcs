#' Complete Signal detection analysis for one group
#'
#' This function will calculate the hit rate, false alarm rate, Miss rate and Correct rejection rate,
#' as well as the d'Prime and Bias (c). Recalculates Hit Rates and False Alarm Rates that are 1 or 0
#' to allow d'Prime and Bias calculation. Prints the D'Prime, Bias (mean and SD), an ROC curve and a AUC
#' calculation (AUC calculation depends on \code{\link[flux]{auc}}). The defaults of the functions are set
#' to the dataset created by \code{\link{sdt.data}}. If function is run without any imput, it will use
#' \code{\link{sdt.data}}'s DEFAULT inputs to generate a dataset. If you have generated a data set through
#' \code{\link{sdt.data}} then the only requirement is to specify the dataset i.e. rate.statistics(x=DATA).
#'
#'
#' @param hits column containing total hits per person
#' @param miss column containing total misses per person
#' @param CorRej column containing total correct rejections per person
#' @param falarm column containing total dalse alarms per person
#' @param rm.intermid  TRUE/FALSE (default = TRUE) removes
#' the intermediate columns created for the calculation of the Hit rate and False Alarm rate,
#' which are created for the recalculation of Hit rates and False alarm rates that are equal to 1 or 0
#'
#' @param x  Data frame must be in summary form (i.e. each row is a participant
#' with columns: total hits per person, total false alarms per person, total correct rejections and
#' total misses)
#'
#' @return a ist consisting of
#' \item{statistics}{A simple table containing the average d'prime and bias as well as their sd}
#' \item{boxes}{Boxplots depicting the distribution of the d'prime and bias}
#' \item{Density}{The density distributions for signal and noise}
#' \item{ROC}{An ROC curve for both groups, including a dot for the position of the criterion}
#' \item{AUC}{Area under the curve}
#' \item{data}{Data frame with transformations and calculations}
#'
#'
#' @example
#'
#' rate.statistics(hits = HIT, miss = Miss, CorRej = CR,
#'                 falarm = FA, sdt = data, rm.intermid = FALSE)
#'@importFrom tidyr gather

rate.statistics <- function(hits = hits, miss = miss, CorRej = CorRej, falarm = falarm, x = NULL, rm.intermid = TRUE){

  #check if data is given
  if (is.null(x) == T){  # if not create dataset
    x <- sdt.data()
  } else {
    x = x
  }

  dat <- data.frame(tot.hits = x$hits, tot.miss = x$miss, tot.fa = x$falarm, tot.corRej = x$corRej)

  dat <- dplyr::mutate(dat, hit.rate1 = (tot.hits / (tot.hits + tot.miss)))
  dat <- dplyr::mutate(dat, fa.rate1 = (tot.fa / (tot.fa + tot.corRej)))
  dat <- dplyr::mutate(dat, miss.rate = (tot.miss / (tot.hits + tot.miss)))
  dat <- dplyr::mutate(dat, corRej.rate = (tot.corRej / (tot.fa + tot.corRej)))

  #recalculate HIT rate  == 1 | == 0
  dat$hit.rate <- 0

  for (i in 1:length(dat$hit.rate1)) {
    if (dat$hit.rate1[i] == 1) {
      dat$hit.rate[i] <- (1 - 1 / (2 * (dat$tot.hits[i] + dat$tot.miss[i])))
    } else if (dat$hit.rate1[i] == 0) {
      dat$hit.rate[i] <- (0 + 1 / (2 * (dat$tot.hits[i] + dat$tot.miss[i])))
    } else {
      dat$hit.rate[i] = dat$hit.rate1[i]
    }
  }

  #recalculate False Alarm rate  == 1 | == 0

  dat$fa.rate <- 0

  for (i in 1:length(dat$fa.rate1)) {
    if (dat$fa.rate1[i] == 1) {
      dat$fa.rate[i] <- (1 - 1 / (2 * (dat$tot.fa[i] + dat$tot.corRej[i])))
    } else if (dat$fa.rate1[i] == 0) {
      dat$fa.rate[i] <- (0 + 1 / (2 * (dat$tot.fa[i] + dat$tot.corRej[i])))
    } else {
      dat$fa.rate[i] = dat$fa.rate1[i]
    }
  }

  #remove or retain intermediate columns
  if (rm.intermid == TRUE){
    dat$hit.rate1 <- NULL
    dat$fa.rate1 <- NULL
    print('The intermediate columns before checking for 100% or 0% accuracy or false alarms have been removed')
  } else {
    print('The intermediate columns before checking for 100% or 0% accuracy or false alarms have been retained and are called *hit.rate1* and *fa.rate1*')
  }


  print(paste("This experiment has a total number of targets:",
              (dat$tot.hits[1]+dat$tot.miss[1]),
              "and a total number of lures:",
              (dat$tot.fa[1]+dat$tot.corRej[1])))

  # Transformation to z-scores for d-prime and bias calculation
  dat<- dplyr::mutate(dat, zHR = stats::qnorm(hit.rate))
  dat<-dplyr::mutate(dat, zFA = stats::qnorm(fa.rate))
  dat<- dplyr::mutate(dat,dPrime = (zHR - zFA))
  dat <- dplyr::mutate(dat,Bias = (-0.5 * (zHR + zFA)))

  avg.dprime <- mean(dat$dPrime)
  sd.dprime <- stats::sd(dat$dPrime)
  avg.bias <- mean(dat$Bias)
  sd.bias <- stats::sd(dat$Bias)
  sdt.stat <- data.frame(avg.dprime, sd.dprime, avg.bias, sd.bias)

  print(paste("The average d Prime (d') score for this example is: ",
              round(avg.dprime, digits = 2), "(SD=",
              round(sd.dprime, digits = 2),") and the Criterion (c) is at: ",
              round(avg.bias, digits = 2), " (SD=",
              round(sd.bias, digits = 2), ")."))

  #' Visualise the distributions of d'Prime and Bias
  #'

  boxes <- data.frame(gather(dat, 'Statistic', 'value', c(dPrime, Bias)))


  graphics::boxplot(boxes$value ~ boxes$Statistic, col = c('salmon', 'turquoise3'),
                    main = 'Distributions of d\' and Bias(c)', outcol = "slateblue3")

  box.plot <- grDevices::recordPlot()
  graphics::plot.new()

  rm(boxes)

  sequence <- seq(-5, 10, length = 1000)

  #' Density Curve

  # get normal probability density functions
  dFAR <- stats::dnorm(sequence,mean=0,sd=1)
  dHR <- stats::dnorm(sequence,mean=avg.dprime,sd=sd.dprime) # sd=1 for equal variance SD

  # draw the density function + line for criterion

  graphics::plot(sequence, dFAR, type = "l", col = 'turquoise3', xlab = "", ylab = "",
                 ylim = c(0, .5), lwd = 2, main = 'Density distribution for signal and noise') # FAR distribution
  graphics::par(new = T)
  graphics::plot(sequence, dHR, type = "l", col = 'salmon', axes = F, xlab = "x",
                 ylab = "Normal probability density function", ylim = c(0, .5), lwd = 2) # HR distribution
  graphics::abline(v = avg.bias, lty = 3, lwd = 3) # dotted line for criterion
  graphics::legend("topright", legend = c("Noise", "Signal+Noise", 'Criterion'), fill = c('turquoise3', 'salmon', 'black'), lty = c(1,1,3))
  graphics::par(new = F)

  dens <- grDevices::recordPlot()
  graphics::plot.new()


  #' ROC curve

  # get response probabilities for each distribution
  pFAR <- 1 - stats::pnorm(sequence, mean = 0, sd = 1)
  pHR <- 1 - stats::pnorm(sequence, mean = avg.dprime, sd = sd.dprime)

  # get response probabilities at criterion -
  pFAR.crit <- 1 - stats::pnorm(avg.bias, mean =0, sd = 1)
  pHR.crit <- 1 - stats::pnorm(avg.bias, mean = avg.dprime, sd = sd.dprime)

  # draw the ROC curve & dot for criterion & the chance line
  graphics::plot(pFAR, pHR, type = "l", col = 'turquoise', xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")
  graphics::par(new = T)
  graphics::plot(pFAR.crit, pHR.crit, col = 'salmon', pch = 19, xlim = c(0, 1), ylim = c(0, 1),
                 axes = F, xlab = "FA Rate", ylab = "Hit Rate")
  graphics::abline(a = 0, b = 1,lty = 3)
  graphics::par(new=F)

  roc.curve <- grDevices::recordPlot()


  #' AUC calculation, imports flux::auc

  AUC <- round(flux::auc(x = pFAR, y = pHR, thresh = 0.001), digits = 2)

  print(paste('The Area Under the Curve for a sample with a mean d\'Prime of ',
              round(avg.dprime, digits = 2), ' is: ',
              AUC))

  params <- list('statistics' = sdt.stat, 'boxes' = box.plot, 'Density' = dens, 'ROC' = roc.curve,'AUC' = AUC, 'data' = dat)

  return(params)

}

