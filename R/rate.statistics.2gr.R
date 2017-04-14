#' Complete Signal detection analysis for 2 groups
#'
#' This function will calculate the hit rate, false alarm rate, Miss rate and Correct rejection rate,
#' as well as the d'Prime and Bias (c). Recalculates Hit Rates and False Alarm Rates that are 1 or 0
#' to allow d'Prime and Bias calculation. Prints the D'Prime, Bias (mean and SD), an ROC curve and a AUC
#' calculation (AUC calculation depends on \code{\link[flux]{auc}}).
#' For the function to work on its own as a demonstrationThe defaults of the functions are set to the dataset
#' created by \code{\link{sdt.data2}}. If function is run without any imput, it will use
#' \code{\link{sdt.data2}}'s DEFAULT inputs to generate a dataset. If you have generated a data set through
#' \code{\link{sdt.data2}} then the only requirement is to specify the dataset i.e. rate.statistics2(x=DATA).
#'
#' @param hits column containing total hits per person, group 1
#' @param miss column containing total misses per person, group 1
#' @param CorRej column containing total correct rejections per person, group 1
#' @param falarm column containing total dalse alarms per person, group 1
#' @param hits2 column containing total hits per person, group 2
#' @param miss2 column containing total misses per person, group 2
#' @param CorRej2 column containing total correct rejections per person, group 2
#' @param falarm2 containing total dalse alarms per person, group 2
#'
#' @param rm.intermid  TRUE/FALSE (default = TRUE) removes the intermediate
#' columns created for the calculation of the Hit rate and False Alarm rate,
#' which are created for the recalculation of Hit rates and False alarm rates
#' that are equal to 1 or 0
#'
#' @param x  Data frame Must be in summary form (i.e. each row is a participant
#' with columns: total hits per person, total false alarms per person, total correct
#' rejections and total misses)
#'
#'
#' @return a list consisting of
#' \item{statistics}{A simple table containing the average d'prime and bias as well as their sd}
#' \item{boxes}{Boxplots depicting the distribution of the d'prime and bias for each group}
#' \item{Density}{The density distributions for signal and noise for both groups}
#' \item{ROC}{An ROC curve for both groups, including a dot for the position of the criterion}
#' \item{AUC}{Area under the curve for Group 1}
#' \item{AUC2}{Area under the curve for Group 2}
#' \item{Group1}{Data frame with transformations and calculations for Group 1}
#' \item{Group2}{Data frame with transformations and calculations for Group 2}
#'
#' @example
#'
#' rate.statistics2(hits = HIT, miss = Miss, CorRej = CR,
#'                  falarm = FA, hits2 = HIT2, miss2 = Miss2, CorRej2 = CR2,
#'                  falarm2 = FA2, x = sdt2, rm.intermid = TRUE)
#'
#'@importFrom tidyverse gather
#'
#'
rate.statistics2 <- function(hits = hits, miss = miss, CorRej = CorRej, falarm = falarm, hits2 = hits2, miss2 = miss2,
                             CorRej2 = CorRej2, falarm2 = falarm2, x = NULL, rm.intermid = TRUE){

  #check if data is given
  if (is.null(x) == T){  # if not create dataset
    x <- sdt.data2()
  } else {
    x = x
  }

  dat <- data.frame(tot.hits = x$hits, tot.miss = x$miss, tot.fa = x$falarm, tot.corRej = x$corRej)
  dat2 <- data.frame(tot.hits2 = x$hits2, tot.miss2 = x$miss2, tot.fa2 = x$falarm2, tot.corRej2 = x$corRej2)

  dat <- dplyr::mutate(dat, hit.rate1 = (tot.hits / (tot.hits + tot.miss)))
  dat <- dplyr::mutate(dat, fa.rate1 = (tot.fa / (tot.fa + tot.corRej)))
  dat <- dplyr::mutate(dat, miss.rate = (tot.miss / (tot.hits + tot.miss)))
  dat <- dplyr::mutate(dat, corRej.rate = (tot.corRej / (tot.fa + tot.corRej)))
  dat2 <- dplyr::mutate(dat2, hit.rate3 = (tot.hits2 / (tot.hits2 + tot.miss2)))
  dat2 <- dplyr::mutate(dat2, fa.rate3 = (tot.fa2 / (tot.fa2 + tot.corRej2)))
  dat2 <- dplyr::mutate(dat2, miss.rate2 = (tot.miss2 / (tot.hits2 + tot.miss2)))
  dat2 <- dplyr::mutate(dat2, corRej.rate2 = (tot.corRej2 / (tot.fa2 + tot.corRej2)))

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
  dat2$hit.rate2 <- 0

  for (i in 1:length(dat2$hit.rate3)) {
    if (dat2$hit.rate3[i] == 1) {
      dat2$hit.rate2[i] <- (1 - 1 / (2 * (dat2$tot.hits2[i] + dat2$tot.miss2[i])))
    } else if (dat2$hit.rate3[i] == 0) {
      dat2$hit.rate2[i] <- (0 + 1 / (2 * (dat2$tot.hits2[i] + dat2$tot.miss2[i])))
    } else {
      dat2$hit.rate2[i] = dat2$hit.rate3[i]
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

  dat2$fa.rate2 <- 0

  for (i in 1:length(dat2$fa.rate3)) {
    if (dat2$fa.rate3[i] == 1) {
      dat2$fa.rate2[i] <- (1 - 1 / (2 * (dat2$tot.fa2[i] + dat2$tot.corRej2[i])))
    } else if (dat2$fa.rate3[i] == 0) {
      dat2$fa.rate2[i] <- (0 + 1 / (2 * (dat2$tot.fa2[i] + dat2$tot.corRej2[i])))
    } else {
      dat2$fa.rate2[i] = dat2$fa.rate3[i]
    }
  }

  #remove or retain intermediate calculation columns
  if (rm.intermid == TRUE){
    dat$hit.rate1 <- NULL
    dat$fa.rate1 <- NULL
    dat2$hit.rate3 <- NULL
    dat2$fa.rate3 <- NULL
    print('The intermediate columns before checking for 100% or 0% accuracy or false alarms have been removed')
  } else {
    print('The intermediate columns before checking for 100% or 0% accuracy or false alarms have been retained and are called group1: *hit.rate1* and *fa.rate1*; group 2: *hit.rate3* and *fa.rate3*')
  }


  print(paste("This experiment has a total number of targets:",
              (dat$tot.hits[1]+dat$tot.miss[1]),
              "and a total number of lures:",
              (dat$tot.fa[1]+dat$tot.corRej[1])))

  # Transformation to z-scores for d-prime and bias calculation
  dat <- dplyr::mutate(dat, zHR = stats::qnorm(hit.rate))
  dat <-dplyr::mutate(dat, zFA = stats::qnorm(fa.rate))
  dat <- dplyr::mutate(dat,dPrime = (zHR - zFA))
  dat <- dplyr::mutate(dat,Bias = (-0.5 * (zHR + zFA)))
  dat2 <- dplyr::mutate(dat2, zHR2 = stats::qnorm(hit.rate2))
  dat2 <- dplyr::mutate(dat2, zFA2 = stats::qnorm(fa.rate2))
  dat2 <- dplyr::mutate(dat2,dPrime2 = (zHR2 - zFA2))
  dat2 <- dplyr::mutate(dat2,Bias2 = (-0.5 * (zHR2 + zFA2)))

  avg.dprime <- mean(dat$dPrime)
  sd.dprime <- stats::sd(dat$dPrime)
  avg.bias <- mean(dat$Bias)
  sd.bias <- stats::sd(dat$Bias)
  avg.dprime2 <- mean(dat2$dPrime2)
  sd.dprime2 <- stats::sd(dat2$dPrime2)
  avg.bias2 <- mean(dat2$Bias2)
  sd.bias2 <- stats::sd(dat2$Bias2)
  sdt.stat <- data.frame(avg.dprime, sd.dprime, avg.bias, sd.bias, avg.dprime2, sd.dprime2, avg.bias2, sd.bias2)

  print(paste("The average d Prime (d') score for group 1 is: ",
              round(avg.dprime, digits = 2), "(SD=",
              round(sd.dprime, digits = 2),") and the Criterion (c) is at: ",
              round(avg.bias, digits = 2), " (SD=",
              round(sd.bias, digits = 2), ").
              The average d Prime (d') score for group 2 is: ",
              round(avg.dprime2, digits = 2), "(SD=",
              round(sd.dprime2, digits = 2),") and the Criterion (c) is at: ",
              round(avg.bias2, digits = 2), " (SD=",
              round(sd.bias2, digits = 2), ")."))

  #' Visualise the distributions of d'Prime and Bias
  #'

  boxes <- data.frame(gather(dat, 'Statistic', 'value', c(dPrime, Bias)))
  boxes2 <- data.frame(gather(dat2, 'Statistic2', 'value2', c(dPrime2, Bias2)))

  old.par <- graphics::par(mfrow=c(1, 2))

  graphics::boxplot(boxes$value ~ boxes$Statistic, col = c('salmon', 'turquoise3'),
                    main = 'Distributions for group 1', outcol="slateblue3")
  graphics::boxplot(boxes2$value2 ~ boxes2$Statistic2,  names = c('Bias', 'dPrime'), col = c('salmon', 'turquoise3'),
                    main = 'Distributions for group 2', outcol = "slateblue3")
  graphics::par(old.par)

  box.plot <- grDevices::recordPlot()

  graphics::par()
  graphics::plot.new()


  rm(boxes, boxes2)

  sequence <- seq(-5, 10, length = 1000)

  #' Density Curve

  # get normal probability density functions group 1
  dFAR <- stats::dnorm(sequence,mean=0,sd=1)
  dHR <- stats::dnorm(sequence,mean=avg.dprime,sd=sd.dprime) # sd=1 for equal variance SD
  # get normal probability density functions group 2
  dFAR2 <- stats::dnorm(sequence,mean=0,sd=1)
  dHR2 <- stats::dnorm(sequence,mean=avg.dprime2,sd=sd.dprime2) # sd=1 for equal variance SD

  # draw the density function + line for criterion
  old.par2 <- graphics::par(mfcol=c(2, 1))

  graphics::plot(sequence, dFAR, type = "l", col = 'turquoise3', xlab = "", ylab = "",
                 ylim = c(0, .5), lwd = 2, main = 'Group 1') # FAR distribution
  graphics::par(new = T)
  graphics::plot(sequence, dHR, type = "l", col = 'salmon', axes = F, xlab = "x",
                 ylab = "Normal probability density function", ylim = c(0, .5), lwd = 2, main = 'Group 1') # HR distribution
  graphics::abline(v = avg.bias, lty = 3, lwd = 3) # dotted line for criterion
  graphics::legend("topright", legend = c("Noise", "Signal+Noise"), fill = c('turquoise3', 'salmon', 'black'), lty = c(1,1,3))
  graphics::par(new = F)

  graphics::plot(sequence, dFAR2, type = "l", col = 'turquoise3', xlab = "", ylab = "",
                 ylim = c(0, .5), lwd = 2, main = 'Group 2') # FAR2 distribution
  graphics::par(new = T)
  graphics::plot(sequence, dHR2, type = "l", col = 'salmon', axes = F, xlab = "x",
                 ylab = "Normal probability density function 2", ylim = c(0, .5), lwd = 2, main = 'Group 2') # HR2 distribution
  graphics::abline(v = avg.bias2, lty = 3, lwd = 3) # dotted line for criterion
  graphics::legend("topright", legend = c("Noise", "Signal+Noise"), fill = c('turquoise3', 'salmon', 'black'), lty = c(1,1,3))
  graphics::par(new = F)
  graphics::par(old.par2)

  dens <- grDevices::recordPlot()
  graphics::par()
  graphics::plot.new()

  #' ROC curve

  # get response probabilities for each distribution group 1
  pFAR <- 1 - stats::pnorm(sequence, mean = 0, sd = 1)
  pHR <- 1 - stats::pnorm(sequence, mean = avg.dprime, sd = sd.dprime)

  # get response probabilities for each distribution group 2
  pFAR2 <- 1 - stats::pnorm(sequence, mean = 0, sd = 1)
  pHR2 <- 1 - stats::pnorm(sequence, mean = avg.dprime2, sd = sd.dprime2)

  # get response probabilities at criterion -
  pFAR.crit <- 1 - stats::pnorm(avg.bias, mean =0, sd = 1)
  pHR.crit <- 1 - stats::pnorm(avg.bias, mean = avg.dprime, sd = sd.dprime)

  # get response probabilities at criterion -
  pFAR.crit2 <- 1 - stats::pnorm(avg.bias2, mean =0, sd = 1)
  pHR.crit2 <- 1 - stats::pnorm(avg.bias2, mean = avg.dprime2, sd = sd.dprime2)

  # draw the ROC curve & dot for criterion & the chance line
  graphics::plot(pFAR, pHR, type = "l", col = 'dodgerblue4', xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")
  graphics::par(new = T)
  graphics::plot(pFAR2, pHR2, type = "l", col = 'limegreen', xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")
  graphics::par(new = T)
  graphics::plot(pFAR.crit, pHR.crit, col = 'deeppink', pch = 19, xlim = c(0, 1), ylim = c(0, 1),
                 axes = F, xlab = "FA Rate", ylab = "Hit Rate")
  graphics::par(new = T)
  graphics::plot(pFAR.crit2, pHR.crit2, col = 'navy', pch = 19, xlim = c(0, 1), ylim = c(0, 1),
                 axes = F, xlab = "", ylab = "")
  graphics::par(new = T)
  graphics::abline(a = 0, b = 1,lty = 3)
  graphics::par(new=F)

  roc.curve <- grDevices::recordPlot()
  graphics::par()

  #' AUC calculation, imports flux::auc

  AUC <- round(flux::auc(x = pFAR, y = pHR, thresh = 0.001), digits = 2)
  AUC2 <- round(flux::auc(x = pFAR2, y = pHR2, thresh = 0.001), digits = 2)

  print(paste('The Area Under the Curve for a group 1 with a mean d\'Prime of ',
              round(avg.dprime, digits = 2), ' is: ',
              AUC,
              'The Area Under the Curve for group 2 with a mean d\'Prime of ',
              round(avg.dprime2, digits = 2), ' is: ',
              AUC2))

  params <- list('statistics' = sdt.stat, 'boxes' = box.plot, 'Density' = dens, 'ROC' = roc.curve,
                 'AUC' = AUC, 'AUC2' = AUC2, 'Group1' = dat, 'Group2' = dat2)

  return(params)

}
