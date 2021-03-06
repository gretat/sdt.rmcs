% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rate.statistics2.R
\name{rate.statistics2}
\alias{rate.statistics2}
\title{Complete Signal detection analysis for 2 groups}
\usage{
rate.statistics2(hits = hits, miss = miss, CorRej = CorRej,
  falarm = falarm, hits2 = hits2, miss2 = miss2, CorRej2 = CorRej2,
  falarm2 = falarm2, x = NULL, rm.intermid = TRUE)
}
\arguments{
\item{hits}{column containing total hits per person, group 1}

\item{miss}{column containing total misses per person, group 1}

\item{CorRej}{column containing total correct rejections per person, group 1}

\item{falarm}{column containing total dalse alarms per person, group 1}

\item{hits2}{column containing total hits per person, group 2}

\item{miss2}{column containing total misses per person, group 2}

\item{CorRej2}{column containing total correct rejections per person, group 2}

\item{falarm2}{containing total dalse alarms per person, group 2}

\item{x}{Data frame - Must be in summary form (i.e. each row is a participant
with columns: total hits per person, total false alarms per person, total correct
rejections and total misses)}

\item{rm.intermid}{TRUE/FALSE (default = TRUE) removes the intermediate
columns created for the calculation of the Hit rate and False Alarm rate,
which are created for the recalculation of Hit rates and False alarm rates
that are equal to 1 or 0}
}
\value{
\item{statistics}{A simple table containing the average d'prime and bias as well as their sd}
    \item{boxes1}{Boxplots depicting the distribution of the d'prime and bias for group 1}
    \item{boxes2}{Boxplots depicting the distribution of the d'prime and bias for group 2}
    \item{Density1}{The density distributions for signal and noise for group 1}
    \item{Density1}{The density distributions for signal and noise for group 2}
    \item{ROC}{An ROC curve for both groups, including a dot for the position of the criterion}
    \item{AUC1}{Area under the curve for Group 1}
    \item{AUC2}{Area under the curve for Group 2}
    \item{Group1}{Data frame with transformations and calculations for Group 1}
    \item{Group2}{Data frame with transformations and calculations for Group 2}
}
\description{
Calculates rate statistics, signal detection statistics - d' and c, and auc. Produces plots for visualisation.
}
\details{
Calculates hit rate, false alarm rate, Miss rate and Correct rejection rate,
    as well as the d'Prime and Bias (c). Recalculates Hit Rates and False Alarm Rates that are 1 or 0
    to allow d'Prime and Bias calculation. Prints the D'Prime, Bias (mean and SD), an ROC curve and a AUC
    calculation (AUC calculation depends on \code{\link[flux]{auc}}).
    For the function to work on its own as a demonstrationThe defaults of the functions are set to the dataset
    created by \code{\link{sdt.data2}}. If function is run without any imput, it will use
    \code{\link{sdt.data2}}'s DEFAULT inputs to generate a dataset. If you have generated a data set through
    \code{\link{sdt.data2}} then the only requirement is to specify the dataset i.e. rate.statistics2(x=DATA).
}
\examples{
    rate.statistics2()

    data <- sdt.data2(60, 150, 0.3, 0.8, 0.5, 0.7)
    result <- rate.statistics2(x = data, rm.intermid = FALSE)
    result$Density2
\dontrun{
    rate.statistics2(hits = HIT, miss = Miss, CorRej = CR,
                 falarm = FA, hits2 = HIT2, miss2 = Miss2, CorRej2 = CR2,
                 falarm2 = FA2, x = sdt2, rm.intermid = TRUE)
                 }
}
