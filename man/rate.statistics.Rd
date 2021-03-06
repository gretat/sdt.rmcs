% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rate.statistics.R
\name{rate.statistics}
\alias{rate.statistics}
\title{Complete Signal detection analysis for one group}
\usage{
rate.statistics(hits = hits, miss = miss, CorRej = CorRej,
  falarm = falarm, x = NULL, rm.intermid = TRUE)
}
\arguments{
\item{hits}{column containing total hits per person}

\item{miss}{column containing total misses per person}

\item{CorRej}{column containing total correct rejections per person}

\item{falarm}{column containing total dalse alarms per person}

\item{x}{Data frame - Must be in summary form (i.e. each row is a participant
with columns: total hits per person, total false alarms per person, total correct rejections and
total misses)}

\item{rm.intermid}{TRUE/FALSE (default = TRUE) removes
the intermediate columns created for the calculation of the Hit rate and False Alarm rate,
which are created for the recalculation of Hit rates and False alarm rates that are equal to 1 or 0}
}
\value{
\item{statistics}{A simple table containing the average d'prime and bias as well as their sd}
    \item{boxes}{Boxplots depicting the distribution of the d'prime and bias}
    \item{Density}{The density distributions for signal and noise}
    \item{ROC}{An ROC curve for both groups, including a dot for the position of the criterion}
    \item{AUC}{Area under the curve}
    \item{data}{Data frame with transformations and calculations}
}
\description{
Calculates rate statistics, signal detection statistics - d' and c, and auc. Produces plots for visualisation.
}
\details{
Calculates the hit rate, false alarm rate, Miss rate and Correct rejection rate,
    as well as the d'Prime and Bias (c). Recalculates Hit Rates and False Alarm Rates that are 1 or 0
    to allow d'Prime and Bias calculation. Prints the D'Prime, Bias (mean and SD), an ROC curve and a AUC
    calculation (AUC calculation depends on \code{\link[flux]{auc}}). The defaults of the functions are set
    to the dataset created by \code{\link{sdt.data}}. If function is run without any imput, it will use
    \code{\link{sdt.data}}'s DEFAULT inputs to generate a dataset. If you have generated a data set through
    \code{\link{sdt.data}} then the only requirement is to specify the dataset i.e. rate.statistics(x=DATA).
}
\examples{
    rate.statistics()

    data <- sdt.data(50,50,0.35,0.11)
    result <- rate.statistics(x=data,rm.intermid = TRUE)
    result$ROC
\dontrun{
    rate.statistics(hits = HIT, miss = Miss, CorRej = CR,
                falarm = FA, sdt = my.dataset, rm.intermid = FALSE)
}
}
