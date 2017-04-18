## ---- message=FALSE------------------------------------------------------
devtools::install_github('gretat/sdt.rmcs')
library(sdt.rmcs)


## ------------------------------------------------------------------------

mydata <- sdt.data(tot.signal = 30, tot.lure = 50, min.hits = 0.5, min.cr = 0.8)
head(mydata, 5)

## ---- fig.show='hide', fig.width=9, fig.height=5-------------------------
my.analysis <- rate.statistics(hits = hits, miss = miss, CorRej = CorRej, falarm = falarm, x = mydata, rm.intermid = FALSE)

## ------------------------------------------------------------------------
my.analysis$statistics

## ---- fig.show='asis', fig.width=6, fig.height=5-------------------------
my.analysis$boxes

## ---- fig.show='asis', fig.width=9, fig.height=5-------------------------
my.analysis$Density

## ---- fig.show='asis', fig.width=7, fig.height=5-------------------------
my.analysis$ROC

## ------------------------------------------------------------------------
my.analysis$AUC

## ------------------------------------------------------------------------
head(my.analysis$data, 5)

## ------------------------------------------------------------------------

mydata2 <- sdt.data2(tot.signal = 80, tot.lure = 80,
           min.hits = 0.8, min.cr = 0.85,
           min.hits2 = 0.2, min.cr2 = 0.4)
head(mydata2, 5)

## ---- fig.show='hide', fig.width=9, fig.height=5-------------------------

my.analysis2 <- rate.statistics2(hits = hits, miss = miss, CorRej = CorRej, falarm = falarm, 
                                 hits2 = hits2, miss2 = miss2,
                                 CorRej2 = CorRej2, falarm2 = falarm2, x = mydata2, rm.intermid = FALSE)

## ------------------------------------------------------------------------
my.analysis2$statistics

## ---- fig.show='asis', fig.width=6, fig.height=5-------------------------
my.analysis2$boxes1
my.analysis2$boxes2

## ---- fig.show='asis', fig.width=9, fig.height=5-------------------------

my.analysis2$Density1
my.analysis2$Density2

## ---- fig.show='asis', fig.width=9, fig.height=5-------------------------
my.analysis2$ROC

## ------------------------------------------------------------------------
my.analysis2$AUC
my.analysis2$AUC2

## ------------------------------------------------------------------------
head(my.analysis2$Group1, 5)

## ------------------------------------------------------------------------
head(my.analysis2$Group1, 5)

