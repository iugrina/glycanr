## ---- eval=FALSE---------------------------------------------------------
#  # install additional packages to help us work with R
#  install.packages('devtools')

## ---- eval=FALSE---------------------------------------------------------
#  # load devtools package
#  library(devtools)

## ---- eval=FALSE---------------------------------------------------------
#  # install 'glycanr' package from GitHub to help us plot glycans
#  install_github('iugrina/glycanr')

## ------------------------------------------------------------------------
# load glycanr package
library(glycanr)

## ------------------------------------------------------------------------
set.seed(123)
n <- 200
X <- data.frame(ID=1:n, GP1=runif(n), GP2=rexp(n, 0.3),
                GP3=rgamma(n, 2), cc=factor(sample(1:2, n, replace=TRUE)))

## ------------------------------------------------------------------------
head(X)

## ------------------------------------------------------------------------
glyco.plot(X)

## ------------------------------------------------------------------------
glyco.plot(X, violin=TRUE)

## ------------------------------------------------------------------------
glyco.plot(X, collapse=FALSE)

## ------------------------------------------------------------------------
glyco.plot(X, collapse=FALSE, log.transform=TRUE)

## ------------------------------------------------------------------------
glyco.plot(X, collapse=FALSE, log.transform=TRUE, group="cc")

## ------------------------------------------------------------------------
glyco.plot(X, collapse=FALSE, log.transform=TRUE, group="cc", p.adjust.method="fdr")

## ------------------------------------------------------------------------
glyco.plot(X, collapse=FALSE, log.transform=TRUE, group="cc", p.adjust.method="fdr",
           print.p.values=FALSE)

## ------------------------------------------------------------------------
glyco.plot(X, collapse=FALSE, log.transform=TRUE, group="cc", p.adjust.method="fdr",
           print.p.values=FALSE, glyco.names=c("GP1", "GP2"))

