# Fix the Non-standard evaluation usage for check()
if(getRversion() >= "2.15.1"){
    utils::globalVariables(c("value", "variable", "outlier", "desc"))
}

#' Discover outliers in glycan data
#'
#' Returns outliers in data columns starting with GP (representing glycans)
#' sometimes grouped by other variables
#'
#' @author Ivo Ugrina
#' @export
#' @importFrom dplyr %>%
#' @importFrom grDevices boxplot.stats
#' @importFrom stats IQR kruskal.test p.adjust
#' @param data data frame which holds columns representing Glycans.
#'   These column names should start with 'GP'.
#' @param group this a possible grouping parameter on which
#'   stratification of \code{data} should be conducted. It should be
#'   a name of one of the columns in dataframe \code{data}
#'   and of type \code{factor}.
#' @param outlier.function is a function that checks for outliers in
#'   a vector. Receives one parameter representing a vector and returns
#'   logical vector indicating outliers.
#' @param glyco.names names of columns that represent glycan data. If \code{NULL}
#'   all columns starting with 'GP' in their names will be used
#' @return Returns a data.frame with outliers 
#' @examples
#' exampleData <- data.frame(ID=1:100, GP1=runif(100),
#'   GP2=rexp(100,0.2), GP3=rgamma(100, 3),
#'   Plate=factor(sample(1:2,100,replace=TRUE)))
#' glyco.outliers(exampleData)
#' glyco.outliers(exampleData, group="Plate")
glyco.outliers <- function(data, group=NULL, outlier.function=NULL,
			   glyco.names=NULL) {
    
    outf <- function(x) {
        lq <- boxplot.stats(x)$stats[2]
        uq <- boxplot.stats(x)$stats[4]
        
        ifelse(x > uq + 1.5 * IQR(x) | x < lq - 1.5 * IQR(x), TRUE, FALSE)
    }
    
    stopifnot(is.data.frame(data))
    if (!is.null(outlier.function)) {
        stopifnot(is.function(outlier.function))
        outf <- outlier.function
    }
    
    if (is.null(glyco.names)) {
        tmp <- grep("^GP", names(data))
        gps <- names(data)[tmp]
    } else {
        gps <- glyco.names
    }
    
    newdata <- tidyr::gather_(data, "variable", "value", gps)
    
    if (is.null(group)) {
        X.out <- newdata %>%
          dplyr::group_by(variable) %>%
          dplyr::mutate(outlier = outf(value))
    } else {
        g <- lapply(c("variable", group), as.symbol)
        X.out <- newdata %>%
          dplyr::group_by_(.dots=g) %>%
          dplyr::mutate(outlier = outf(value))
    }
    
    dplyr::select(dplyr::filter(X.out, outlier == TRUE), -outlier)
} 
