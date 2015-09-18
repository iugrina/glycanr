#' Total Area Normalization of glycan data
#'
#' Returns glycans normalized with Total Area Normalization approach.
#'
#' @author Ivo Ugrina
#' @export tanorm
#' @param data data frame which holds columns representing Glycans.
#' @param glyco.names names of columns that represent glycan data. If \code{NULL}
#'   all columns starting with 'GP' in their names will be used
#' @return Returns a data.frame with original glycan values substituted by normalized ones
#' @examples
#' exampleData <- data.frame(ID=1:100, GP1=runif(100),
#'   GP2=rexp(100,0.2), GP3=rgamma(100, 3),
#'   Plate=factor(sample(1:2,100,replace=TRUE)))
#' print(head(tanorm(exampleData)))
tanorm <- function(data, glyco.names=NULL){	
    if (is.null(glyco.names)) {
        tmp <- grep("^GP", names(data))
        gps <- names(data)[tmp]
        not.gps <- names(data[-tmp])
    } else {
        gps <- glyco.names
        not.gps <- names(data)[!names(data) %in% gps]
    }
	data[, gps] <- t(apply(data[, gps], 1,
                           function(x){ x/sum(x)*100 }
                           ))
	return(data)
}
