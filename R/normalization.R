#' Total Area Normalization of glycan data
#'
#' Returns glycans normalized with Total Area Normalization approach.
#'
#' @author Ivo Ugrina
#' @export
#' @param data data frame which holds columns representing Glycans.
#'   These column names should start with 'GP'.
#' @return Returns a data.frame with original glycan values substituted by normalized ones
#' @examples
#' exampleData <- data.frame(ID=1:100, GP1=runif(100),
#'   GP2=rexp(100,0.2), GP3=rgamma(100, 3),
#'   Plate=factor(sample(1:2,100,replace=TRUE)))
#' print(head(tanorm(exampleData)))
tanorm <- function(data){	
	glycans <- grep("^GP\\d+$|^x\\d+$",
                    names(data),
                    ignore.case = TRUE)
	data[, glycans] <- t(apply(data[, glycans], 1,
                            function(x){ x/sum(x)*100 }
                            ))
	return(data)
}
