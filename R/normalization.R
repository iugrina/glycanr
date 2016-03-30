# Fix the Non-standard evaluation usage for check()
if(getRversion() >= "2.15.1"){
    utils::globalVariables(c("value", "variable", "glycan", "isoform", ".",
                             "transpose", "gid", "mxxx", "s"))
}

#' Total Area Normalization of glycan data
#'
#' Returns glycans normalized with Total Area Normalization approach.
#'
#' @author Ivo Ugrina
#' @export tanorm
#' @param d data frame in long format containing glycan measurements
#' @param subclasses should data be normalized per subclass
#' @return Returns a data.frame with original glycan values substituted by normalized ones
#' @details
#' Input data frame should have at least the following three columns: \cr
#'   - gid - representing a unique name of a sample \cr
#'   - glycan - representing glycan names \cr
#'   - value - representing measured values \cr
#' and if the subclasses argument is given it should also have column: \cr
#'   - isoform - representing subclasses (e.g. IgG1, IgG2 and IgG4)
#' @examples
#' data(mpiu)
#' mpiun <- tanorm(mpiu)
#' head(mpiun)
tanorm <- function(d, subclasses=FALSE){
    if(subclasses==FALSE){
        return(tanorm_basic(d))
    }else{
        return(tanorm_subclasses(d))
    }
}

tanorm_basic <- function(d){
    d <- d %>% 
        dplyr::group_by(gid) %>%
		dplyr::mutate(value = value/sum(value, na.rm = TRUE)*100) %>%
		dplyr::ungroup()
	d
}

tanorm_subclasses <- function(d){	
	d <- d %>%
        dplyr::group_by(isoform, gid) %>%
		dplyr::mutate(value = value/sum(value, na.rm = TRUE)*100) %>%
		dplyr::ungroup()
	d
}

#' Reference Peak Normalization of glycan data
#'
#' Returns glycans normalized with Reference Peak Normalization approach.
#'
#' @author Ivo Ugrina, Lucija Klarić
#' @export refpeaknorm
#' @param d data frame in long format containing glycan measurements
#' @param subclasses should data be normalized per subclass
#' @return Returns a data.frame with original glycan values substituted by normalized ones
#' @details
#' Input data frame should have at least the following three columns: \cr
#'   - gid - representing a unique name of a sample \cr
#'   - glycan - representing glycan names \cr
#'   - value - representing measured values \cr
#' and if the subclasses argument is given it should also have column: \cr
#'   - isoform - representing subclasses (e.g. IgG1, IgG2 and IgG4)
#' @examples
#' data(mpiu)
#' mpiun <- refpeaknorm(mpiu)
#' head(mpiun)
refpeaknorm <- function(d, subclasses=FALSE){
    if(subclasses==FALSE){
        return(refpeaknorm_basic(d))
    }else{
        return(refpeaknorm_subclasses(d))
    }
}

refpeaknorm_basic <- function(d){
    tmp <- d %>% 
        dplyr::group_by(glycan) %>% 
        dplyr::summarise(s = sum(value, na.rm=TRUE)) %>% 
        dplyr::ungroup() %>% 
        dplyr::arrange(desc(s))

    max_peak <- as.character(tmp[1,]$glycan)

    d <- d %>%
        dplyr::group_by(gid) %>% 
        dplyr::mutate(value=value/value[glycan==max_peak]) %>% 
        dplyr::ungroup()
	return(d)
}

refpeaknorm_subclasses <- function(d){
    d <- d %>% 
        dplyr::group_by(isoform) %>% 
        dplyr::do(refpeaknorm_basic(.)) %>% 
        dplyr::ungroup()

    d
}

#' Median Normalization of glycan data
#'
#' Returns glycans normalized with Median Normalization approach.
#'
#' @author Ivo Ugrina, Lucija Klarić
#' @export mediannorm
#' @param d data frame in long format containing glycan measurements
#' @param subclasses should data be normalized per subclass
#' @return Returns a data.frame with original glycan values substituted by normalized ones
#' @details
#' Input data frame should have at least the following three columns: \cr
#'   - gid - representing a unique name of a sample \cr
#'   - glycan - representing glycan names \cr
#'   - value - representing measured values \cr
#' and if the subclasses argument is given it should also have column: \cr
#'   - isoform - representing subclasses (e.g. IgG1, IgG2 and IgG4)
#' @examples
#' data(mpiu)
#' mpiun <- mediannorm(mpiu)
#' head(mpiun)
mediannorm <- function(d, subclasses=FALSE){
    if(subclasses==FALSE){
        return(mediannorm_basic(d))
    }else{
        return(mediannorm_subclasses(d))
    }
}

mediannorm_basic <- function(d) {
	d <- d %>%
		dplyr::group_by(gid) %>%
		dplyr::mutate(value = (value - stats::median(value, na.rm = TRUE))/IQR(value, na.rm = TRUE)) %>%
		dplyr::ungroup() 
    d
}

mediannorm_subclasses = function(d) {
	d <- d %>%
		dplyr::group_by(isoform, gid) %>%
		dplyr::mutate(value = (value - stats::median(value, na.rm = TRUE))/IQR(value, na.rm = TRUE)) %>%
		dplyr::ungroup() 
    d
}

#' Median Quotient Normalization of glycan data
#'
#' Returns glycans normalized with Median Quotient Normalization approach.
#'
#' @author Ivo Ugrina, Lucija Klarić
#' @export medianquotientnorm
#' @param d data frame in long format containing glycan measurements
#' @param subclasses should data be normalized per subclass
#' @return Returns a data.frame with original glycan values substituted by normalized ones
#' @details
#' Input data frame should have at least the following three columns: \cr
#'   - gid - representing a unique name of a sample \cr
#'   - glycan - representing glycan names \cr
#'   - value - representing measured values \cr
#' and if the subclasses argument is given it should also have column: \cr
#'   - isoform - representing subclasses (e.g. IgG1, IgG2 and IgG4)
#' @references
#' Dieterle F,Ross A, Schlotterbeck G, Senn H.: \cr
#' Probabilistic Quotient Normalization as Robust Method to Account for
#' Diluition of Complex Biological Mixtures. Application in 1H NMR Metabolomics. \cr
#' Anal Chem 2006;78:4281-90. \cr
#' \url{http://dx.doi.org/10.1021/ac051632c}
#' @examples
#' data(mpiu)
#' mpiun <- medianquotientnorm(mpiu)
#' head(mpiun)
medianquotientnorm <- function(d, subclasses=FALSE){
    if(subclasses==FALSE){
        return(medianquotientnorm_basic(d))
    }else{
        return(medianquotientnorm_subclasses(d))
    }
}

medianquotientnorm_basic <- function(d){
    ref_chromx <- d %>% 
        dplyr::group_by(glycan) %>% 
        dplyr::mutate(mxxx=value/stats::median(value, na.rm=TRUE)) %>% 
        dplyr::ungroup()

    d <- ref_chromx %>% 
        dplyr::group_by(gid) %>% 
        dplyr::mutate(value=value/stats::median(mxxx, na.rm=TRUE)) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(-mxxx)

    d
}

medianquotientnorm_subclasses <- function(d){
    d <- d %>% 
        dplyr::group_by(isoform) %>% 
        dplyr::do(medianquotientnorm_basic(.)) %>% 
        dplyr::ungroup()

    d
}

#' Quantile Normalization of glycan data
#'
#' Returns glycans normalized with Quantile Normalization approach.
#'
#' @author Ivo Ugrina, Lucija Klarić
#' @export quantilenorm
#' @param d data frame in long format containing glycan measurements
#' @param transpose transpose the data prior to normalization
#' @param subclasses should data be normalized per subclass
#' @return Returns a data.frame with original glycan values substituted by normalized ones
#' @details
#' Input data frame should have at least the following three columns: \cr
#'   - gid - representing a unique name of a sample \cr
#'   - glycan - representing glycan names \cr
#'   - value - representing measured values \cr
#' and if the subclasses argument is given it should also have column: \cr
#'   - isoform - representing subclasses (e.g. IgG1, IgG2 and IgG4)
#' @references
#' Bolstad, B. M., Irizarry R. A., Astrand, M, and Speed, T. P.: \cr
#' A Comparison of Normalization Methods for High Density Oligonucleotide
#' Array Data Based on Bias and Variance.\cr
#' Bioinformatics 19(2), p. 185-193, 2003. \cr
#' \url{http://dx.doi.org/10.1093/bioinformatics/19.2.185}
#' @examples
#' data(mpiu)
#' mpiun <- quantilenorm(mpiu)
#' head(mpiun)
#' 
#' # transpose (change) subjects and measurements
#' mpiunt <- quantilenorm(mpiu, transpose=TRUE)
#' head(mpiunt)
quantilenorm <- function(d, subclasses=FALSE, transpose=FALSE){
    if(!requireNamespace("preprocessCore", quietly=TRUE)){
        stop("Unable to proceed since package preprocessCore from
        BioConductor is not available on this system. This
        package is a prerequisite to use the quantilenorm function!")
    }

    if(subclasses==FALSE){
        return(quantilenorm_basic(d, transpose))
    }else{
        return(quantilenorm_subclasses(d, transpose))
    }
}


quantilenorm_basic <- function(d, transpose=FALSE){
    if(!requireNamespace("preprocessCore", quietly=TRUE)){
        stop("Unable to proceed since package preprocessCore from
        BioConductor is not available on this system. This
        package is a prerequisite to use the quantilenorm function!")
    }

    tmp <- d %>% 
        dplyr::select(gid, glycan, value) %>% 
        tidyr::spread(glycan, value)

	glycans <- as.character(unique(d$glycan))

    if(transpose){
        tempD <- preprocessCore::normalize.quantiles(as.matrix(tmp[, glycans]))
    }else{
        tempD <- preprocessCore::normalize.quantiles(t(as.matrix(tmp[, glycans])))
    }

    if(transpose){
        tmp[,glycans] <- tempD
    }else{
        tmp[,glycans] <- t(tempD)
    }

    tmp <- tmp %>% 
        tidyr::gather_("glycan", "value", glycans)

    d <- d %>% 
        dplyr::select(-value)
    d <- merge(d, tmp, by=c("gid", "glycan"))

	return(d)
}

quantilenorm_subclasses <- function(d, transpose=FALSE){
    d <- d %>% 
        dplyr::group_by(isoform) %>% 
        dplyr::do(quantilenorm_basic(., transpose)) %>% 
        dplyr::ungroup()

    d
}

