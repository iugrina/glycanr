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
#' Input data frame should have at least the following three columns:
#'   - gid - representing a unique name of a sample
#'   - glycan - representing glycan names
#'   - value - representing measured values
#' and if the subclasses argument is given it should also have column:
#'   - isoform - representing subclasses (e.g. IgG1, IgG2 and IgG4)
tanorm <- function(d, subclasses=FALSE){
    if(subclasses==FALSE){
        return(tanorm_basic(d))
    }else{
        return(tanorm_subclasses(d))
    }
}

tanorm_basic <- function(d){
    d <- d %>% 
        group_by(gid) %>%
		mutate(value = value/sum(value, na.rm = TRUE)*100) %>%
		ungroup()
	d
}

tanorm_subclasses <- function(d){	
	d <- d %>%
        group_by(isoform, gid) %>%
		mutate(value = value/sum(value, na.rm = TRUE)*100) %>%
		ungroup()
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
#' Input data frame should have at least the following three columns:
#'   - gid - representing a unique name of a sample
#'   - glycan - representing glycan names
#'   - value - representing measured values
#' and if the subclasses argument is given it should also have column:
#'   - isoform - representing subclasses (e.g. IgG1, IgG2 and IgG4)
refpeaknorm <- function(d, subclasses=FALSE){
    if(subclasses==FALSE){
        return(refpeaknorm_basic(d))
    }else{
        return(refpeaknorm_subclasses(d))
    }
}

refpeaknorm_basic <- function(d){
    tmp <- d %>% 
        group_by(glycan) %>% 
        summarise(s = sum(value, na.rm=TRUE)) %>% 
        ungroup() %>% 
        arrange(desc(s))

    max_peak <- as.character(tmp[,1]$glycan)

    d <- d %>%
        group_by(gid) %>% 
        mutate(value=value/value[glycan==max_peak]) %>% 
        ungroup()
	return(d)
}

refpeaknorm_subclasses <- function(d){
    d <- d %>% 
        group_by(isoform) %>% 
        do(refpeaknorm_basic(.)) %>% 
        ungroup()

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
#' Input data frame should have at least the following three columns:
#'   - gid - representing a unique name of a sample
#'   - glycan - representing glycan names
#'   - value - representing measured values
#' and if the subclasses argument is given it should also have column:
#'   - isoform - representing subclasses (e.g. IgG1, IgG2 and IgG4)
mediannorm <- function(d, subclasses=FALSE){
    if(subclasses==FALSE){
        return(mediannorm_basic(d))
    }else{
        return(mediannorm_subclasses(d))
    }
}

mediannorm_basic <- function(d) {
	d <- d %>%
		group_by(glycan) %>%
		mutate(value = (value - median(value, na.rm = TRUE))/IQR(value, na.rm = TRUE)) %>%
		ungroup() 
    d
}

mediannorm_subclasses = function(d) {
	d <- d %>%
		group_by(isoform, glycan) %>%
		mutate(value = (value - median(value, na.rm = TRUE))/IQR(value, na.rm = TRUE)) %>%
		ungroup() 
    d
}

#' Median Quantile Normalization of glycan data
#'
#' Returns glycans normalized with Median Quantile Normalization approach.
#'
#' @author Ivo Ugrina, Lucija Klarić
#' @export medianquotientnorm
#' @param d data frame in long format containing glycan measurements
#' @param subclasses should data be normalized per subclass
#' @return Returns a data.frame with original glycan values substituted by normalized ones
#' @details
#' Input data frame should have at least the following three columns:
#'   - gid - representing a unique name of a sample
#'   - glycan - representing glycan names
#'   - value - representing measured values
#' and if the subclasses argument is given it should also have column:
#'   - isoform - representing subclasses (e.g. IgG1, IgG2 and IgG4)
medianquotientnorm <- function(d, subclasses=FALSE){
    if(subclasses==FALSE){
        return(medianquotientnorm_basic(d))
    }else{
        return(medianquotientnorm_subclasses(d))
    }
}


medianquotientnorm_basic <- function(d){
    ref_chromx <- d %>% 
        group_by(glycan) %>% 
        mutate(mxxx=value/median(value, na.rm=TRUE)) %>% 
        ungroup()

    d <- ref_chromx %>% 
        group_by(gid) %>% 
        mutate(value=value/median(mxxx, na.rm=TRUE)) %>% 
        ungroup() %>% 
        select(-mxxx)

    d
}

medianquotientnorm_subclasses <- function(d){
    d <- d %>% 
        group_by(isoform) %>% 
        do(medianquotientnorm_basic(.)) %>% 
        ungroup()

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
#' Input data frame should have at least the following three columns:
#'   - gid - representing a unique name of a sample
#'   - glycan - representing glycan names
#'   - value - representing measured values
#' and if the subclasses argument is given it should also have column:
#'   - isoform - representing subclasses (e.g. IgG1, IgG2 and IgG4)
quantilenorm <- function(d, subclasses=FALSE, transpose=FALSE){
    library(preprocessCore)
    if(subclasses==FALSE){
        return(medianquotientnorm_basic(d, transpose))
    }else{
        return(medianquotientnorm_subclasses(d, transpose))
    }
}


quantilenorm_basic <- function(d, transpose=FALSE){
    tmp <- d %>% 
        select(gid, glycan, value) %>% 
        spread(glycan, value)

	glycans <- unique(d$glycan)

    if(transpose){
        tempD <- normalize.quantiles(as.matrix(tmp[, glycans]))
    }else{
        tempD <- normalize.quantiles(t(as.matrix(tmp[, glycans])))
    }

    if(transpose){
        tmp[,glycans] <- tempD
    }else{
        tmp[,glycans] <- t(tempD)
    }

    tmp <- tmp %>% 
        gather_("glycan", "value", glycans)

    d <- d %>% 
        select(-value)
    d <- merge(d, tmp, by=c("gid", "glycan"))

	return(d)
}

quantilenorm_subclasses <- function(d, transpose=FALSE){
    d <- d %>% 
        group_by(isoform) %>% 
        do(quantilenorm_basic(., transpose)) %>% 
        ungroup()

    d
}

