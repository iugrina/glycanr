#' Derived traits for Glycan peaks in IgG for UPLC
#'
#' Calcuates values of derived traits for Glycan peaks in IgG for UPLC
#'
#' @author Ivo Ugrina
#' @export iudt
#' @param data data frame that holds columns representing Glycans.
#'   These column names should start with 'GP'.
#' @param method year of the derived traits definition. By default 2014.
#' @return Returns a data.frame with derived traits
#' @details
#' Calculates derived traits from basic glycan peaks. User can choose
#' which definition of the derived traits he will use
#' (see references for different versions/definitions of derived traits).
#' 
#' @references
#' Jennifer E. Huffman et al. (2014)
#' "Comparative Performance of Four Methods for High-throughput Glycosylation Analysis of Immunoglobulin G in Genetic and Epidemiological Research*"
#' \url{http://dx.doi.org/10.1074/mcp.M113.037465}
iudt <- function(data, method="2014") {
    x <- NULL

    if(method == "2014"){
       x <- igg.uplc.derived.traits.2014(data)
    }

    x
}


#' Derived traits for Glycan peaks in PLASMA for HPLC
#'
#' Calcuates values of derived traits for Glycan peaks in Plasma for HPLC
#'
#' @author Ivo Ugrina
#' @export phdt
#' @param data data frame that holds columns representing Glycans.
#'   These column names should start with 'GP'.
#' @param method year of the derived traits definition. By default 2011.
#' @return Returns a data.frame with derived traits
#' @details
#' Calculates derived traits from basic glycan peaks. User can choose
#' which definition of the derived traits he will use
#' (see references for different versions/definitions of derived traits).
#' 
#' @references
#' Lu et al. (2011)
#' "Screening Novel Biomarkers for Metabolic Syndrome by Profiling 
#'  Human Plasma N-Glycans in Chinese Han and Croatian Populations"
#' \url{http://dx.doi.org/10.1021/pr2004067}
phdt <- function(data, method="2011") {
    x <- NULL

    if(method == "2011"){
       x <- plasma.hplc.derived.traits.2011(data)
    }

    x
}


#' Derived traits for Glycan peaks in IgG for LCMS
#'
#' Calcuates values of derived traits for Glycan peaks in IgG for LCMS
#'
#' @author Ivo Ugrina
#' @export ildt
#' @param data data frame that holds columns representing Glycans.
#' @param method year of the derived traits definition. By default 2014.
#' @return Returns a data.frame with derived traits
#' @details
#' Calculates derived traits from basic glycan peaks. User can choose
#' which definition of the derived traits he will use
#' (see references for different versions/definitions of derived traits).
#' 
#' @references
#' Jennifer E. Huffman et al. (2014)
#' "Comparative Performance of Four Methods for High-throughput Glycosylation Analysis of Immunoglobulin G in Genetic and Epidemiological Research*"
#' \url{http://dx.doi.org/10.1074/mcp.M113.037465}
ildt <- function(data, method="2014") {
    x <- NULL
 
    if(method == "2014"){
       x <- igg.lcms.derived.traits.2014(data)
    }

    x
}

#' Translate names between computer readable and human readable
#' for derived traits of IgG with LCMS
#'
#' Translates names between computer readable and human readable
#' for derived traits of IgG with LCMS
#'
#' @author Ivo Ugrina
#' @export ildt.translate
#' @param orignames vector; type string
#' @param method year of the derived traits definition. By default 2014.
#' @return Returns a character vector with original and translated names
#' @details
#' User can choose which definition of the derived traits he will use
#' (see references for different versions/definitions of derived traits).
#' 
#' @references
#' Jennifer E. Huffman et al. (2014)
#' "Comparative Performance of Four Methods for High-throughput Glycosylation Analysis of Immunoglobulin G in Genetic and Epidemiological Research*"
#' \url{http://dx.doi.org/10.1074/mcp.M113.037465}
ildt.translate <- function(orignames, method="2014") {
    x <- NULL
  
    if(method == "2014"){
       x <- ildt.translate.2014(orignames)
    }

    x
}

#' Translate names between computer readable and human readable
#' for derived traits of IgG with UPLC
#'
#' Translates names between computer readable and human readable
#' for derived traits of IgG with UPLC
#'
#' @author Ivo Ugrina
#' @export iudt.translate
#' @param orignames vector; type string
#' @param method year of the derived traits definition. By default 2014.
#' @return Returns a character vector with original and translated names
#' @details
#' User can choose which definition of the derived traits he will use
#' (see references for different versions/definitions of derived traits).
#' 
#' @references
#' Jennifer E. Huffman et al. (2014)
#' "Comparative Performance of Four Methods for High-throughput Glycosylation Analysis of Immunoglobulin G in Genetic and Epidemiological Research*"
#' \url{http://dx.doi.org/10.1074/mcp.M113.037465}
iudt.translate <- function(orignames, method="2014") {
    x <- NULL
  
    if(method == "2014"){
       x <- iudt.translate.2014(orignames)
    }

    x
}
