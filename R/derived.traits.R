#' Derived traits for Glycan peaks in IgG for UPLC
#'
#' Calcuates values of derived traits for Glycan peaks in IgG for UPLC
#'
#' @author Ivo Ugrina
#' @export igg.uplc.derived.traits
#' @param data data frame which holds columns representing Glycans.
#'   These column names should start with 'GP'.
#' @return Returns a data.frame with derived traits
igg.uplc.derived.traits <- function(data, method="2014") {
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
#' @export plasma.hplc.derived.traits
#' @param data data frame which holds columns representing Glycans.
#'   These column names should start with 'GP'.
#' @return Returns a data.frame with derived traits
plasma.hplc.derived.traits <- function(d, method="2011") {
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
#' @export igg.lcms.derived.traits
#' @param data data frame in in long format.
#' @return Returns a data.frame with derived traits
igg.lcms.derived.traits <- function(data, method="2014") {
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
#' @return Returns a character vector with original and translated names
ildt.translate <- function(orignames, method="2014") {
    x <- NULL
  
    if(method == "2014"){
       x <- ildt.translate.2014(orignames)
    }

    x
}
