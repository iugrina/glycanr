#' Derived traits for Glycan peaks in IgG for UPLC
#'
#' Calcuates values of derived traits for Glycan peaks in IgG for UPLC
#'
#' @author Ivo Ugrina; Frano Vučković
#' @export igg.uplc.derived.traits
#' @param data data frame which holds columns representing Glycans.
#'   These column names should start with 'GP'.
#' @return Returns a data.frame with derived traits
#' @references
#' Jennifer E. Huffman et al. 
#' "Comparative Performance of Four Methods for High-throughput Glycosylation Analysis of Immunoglobulin G in Genetic and Epidemiological Research*"
#' \link{http://dx.doi.org/10.1074/mcp.M113.037465 }
igg.uplc.derived.traits <- function(data) {
    
    # derived glycans
    data$IGP24 = with(data, (GP16 + GP18 + GP23)/(GP16 + GP18 + GP23 + GP8 + GP9 + 
        GP14)) * 100
    data$IGP25 = with(data, (GP19 + GP24)/(GP19 + GP24 + GP10 + GP11 + GP15)) * 100
    data$IGP26 = with(data, (GP16 + GP18 + GP23)/(GP16 + GP18 + GP23 + GP4 + GP8 + 
        GP9 + GP14)) * 100
    data$IGP27 = with(data, (GP19 + GP24)/(GP19 + GP24 + GP6 + GP10 + GP11 + GP15)) * 
        100
    data$IGP28 = with(data, GP16/(GP16 + GP8 + GP9)) * 100
    data$IGP29 = with(data, GP18/(GP18 + GP14 + GP23)) * 100
    data$IGP30 = with(data, GP23/(GP23 + GP14 + GP18)) * 100
    data$IGP31 = with(data, GP19/(GP19 + GP15 + GP24)) * 100
    data$IGP32 = with(data, GP24/(GP24 + GP15 + GP19)) * 100
    data$IGP33 = with(data, (GP16 + GP18 + GP19)/(GP23 + GP24))
    data$IGP34 = with(data, (GP16 + GP18)/GP23)
    data$IGP35 = with(data, GP19/GP24)
    data$IGP36 = with(data, (GP19 + GP24)/(GP16 + GP18 + GP23))
    data$IGP37 = with(data, GP19/(GP16 + GP18))
    data$IGP38 = with(data, GP19/(GP16 + GP18 + GP19)) * 100
    data$IGP39 = with(data, GP24/GP23)
    data$IGP40 = with(data, GP24/(GP23 + GP24)) * 100
    
    # neutral glycans
    GPn = with(data, GP1 + GP2 + GP4 + GP6 + GP7 + GP8 + GP9 + GP10 + GP11 + GP12 + 
        GP13 + GP14 + GP15)
    data$IGP41 = with(data, GP1/GPn) * 100
    data$IGP42 = with(data, GP2/GPn) * 100
    data$IGP43 = with(data, GP4/GPn) * 100
    data$IGP44 = with(data, GP5/GPn) * 100
    data$IGP45 = with(data, GP6/GPn) * 100
    data$IGP46 = with(data, GP7/GPn) * 100
    data$IGP47 = with(data, GP8/GPn) * 100
    data$IGP48 = with(data, GP9/GPn) * 100
    data$IGP49 = with(data, GP10/GPn) * 100
    data$IGP50 = with(data, GP11/GPn) * 100
    data$IGP51 = with(data, GP12/GPn) * 100
    data$IGP52 = with(data, GP13/GPn) * 100
    data$IGP53 = with(data, GP14/GPn) * 100
    data$IGP54 = with(data, GP15/GPn) * 100
    
    # neutral derived glycans
    data$IGP55 = with(data, (IGP41 + IGP42 + IGP43 + IGP45))
    data$IGP56 = with(data, (IGP46 + IGP47 + IGP48 + IGP49 + IGP50))
    data$IGP57 = with(data, (IGP51 + IGP52 + IGP53 + IGP54))
    data$IGP58 = with(data, (IGP41 + IGP43 + IGP45 + IGP47 + IGP48 + IGP49 + IGP50 + 
        IGP53 + IGP54))
    data$IGP59 = with(data, (IGP41 + IGP43 + IGP45)/IGP55) * 100
    data$IGP60 = with(data, (IGP47 + IGP48 + IGP49 + IGP50)/IGP56) * 100
    data$IGP61 = with(data, (IGP53 + GP15)/IGP57) * 100
    data$IGP62 = with(data, (IGP41 + IGP43 + IGP47 + IGP48 + IGP53))
    data$IGP63 = with(data, (IGP41 + IGP43)/IGP55) * 100
    data$IGP64 = with(data, (IGP47 + IGP48)/IGP56) * 100
    data$IGP65 = with(data, IGP53/IGP57) * 100
    data$IGP66 = with(data, (IGP45 + IGP49 + IGP50 + IGP54))
    data$IGP67 = with(data, IGP45/IGP55) * 100
    data$IGP68 = with(data, (IGP49 + IGP50)/IGP56) * 100
    data$IGP69 = with(data, IGP54/IGP57) * 100
    data$IGP70 = with(data, IGP66/IGP62) * 100
    data$IGP71 = with(data, IGP66/IGP58) * 100
    data$IGP72 = with(data, IGP62/(IGP52 + IGP66))
    data$IGP73 = with(data, IGP52/(IGP62 + IGP66)) * 1000
    data$IGP74 = with(data, IGP54/IGP53)
    data$IGP75 = with(data, IGP54/(IGP53 + IGP54)) * 100
    data$IGP76 = with(data, IGP53/(IGP52 + IGP54))
    data$IGP77 = with(data, IGP52/(IGP53 + IGP54)) * 1000
    
    return(data)
}

