# return outliers in data columns starting with GP (representing glycans)
# sometimes grouped by other variables
glyco.outliers <- function(data, group, outlier.function) {
    
    outf <- function(x) {
        lq <- boxplot.stats(x)$stats[2]
        uq <- boxplot.stats(x)$stats[4]
        
        ifelse(x > uq + 1.5 * IQR(x) | x < lq - 1.5 * IQR(x), TRUE, FALSE)
    }
    
    stopifnot(is.data.frame(data))
    if (!missing(outlier.function)) {
        stopifnot(is.function(outlier.function))
        outf <- outlier.function
    }
    
    tmp <- grep("GP", names(data))
    gps <- names(data)[tmp]
    not.gps <- names(data[-tmp])
    
    newdata <- reshape2::melt(data, id = not.gps)
    
    if (missing(group)) {
        X.out <- newdata %>% dplyr::group_by(variable) %>% dplyr::mutate(outlier = outf(value))
    } else {
        g <- lapply(c("variable", group), as.symbol)
        X.out <- newdata %>% dplyr::regroup(g) %>% dplyr::mutate(outlier = outf(value))
    }
    
    dplyr::filter(X.out, outlier == TRUE)
} 
