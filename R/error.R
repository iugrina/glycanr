# igg.lcms.derived.traits error handling
# Useful if some glycans are missing
ildt.eh <- function(expr){
    tryCatch(
             expr
             , error = function(e) {
                 if(grepl("object '[^ ]*' not found", e$message)){
                     warning("not all derived traits could be calculated!")
                 } else {
                     stop(e)
                 }
             }
                 
    )
}
