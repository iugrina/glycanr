library(devtools)

load_all()
document()
build_vignettes()
build()
install(build_vignettes = TRUE)
