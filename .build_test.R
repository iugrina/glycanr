library(devtools)

load_all()
document()
build()
install(build_vignettes = TRUE)
