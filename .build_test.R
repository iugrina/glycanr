library(devtools)

load_all()
document()
build()
install(build_vignettes = TRUE)

# pre release check
check(cran = TRUE)
build_win()
revdep_check()
