library(devtools)
# library(revdepcheck)

load_all()
document()
build()
install(build_vignettes = TRUE)

# pre release check
# shell: R CMD check --as-cran glycanr_x.y.z.tar.gz
check(cran = TRUE)
check_win_devel()
check_win_oldrelease()
check_win_release()
#revdep_check()

spell_check()
check_rhub()

# release to CRAN
release()
