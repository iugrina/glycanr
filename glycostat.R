library(devtools)

load_all()

glyco.outliers(kt.plasma)
glyco.outliers(kt.plasma, group=c("Plate"))

glyco.plot(filter(kt.plasma,MajaSumnja==0), collapse=F, violin=T, all=T, group="Plate", log=F)

tmp <- igg.derived.glycans(crc.igg.data)

