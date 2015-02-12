data/earnings.rda: inst/exdata/earnings.sav
	Rscript -e "earnings <- foreign::read.spss('$<', use.value.labels=FALSE, max.value.labels=0, to.data.frame=TRUE); save(earnings, file='$@')"
