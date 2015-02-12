# files to be prepared
files=data/earnings.rda data/gender_roles.rda

.PHONY: all
all: $(files)

data/earnings.rda: inst/exdata/earnings.sav
	Rscript -e "earnings <- foreign::read.spss('$<', use.value.labels=FALSE, max.value.labels=0, to.data.frame=TRUE); save(earnings, file='$@')"

data/gender_roles.rda: inst/exdata/gender_roles.txt
	Rscript -e "gender_roles <- read.table('$<', header=TRUE, as.is=TRUE); save(gender_roles, file='$@')"
