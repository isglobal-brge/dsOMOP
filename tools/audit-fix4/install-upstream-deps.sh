set -eu
export R_LIBS_USER=/root/rt/audit-fix4-deps:/root/rt/numeric-deps:/root/rt/labels-deps
Rscript -e 'install.packages("DatabaseConnector",lib="/root/rt/audit-fix4-deps",repos=c("https://ohdsi.r-universe.dev","https://cloud.r-project.org"),Ncpus=2)'
R CMD INSTALL -l /root/rt/audit-fix4-deps /root/rt/audit-fix4-fixtures/CommonDataModel.tar.gz
R CMD INSTALL -l /root/rt/audit-fix4-deps /root/rt/audit-fix4-fixtures/Eunomia.tar.gz
