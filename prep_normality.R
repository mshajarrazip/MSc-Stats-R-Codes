rm(list = ls())
# testing for normality
# reference: http://www.sthda.com/english/wiki/normality-test-in-r

install.packages("dplyr")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library("dplyr")
library("ggpubr")

datasrc <- "GA_GEN.csv"
my_data <- read.csv(datasrc)
row.names(my_data) <- my_data$Probname
my_data <- my_data[2:length(my_data)]
my_data
varnames <- colnames(my_data)
varnames
# display random sample of 10 rows
set.seed(1234)
dplyr::sample_n(my_data, 10)

# The central limit theorem: no matter what distribution things have, 
# the sampling distribution tends to be normal i the sample os large
# enough (n>30)
n <- length(my_data[,1])
n

# creating output folders
mainDir <- "Output"
dir.create(mainDir, showWarnings = FALSE)
# checking normality

# visual
# QQ-plot - if all points fall approx. on reference line, we can
# assume normality

# creates the qqplot output - j is the index of column
gen_qqplot <- function(outDir, filename, my_data, j){
  jpeg(file.path(mainDir, outDir, filename))
  qq <-ggqqplot(my_data[,j])
  print(qq)
  dev.off()
} 

#gen_qqplot(outDir, "yolo.jpg", my_data, 1)

tstamp <- as.numeric(Sys.time(), units="secs")
outDir <- paste("QQPLOT", datasrc, toString(tstamp), sep="_" ) 
dir.create(file.path(mainDir, outDir))
outDir
for (j in 1:length(my_data)){
  fname <- paste("qqplot", varnames[j], "jpg", sep=".")
  gen_qqplot(outDir, fname, my_data, j)
}

# normality test
# shapiro-test: if p-value > 0.05, then the distribution
# of data are not significantly different from normal
# distribution - so we can assume normality
# log transformation

log_my_data <- my_data
for (j in 1:length(my_data)) {
  print(varnames[j])
  col_log10 <- log10(my_data[, j])
  log_my_data[,j] <- col_log10
}
log_my_data

for (j in 1:length(my_data)){
  print(varnames[j])
  s<-shapiro.test(my_data[,j])
  print(s)
}

# normality test on log-transformed data
for (j in 1:length(log_my_data)){
  print(varnames[j])
  s<-shapiro.test(log_my_data[,j])
  print(s)
}



