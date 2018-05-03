rm(list = ls())

# reading data
datasrc <- "D_AAR_AARLO.csv"
my_data <- read.csv(datasrc)
my_data
rownames(my_data) <- my_data$Problem
my_data <- my_data[2:length(my_data)]
my_data

varnames <- colnames(my_data)
varnames

# calculate differences
my_data_diff <- my_data[,1]-my_data[,2]

# remove NAs and calculate sample size
my_data_diff[my_data_diff == 0.0] <- NA
my_data_diff <- my_data_diff[!is.na(my_data_diff)]
my_data_diff
n <- length(my_data_diff)

# calculate positives
pos <- length(my_data_diff[my_data_diff > 0.0])
neg <- n - pos
pos
neg

# set the success event (smaller of pos and neg)
x <- min(c(pos, neg))
x

# calculate p-value
p <- dbinom(x, size=n, prob=0.5)
p

# if p<0.05, there is a significant difference in median