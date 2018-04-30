rm(list = ls())
# Friedman Test
# The non-parametric repeated measures ANOVA
# example is from http://vassarstats.net/textbook/ch15a.html
# Test data is in Friedman_Test.csv
# Replace fname with your data

# About test data:
# A subject is a violinist, testing violins ABC, and giving 
# giving each a score 1-10 for instrument rating 

# NULL HYPOTHESIS: All groups do not differ
fname <- "GA_GEN.csv"
my_data <- read.csv(fname)

# STEP 1: Rank-ordering the subjects 
# Our test ranks largest value in a row the lowest
# rank range 1 - k ( k lowest )
# ties rank the average of their ranks
row.names(my_data) <- my_data[,1]
my_data <- my_data[,2:length(my_data)]
k <- length(my_data)
n <- length(my_data[,1])
my_data

my_rank_data <- my_data
for (i in 1:n){
  my_rank_data[i,] <- rank(my_data[i,], na.last="keep", ties.method = "average")
}
my_rank_data
groups <- colnames(my_rank_data)
groups

# STEP 2: Calculate sums and means of the ranks
sums <- sapply(my_rank_data, sum)
means <- sapply(my_rank_data, mean)

sums_all <- (n*k)*((k+1)/2)
mean_all <- (k+1)/2
sums_all
mean_all

# STEP 3: The measure of aggregate group differences
# ... the measure of aggregate degree to which the k group means differ
# ... It is the between-groups sum of squared deviates 
# ... the squared deviate for any particular group mean
# is equal to the squared difference between that group mean
# and the mean of the overall array of data, multiplied
# by the number of observations on which the group mean is based

nk <- n*k
nk

SS <- mapply(function(x,y) {n*((x-y)*(x-y))}, means, mean_all)
SS
SS_bg <- sum(SS)
SS_bg

# next: The sampling distribution of SS_bg
X2 = SS_bg/(k*(k+1)/12)
X2

# Referring to the Chi2 sampling distribution
# X2 >= 5.99 5% of the distribution
# X2 >= 7.38 2.5% of the distribution
# X2 >= 9.21 1.0% of the distribution

# Example outcome report:
# Our musical arts foundation can therefore conclude
# with considerable confidence that the observed 
# differences among the mean rankings for the three violins 
# reflect something more than mere random variability,
# something more than mere chance coincidence among the 
# judgments of the expert players. 
