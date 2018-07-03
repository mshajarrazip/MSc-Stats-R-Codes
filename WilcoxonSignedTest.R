rm(list = ls())

# reading data
datasrc <- "Input/X/WilcoxTest.csv"
my_data <- read.csv(datasrc)
my_data
rownames(my_data) <- my_data[,1]
my_data <- my_data[2:length(my_data)]
my_data


# Step 1: Get paired absolute differences
X_A <- my_data[1]
X_B <- my_data[2]
X_A
X_B
X <- X_A - X_B
X_nonzero <- X[X != 0]
X_nonzero
X_abs <- abs(X_nonzero)
X_abs

# Step 2: Rank the paired difference and reassign signs
X_sign <- X_nonzero/X_abs
X_sign
X_rank <- rank(X_abs)
X_rank
X_signed_rank <- X_sign * X_rank
X_signed_rank
W <- sum(X_signed_rank)
W
N <- length(X_signed_rank)
N

# Step 3: Calculate the z-value
sd_w <- sqrt((N*(N+1)*((2*N)+1))/6)
z <- 0
if (W > 0){
  z <- (W - .5)/sd_w
} else {
  z <- (W + .5)/sd_w
}
z

p_onetail <- pnorm(-abs(z))
p_onetail
p_twotail <- p_onetail*2
p_twotail
