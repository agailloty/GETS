# The General To Specific (GETS)

# To appreciate Hoover and Perez's contributions to general-to-specific modeling, 
# consider the most basic steps that such an algorithm follows.
# 
# Ascertain that the general statistical model is congruent.
# Eliminate a variable (or variables) that satisfies the 
# selection (i.e., simplification) criteria.
# 
# Check that the simplified model remains congruent.
# Continue steps 2 and 3 until none of the remaining variables can be eliminated.

# IMPLEMENT A MECHANISM TO ADD LAGGED VARIABLES TO DATA

shift <- function(x, lag = 1) {
  # Takes a vector of length n and returns a vector n-1 where the first element
  # is lagged
   c(rep(NA, lag), x[1:(length(x) - lag)])
}

staggerLags <- function(data, lags = 5) {
  # This function takes as input a matrix or dataframe of shape n * k. 
  # It returns a dataframe/matrix of shape n-lag * k*lags
  # Example a dataframe of 200 * 5 will become 195 * 25 where the new 
  # columns are lags of the initial columns. 
  LAG <- lags
  lagVector <- function(x, lags = LAG, name) {
    # takes a numeric vector of length n and returns a matrix of 
    # shape n-lags * lags
    # Example : a vector of length 200 on which we apply 5 lags
    # will become a matrix of shape 195 * 5
    out = vector("list")
    out[[name]] = x
    for (i in seq(lags)) {
      out[[paste0(name, "_", (lags - i) + 1)]] <- shift(x, i)
    }
    out
  }

    laggedVariables <- vector("list")
    count = 1
    for (var in colnames(data)) {
      laggedVariables[[count]] <- lagVector(data[[var]], lags, var)
      count = count + 1
    }
    na.omit(data.frame(laggedVariables))

}

# IMPLEMENT THE HENDRY GETS METHOD

# Steps :
# - Fit an over-parametrized model
# Save the values of the R2, SEE and RSS at each step
# Do this iteratively
# - Remove the variable with the highest t-ratio starting with the furthest lags
# If two coefficients are close enough (threshold to be fixed), create a new variable called
# DX2_Y3 that is the difference of the two.


# Tests
df <- ggplot2::faithfuld[1:150,]
laggedDF <- staggerLags(df, 5)
mod <- lm(eruptions~., data = laggedDF)
summary(mod)
