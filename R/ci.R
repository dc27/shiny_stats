make_ci_table <- function(rep, popn, sample_size, ci_level) {
  # samples from population and calculates a conf int
  
  s_wpm <- sample(popn, sample_size, replace = FALSE)
  
  x <- mean(s_wpm)
  s <- sd(s_wpm)
  n <- length(s_wpm)
  
  width <- ci_level + (1-ci_level)/2
  p <- c(0, ci_level) + width
  
  # originally I used qnorm but too many of my cis were hitting the mean
  error <- qt(width ,df=n-1)*s/sqrt(n)
  ci <- c(x - error, x + error)
  
  data.frame(
    rep = rep,
    x = x,
    ci_low = ci[1],
    ci_high = ci[2]
  )
}