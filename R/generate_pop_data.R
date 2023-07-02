set.seed(2015)

# population of Scotland who type regularly
population <- rnorm(4000000, mean = 0, sd = 10)

# can't have negative typing speeds so shift everything up
population <- population + abs(floor(min(population)))

population <- floor(population)

pop_wpm <- population