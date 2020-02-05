#' This is how to use more than one core in R at a time. Not all processes
#' will be worth parellising as the CPU usage to communicate is greater than
#' the cost of just running things sequentially

library(parallel)
library(benchmarkme)

detectCores()
get_cpu()

# create data
m <- matrix(rnorm(10000), ncol = 10)

# use apply to calculate median for each row
system.time(res <- apply(m, 1, median))

# use n-1 cores for R so that you can still do other things too
copies_of_r <- detectCores() - 1
cl <- makeCluster(copies_of_r)

## parApply
system.time(res <- parApply(cl, m, 1, median)) # parallel apply function - needs additional argument for cluster
stopCluster(cl) # free up resources

## parSapply
pokemon <- read.csv("Dropbox/PhD2/Research Skills/Statistics/R/datacamp/pokemon.csv")

## normal single thread
bootstrap <- function(data_set){
  # sample with replacement
  s <- sample(1:nrow(data_set), replace = TRUE)
  new_data <- data_set[s,]
  
  # calculate the correlation
  cor(new_data$Attack, new_data$Defense)
}

system.time(sapply(1:100, function(i) bootstrap(pokemon)))

## multi thread
copies_of_r <- detectCores() - 1
cl <- makeCluster(copies_of_r)
clusterExport(cl, c("bootstrap", "pokemon")) # have to explicitly export functions as well

system.time(parSapply(cl, 1:100, function(i) bootstrap(pokemon)))
stopCluster(cl)
