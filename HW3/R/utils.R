# utils.R
#
# Common utility functions.

## ---- def-printf ----

# Formatted print function.
printf <- function(format, ...) {
  print(sprintf(format, ...))
}

## ---- def-deparse ----

# Get the name of the argument
# passed into a function.
varname <- function(v1) {
  deparse(substitute(v1))
}

## ---- def-showclass ----

class.format <- function(.obj) {
  return(paste(class(.obj), collapse="/"))
}

## ---- def-clusterfy ----

# Setup parallel clusters.
start.cluster <- function(n.cores = 2, type = "PSOCK") {
  cluster.inst <- parallel::makeCluster(n.cores, type = type)
  printf("Created cluster %s.", cluster.inst)
  doParallel::registerDoParallel(cl = cluster.inst)
  printf("Are parallel clusters registered? %s",
         foreach::getDoParRegistered())
  printf("How many workers are available? %s",
         foreach::getDoParWorkers())
  return(cluster.inst)
}

# Stop a cluster
end.cluster <- function(cluster_inst) {
  printf("Stopping cluster %s.", cluster_inst)
  parallel::stopCluster(cl = cluster_inst)
  printf("Cluster ended.")
}
