# misc cluster functions 
library(stats)
library(cluster)
library(fpc)
library(dbscan)

kmeans_reps <- function(data, centers, reps){
  w_ss = Inf
  for(i in 1:reps){
    k_cluster <- kmeans(x = data, centers = centers)
    if((k_cluster$tot.withinss) < w_ss){
      ssw = k_cluster$tot.withinss
      my_k_cluster <- k_cluster
    }
  }
  return(my_k_cluster)
}

min_rep <- function(K, eps){
  ceiling(log(eps) / log(1 - factorial(K)/K^K))
}


mysil <- function(x, dmat){
  return(mean(silhouette(x = x, dmat = dmat)[,3]))
}

find_sil <- function(data, kmax, niter, eps){
  dmat <- rdist(data)
  sil_v <- 1:kmax
  for(K in 2:kmax){
  	iter <- min(niter, min_rep(K, eps))
  	kmeans_tmp <- kmeans_reps(data, K, iter)
  	sil_v[K] <- mysil(kmeans_tmp$cluster, dmat)
  }
  sil_v <- sil_v[2:kmax]
  plot(2:kmax, sil_v, xlab='K', ylab='Silhouette Coefficient')
  return(list(max = max(sil_v), where = which.max(sil_v) + 1))
}

plot_ssw <- function(data, kmax, niter, eps){
  ssw_v <- 1:kmax
  
  for(K in 1:kmax){
    iter <- min(niter, min_rep(K, eps))
    kmeans_tmp <- kmeans_reps(data, K, iter)
    ssw_v[K] <- kmeans_tmp$tot.withinss
  }
  plot(1:kmax, ssw_v, xlab='K',ylab='SSW')
}
