#' @title Genereates
#' @param  samples Specify how many samples
#'
#' @export





SamplingDist <- function(samples=50, sample_size=1000, mean = 0, sd = 1){


  sampleMeans <- rep(NA,samples)
  for (i in 1:samples){
    x <- rnorm(sample_size, mean = mean, sd = sd)
    sampleMeans[i] <- mean(x)
  }

  return(sampleMeans)
}

