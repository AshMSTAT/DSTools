#' @title Sampling Distribution Function for Fun
#'
#' @param  Samples Specify how many samples that you will do
#' @param  sample_size  Specifies the size of the sample you are taking
#' @param  mean of the normal function for generating the individual random points
#' @param  sd the standard deviation of the normal function for generating the individual random points.
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


generator = function(n, pop.mean=0, pop.sd=1, conf.lvl=.95) {

  plot(NULL
       ,xlim = c(pop.mean-pop.sd/2,pop.mean+pop.sd/2)
       ,ylim = c(0,100)
       ,yaxt = 'n'
       ,xlab = (conf.lvl)
       ,ylab = (n)
       #        ,main = "Confidence Intervals of 100 Samples"
  )

  abline(v = pop.mean, col = 'black')
  mtext(expression(mu), cex = 2, at = pop.mean)
  type2.error = 0
  for (i in 1:100){

    x <- rnorm(n, mean = pop.mean, sd = pop.sd)
    test <- t.test(x,conf.level=conf.lvl)
    interval <- test$conf.int

    if(pop.mean>interval[1] & pop.mean<interval[2]){
      lines(c(interval[1],interval[2]),c(i,i), lwd=2,col='black')
    }
    else{
      lines(c(interval[1],interval[2]),c(i,i), lwd=2,col='red' )
      type2.error <- type2.error + 1
    }
   }
  return(type2.error)
}

