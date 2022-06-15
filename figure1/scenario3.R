# simulation function
simu = function(size, nvariate, prob, mu, sigma, betay){
  # size is the sample size
  # nvariate is the number of exposures
  # prob is the probablity vector
  # mu is the mean
  # sigma is the variance
  # betay is the effect of exposure (coefficient)
  state = data.frame(matrix(nrow = size, ncol = nvariate))
  dd = data.frame(matrix(nrow = size, ncol = nvariate))
  contt = data.frame(matrix(nrow = size, ncol = nvariate))
  for (j in 1:nvariate) {
    sta = sample(x = c(1,2,3,4), size = size, replace = T, prob = prob)
    cont = sta
    for (i in 1:size) {
      cont[i] = ifelse(sta[i]==1,rnorm(1,mean = mu[1], sd=sigma[1]),
                     ifelse(sta[i]==2, rnorm(1, mean = mu[2], sd=sigma[2]), 
                            ifelse(sta[i]==3, rnorm(1, mean = mu[3], sd=sigma[3]), 
                                   rnorm(1, mean = mu[4], sd=sigma[4]))))
    }
    
    contt[,j] = cont
    state[,j] = sta
  }
  
  y=(betay[1]*state[,1] + betay[2]*state[,2])
  y = rnorm(size) + y
  
  state = cbind(y, state)
  contt = cbind(y, contt)
  return(data = list(state, contt))
}



# install.packages("devtools")
# devtools::install_github("sitingLi/amc")
library(amc)
library(qgcomp)
library(gWQS)
psi = data.frame(matrix(nrow = 1000, ncol = 3))
colnames(psi) <- c("amc", "gcomp", "wqs")

set.seed(1)
for (j in 1:1000){
  # generate data
  data = simu(size = 200, nvariate = 5, prob = c(0.4, 0.3, 0.2, 0.1), mu = c(1,2,3,4), 
           sigma = c(1/3, 1/3, 1/3, 1/3), betay = c(0.5, 0.5))
  input = data[[2]][,-1]
  
  # amc-based g-computation
  result.amc = amc.fixed.k(input, 4)
  output = result.amc$output
  oldseed <- .Random.seed
  output <- cbind(data[[2]][,1],output)
  colnames(output)[1] <- "y"
  result1 = qgcomp.noboot(y~., data = output, family = "gaussian", q = NULL)
  psi$amc[j] = result1[["psi"]]
  
  # quantile-based g-computation
  obs = data[[2]]
  result2 = qgcomp.noboot(y~., data = obs, family = "gaussian", q = 4)
  psi$gcomp[j] = result2[["psi"]]
  
  # wqs
  exposures = paste0("X", 1:5)
  fit <- try(
    wqsfit  <- suppressWarnings(gwqs(y ~ wqs, data = obs, mix_name = exposures, q = 4, family = "gaussian"))
  )
  
  if("try-error" %in% class(fit)){
    .Random.seed <- oldseed
    print("No positive b1 in the bootstrapped models -> Set b1_pos = FALSE")
    wqsfit <- suppressWarnings(gwqs(y ~ wqs, data=obs, mix_name=exposures, b1_pos=FALSE, q=4, family="gaussian"))
  }else{
    wqsfit <- wqsfit
  }
  psi$wqs[j] = wqsfit[["fit"]][["coefficients"]][2]
}

write.csv(psi, "scenario3.csv")


