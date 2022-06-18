simu = function(size, nvariate, prob, mu, sigma, betay){
  # size is the sample size
  # nvariate is the number of exposures
  # prob is the probability vector
  # mu is the mean
  # sigma is the variance
  # betay is the effect of exposure (coefficient)
  dd = data.frame(matrix(nrow = size, ncol = nvariate))
  state = data.frame(matrix(nrow = size, ncol = nvariate))
  contt = data.frame(matrix(nrow = size, ncol = nvariate))
  for (j in 1:nvariate) {
    sta = sample(x = c(1, 2, 3, 4), size = size, replace = T, prob = prob)
    cont = sta
    for (i in 1:size) {
      cont[i]=ifelse(sta[i]==1,rnorm(1, mean = mu[1], sd = sigma[1]),
                     ifelse(sta[i]==2, rnorm(1, mean = mu[2], sd = sigma[2]), 
                            ifelse(sta[i]==3, rnorm(1, mean = mu[3], sd = sigma[3]), 
                                   rnorm(1, mean = mu[4], sd = sigma[4]))))
    }
    
    contt[,j] = cont
    state[,j] = sta
  }
  
  y=(betay[1]*state[,1]+betay[2]*state[,2]+betay[3]*state[,3]+betay[4]*state[,4]+betay[5]*state[,5]
     +betay[6]*state[,6]+betay[7]*state[,7]-betay[8]*state[,8]-betay[9]*state[,9]-betay[10]*state[,10])
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

set.seed(878)
simu_times = 1000 # simulate 1000 times


weights_result1 = weights_result2 = matrix(NA, 30, simu_times)
wqs_weights = matrix(NA, 30, simu_times)

for (j in 1:simu_times){
  print(paste("Number of the currently executing iteration:", j))
  # generate data
  data = simu(size = 1500, nvariate = 30, prob = c(0.4, 0.3, 0.2, 0.1), mu=c(1, 2, 3, 4), 
           sigma = c(1/3, 1/3, 1/3, 1/3), betay = rep(0.1, 10))
  
  input = data[[2]][, -1]
  
  # amc-based g-computation
  result.amc = amc.fixed.k(input, 4)
  output = result.amc$output
  oldseed <- .Random.seed
  output <- cbind(data[[2]][, 1],output)
  colnames(output)[1] <- "y"
  result1 = qgcomp.noboot(y~., data = output, family = "gaussian", q = NULL)
  
  # quantile-based g-computation
  obs=data[[2]]
  result2 = qgcomp.noboot(y~., data = obs, family = "gaussian", q = 4)
  
  # wqs
  exposures = paste0("X", 1:30)
  fit <- try(
    wqsfit  <- suppressWarnings(gwqs(y ~ wqs, data = obs, mix_name = exposures, q = 4, family = "gaussian", seed = oldseed))
  )
  if("try-error" %in% class(fit)){
    print("No positive b1 in the bootstrapped models -> Set b1_pos = FALSE")
    wqsfit <- suppressWarnings(gwqs(y ~ wqs, data = obs, mix_name = exposures, b1_pos = FALSE, q = 4, family = "gaussian", seed = oldseed))
  }else{
    wqsfit <- wqsfit
  }
  .Random.seed <- oldseed
  
  # save weights of exposures from amc-based g-computation model
  pos_weight = as.data.frame(result1[["pos.weights"]])
  colnames(pos_weight) = "N"
  neg_weight = as.data.frame(-result1[["neg.weights"]])
  colnames(neg_weight)  = "N"
  temp = rbind(pos_weight, neg_weight)
  weights_result1[,j] = unlist(temp[exposures,])
  
  # save weights of exposures from quantile-based g-computation model
  pos_weight = as.data.frame(result2[["pos.weights"]])
  colnames(pos_weight) = "N"
  neg_weight = as.data.frame(-result2[["neg.weights"]])
  colnames(neg_weight)  = "N"
  temp = rbind(pos_weight, neg_weight)
  weights_result2[,j] = unlist(temp[exposures,])
  
  # save weights of exposures from wqs model
  name = as.character(wqsfit[["final_weights"]][["mix_name"]])
  temp = as.data.frame(wqsfit[["final_weights"]][["mean_weight"]])
  rownames(temp) = name
  if("try-error" %in% class(fit)){
    wqs_weights[,j] = -temp[exposures,]
  }else{
    wqs_weights[,j] = temp[exposures,]
  }
  
}

# write result to files
rownames(weights_result1) = exposures
rownames(weights_result2) = exposures
rownames(wqs_weights)  = exposures

write.table(weights_result1, file = "weights_amc_gcomp.csv",
            sep=",", quote = FALSE, row.names=TRUE, col.names=FALSE)

write.table(weights_result2, file = "weights_quantile_gcomp.csv",
            sep=",", quote = FALSE, row.names=TRUE, col.names=FALSE)

write.table(wqs_weights, file = "weights_wqs.csv",
            sep=",", quote = FALSE, row.names=TRUE, col.names=FALSE)
