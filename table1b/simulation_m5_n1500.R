simu = function(size, nvariate, prob, mu, sigma, betay){
  # size is the sample size
  # nvariate is the number of exposures
  # prob is the probability vector
  # mu is the mean
  # sigma is the variance
  # betay is the effect of exposure (coefficient)
  dd=data.frame(matrix(nrow = size, ncol = nvariate))
  state=data.frame(matrix(nrow = size, ncol = nvariate))
  contt=data.frame(matrix(nrow = size, ncol = nvariate))
  for (j in 1:nvariate) {
    sta = sample(x = c(1, 2, 3, 4), size = size, replace = T, prob = prob)
    cont = sta
    for (i in 1:size) {
      cont[i]=ifelse(sta[i]==1, rnorm(1, mean = mu[1], sd = sigma[1]),
                     ifelse(sta[i]==2, rnorm(1, mean = mu[2], sd = sigma[2]), 
                            ifelse(sta[i]==3, rnorm(1, mean = mu[3], sd = sigma[3]), 
                                   rnorm(1, mean = mu[4], sd = sigma[4]))))
    }
    
    contt[,j] = cont
    state[,j] = sta
  }
  
  state_factor <- lapply(state, factor)
  state_factor<- data.frame(state_factor)
  
  dummy1 <- as.factor(state_factor[, 1]) # dummy variables
  dummy1 <- as.data.frame(model.matrix(~ dummy1 - 1))
  dummy1 <- as.matrix(dummy1)
  dummy2 <- as.factor(state_factor[, 2])
  dummy2 <- as.data.frame(model.matrix(~ dummy2 - 1))
  dummy2 <- as.matrix(dummy2)
  dummy3 <- as.factor(state_factor[, 3])
  dummy3 <- as.data.frame(model.matrix(~ dummy3 - 1))
  dummy3 <- as.matrix(dummy3)
  dummy4 <- as.factor(state_factor[, 4])
  dummy4 <- as.data.frame(model.matrix(~ dummy4 - 1))
  dummy4 <- as.matrix(dummy4)
  dummy5 <- as.factor(state_factor[, 5])
  dummy5 <- as.data.frame(model.matrix(~ dummy5 - 1))
  dummy5 <- as.matrix(dummy5)
  
  
  y <- 1*dummy1%*%betay+1*dummy2%*%betay+1*dummy3%*%betay+1*dummy4%*%betay+1*dummy5%*%betay
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
# install.packages("fastDummies")
library(fastDummies)

set.seed(878)
simu_times = 1000 # simulate 1000 times

weights_result1 = weights_result2 = matrix(NA, 15, simu_times)
wqs_weights = matrix(NA, 15, simu_times)


for (j in 1:simu_times){
  print(paste("Number of the currently executing iteration:", j))
  # generate data
  data=simu(size = 1500, nvariate = 5, prob = c(0.4, 0.3, 0.2, 0.1), mu=c(1, 2, 3, 4), 
           sigma = c(1/3, 1/3, 1/3, 1/3), betay = c(0, 1, -1, 0))
  
  input = data[[2]][, -1]
  
  # amc-based g-computation
  result.amc = amc.fixed.k(input, 4)
  output = result.amc$output
  output <- data.frame(lapply(output, factor))
  oldseed <- .Random.seed
  dummy_obs <- dummy_cols(output, remove_first_dummy = TRUE)
  newdata <- dummy_obs[,6:20] # X1-X5 (categorized by amc) are transformed to dummy variables
  newdata <- cbind(data[[2]][, 1], newdata)
  colnames(newdata)[1] <- "y"
  result1 = qgcomp.noboot(y~., data = newdata, family = "gaussian", q = NULL)
  
  # quantile-based g-computation
  contt <- data[[2]][-1]
  quantile_contt <- contt
  for (i in 1:5){
    temp <- contt[, i]
    thres <- quantile(temp) # quantiles
    thres <- c(-Inf, thres[-1])
    quantile_contt[, i] <- cut(contt[, i], breaks = thres, labels = c('1', '2', '3', '4'))
  }
  dummy_obs <- dummy_cols(quantile_contt, remove_first_dummy = TRUE)
  obs <- dummy_obs[, 6:20]  # X1-X5 (categorized into quantiles) are transformed to dummy variables
  obs <- cbind(data[[2]][, 1], obs)
  colnames(obs)[1] <- "y"
  result2 = qgcomp.noboot(y~., data = obs, family = "gaussian", q = NULL)
  
  # wqs
  exposures = colnames(obs)[-1]
  fit <- try(
    wqsfit  <- suppressWarnings(gwqs(y ~ wqs, data = obs, mix_name = exposures, q = NULL, family = "gaussian", seed = oldseed))
  )
  if("try-error" %in% class(fit)){
    print("No positive b1 in the bootstrapped models -> Set b1_pos = FALSE")
    wqsfit <- suppressWarnings(gwqs(y ~ wqs, data = obs, mix_name = exposures, b1_pos = FALSE, q = NULL, family = "gaussian", seed = oldseed))
  }else{
    wqsfit <- wqsfit
  }
  .Random.seed <- oldseed
  

  # save weights of dummy variables from amc-based g-computation model
  pos_weight = as.data.frame(result1[["pos.weights"]])
  colnames(pos_weight) = "N"
  neg_weight = as.data.frame(-result1[["neg.weights"]])
  colnames(neg_weight)  = "N"
  temp = rbind(pos_weight, neg_weight)
  weights_result1[,j] = unlist(temp[exposures,])
  
  # save weights of dummy variables from quantile-based g-computation model
  pos_weight = as.data.frame(result2[["pos.weights"]])
  colnames(pos_weight) = "N"
  neg_weight = as.data.frame(-result2[["neg.weights"]])
  colnames(neg_weight)  = "N"
  temp = rbind(pos_weight, neg_weight)
  weights_result2[,j] = unlist(temp[exposures,])
  
  # save weights of dummy variables from wqs model
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

