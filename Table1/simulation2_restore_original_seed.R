setwd("/dartfs-hpc/rc/lab/J/JGUI/simulation/change_seeds/tpr_1000_times_seed878/tpr_nonlinear_size1500_covariate50/")

sim=function(size, nvariate, prob, mu, sigma, betay){
  ##prob is the probablity vector
  ##nvariate is the number of chemical
  ##mu is the mean value
  ##sigma is the variance
  ##threshold is the quantitle for  zero-inflation value 
  dd=data.frame(matrix(nrow = size, ncol = nvariate))
  state=data.frame(matrix(nrow = size, ncol = nvariate))
  contt=data.frame(matrix(nrow = size, ncol = nvariate))
  for (j in 1:nvariate) {
    sta=sample(x=c(1,2,3,4), size = size, replace = T, prob = prob)
    cont=sta
    for (i in 1:size) {
      cont[i]=ifelse(sta[i]==1,rnorm(1,mean = mu[1], sd=sigma[1]),
                     ifelse(sta[i]==2, rnorm(1, mean = mu[2], sd=sigma[2]), 
                            ifelse(sta[i]==3, rnorm(1, mean = mu[3], sd=sigma[3]), 
                                   rnorm(1, mean = mu[4], sd=sigma[4]))))
    }
    
    contt[,j]=cont
    state[,j]=sta
  }
  
  y=(betay[1]*state[,1]+betay[2]*state[,2]+betay[3]*state[,3]+betay[4]*state[,4]+betay[5]*state[,5]
     +betay[6]*state[,6]+betay[7]*state[,7]-betay[8]*state[,8]-betay[9]*state[,9]-betay[10]*state[,10])
  y = rnorm(size) + y
  
  state=cbind(y,state)
  contt=cbind(y,contt)
  return(data=list(state,contt))
}



###only linear term###
library(qgcomp)
library(gWQS)


set.seed(878)

n = 1000

psi_result1 = psi_result2 = rep(NA, n)
pos.psi_result1 = pos.psi_result2 = rep(NA, n)
neg.psi_result1 = neg.psi_result2 = rep(NA, n)
weights_result1 = weights_result2 = matrix(NA, 50, n)

wqs = rep(NA, n)
wqs_weights = matrix(NA, 50, n)


for (j in 1:n){
  print(paste("Number of the currently executing iteration:", j))
  data=sim(size = 1500, nvariate = 50, prob = c(0.4, 0.3, 0.2, 0.1), mu=c(1,2,3,4), 
           sigma = c(1/3,1/3,1/3,1/3), betay = rep(0.1,10))
  
  a=data[[2]][,-1]
  output=a
  num_sub=dim(output)[1]
  num_var=dim(output)[2]
  # temp=12
  mat_position=matrix(0,3,num_var)
  mat_threshold=matrix(0,3,num_var)
  
  for (temp in 1:num_var)
  {
    data_temp=sort(a[,temp])
    data_temp = data.frame(data_temp)
    names(data_temp)[1] = 'value'
    
    # k=2
    F_value=rep(0, num_sub-1)
    for (i in 1:(num_sub-1))
    {
      threshold1 = data_temp$value[i]
      data_temp[,'value.level'] <- cut(data_temp$value,breaks=c(-Inf,threshold1,Inf),labels=c('0','1'))
      one.way <- aov(value ~ value.level, data = data_temp)
      # summary(one.way)
      F_value[i]=summary(one.way)[[1]][["F value"]][1]
    }
    position1 = which.max(F_value) 
    threshold1 = data_temp$value[position1]
    mat_position[1,temp]=position1
    mat_threshold[1,temp]=threshold1
    
    # k=3
    F_value=rep(0, num_sub-2)
    for (i in 1:(num_sub-2))
    {
      threshold2 = data_temp$value[i]
      if(threshold2 == threshold1){
        F_value[i]=0
      } 
      else {
        threshold = sort(c(threshold1, threshold2))
        data_temp[,'value.level'] <- cut(data_temp$value,breaks=c(-Inf,threshold,Inf),labels=c('0','1','2'))
        one.way <- aov(value ~ value.level, data = data_temp)
        # summary(one.way)
        F_value[i]=summary(one.way)[[1]][["F value"]][1]
      }
    }
    position2 = which.max(F_value) 
    threshold2 = data_temp$value[position2]
    mat_position[2,temp]=position2
    mat_threshold[2,temp]=threshold2
    
    # k=4
    F_value=rep(0, num_sub-3)
    for (i in 1:(num_sub-3))
    {
      threshold3 = data_temp$value[i]
      if((threshold3 == threshold1) | (threshold3 == threshold2)){
        F_value[i]=0
      } 
      else {
        threshold = sort(c(threshold1, threshold2, threshold3))
        data_temp[,'value.level'] <- cut(data_temp$value,breaks=c(-Inf,threshold,Inf),labels=c('0','1','2','3'))
        one.way <- aov(value ~ value.level, data = data_temp)
        # summary(one.way)
        F_value[i]=summary(one.way)[[1]][["F value"]][1]
      }
    }
    position3 = which.max(F_value) 
    threshold3 = data_temp$value[position3]
    mat_position[3,temp]=position3
    mat_threshold[3,temp]=threshold3
    
    output[,temp] <- cut(a[,temp],breaks=c(-Inf,sort(c(threshold1, threshold2, threshold3)),Inf),labels=FALSE)
  }
  
  oldseed <- .Random.seed
  output <- cbind(data[[2]][,1],output)
  colnames(output)[1] <- "y"
  result1 = qgcomp.noboot(y~., data=output, family="gaussian", q = NULL)
  
  obs=data[[2]]
  result2 = qgcomp.noboot(y~., data=obs, family="gaussian", q = 4)
  
  exposures = paste0("X",1:50)
  fit <- try(
    wqsfit  <- suppressWarnings(gwqs(y ~ wqs, data = obs, mix_name=exposures, q=4, family = "gaussian", seed = oldseed))
  )
  if("try-error" %in% class(fit)){
    print("No positive b1 in the bootstrapped models -> Set b1_pos = FALSE")
    wqsfit <- suppressWarnings(gwqs(y ~ wqs, data=obs, mix_name=exposures, b1_pos=FALSE, q=4, family="gaussian", seed = oldseed))
  }else{
    wqsfit <- wqsfit
  }
  .Random.seed <- oldseed
  
  psi_result1[j] = result1[["psi"]][["psi1"]]
  psi_result2[j] = result2[["psi"]][["psi1"]]
  
  pos.psi_result1[j] = result1[["pos.psi"]]
  pos.psi_result2[j] = result2[["pos.psi"]]
  
  neg.psi_result1[j] = result1[["neg.psi"]]
  neg.psi_result2[j] = result2[["neg.psi"]]
  
  pos_weight = as.data.frame(result1[["pos.weights"]])
  colnames(pos_weight) = "N"
  neg_weight = as.data.frame(-result1[["neg.weights"]])
  colnames(neg_weight)  = "N"
  temp = rbind(pos_weight, neg_weight)
  weights_result1[,j] = unlist(temp[exposures,])
  
  pos_weight = as.data.frame(result2[["pos.weights"]])
  colnames(pos_weight) = "N"
  neg_weight = as.data.frame(-result2[["neg.weights"]])
  colnames(neg_weight)  = "N"
  temp = rbind(pos_weight, neg_weight)
  weights_result2[,j] = unlist(temp[exposures,])
  
  if("try-error" %in% class(fit)){
    wqs[j] = -wqsfit[["fit"]][["coefficients"]][["wqs"]]
  }else{
    wqs[j] = wqsfit[["fit"]][["coefficients"]][["wqs"]]
  }
  
  name = as.character(wqsfit[["final_weights"]][["mix_name"]])
  temp = as.data.frame(wqsfit[["final_weights"]][["mean_weight"]])
  rownames(temp) = name
  
  if("try-error" %in% class(fit)){
    wqs_weights[,j] = -temp[exposures,]
  }else{
    wqs_weights[,j] = temp[exposures,]
  }
  
}


write.table(psi_result1, file = "psi_result1_restore_original_seed.csv",
            sep=",", quote = FALSE, row.names=FALSE, col.names=TRUE)

write.table(psi_result2, file = "psi_result2_restore_original_seed.csv",
            sep=",", quote = FALSE, row.names=FALSE, col.names=TRUE)

write.table(pos.psi_result1, file = "pos.psi_result1_restore_original_seed.csv",
            sep=",", quote = FALSE, row.names=FALSE, col.names=TRUE)

write.table(pos.psi_result2, file = "pos.psi_result2_restore_original_seed.csv",
            sep=",", quote = FALSE, row.names=FALSE, col.names=TRUE)

write.table(neg.psi_result1, file = "neg.psi_result1_restore_original_seed.csv",
            sep=",", quote = FALSE, row.names=FALSE, col.names=TRUE)

write.table(neg.psi_result2, file = "neg.psi_result2_restore_original_seed.csv",
            sep=",", quote = FALSE, row.names=FALSE, col.names=TRUE)

write.table(wqs, file = "wqs_restore_original_seed.csv",
            sep=",", quote = FALSE, row.names=FALSE, col.names=TRUE)


rownames(weights_result1) = exposures
rownames(weights_result2) = exposures
rownames(wqs_weights)  = exposures

write.table(weights_result1, file = "weights_result1_restore_original_seed.csv",
            sep=",", quote = FALSE, row.names=TRUE, col.names=FALSE)

write.table(weights_result2, file = "weights_result2_restore_original_seed.csv",
            sep=",", quote = FALSE, row.names=TRUE, col.names=FALSE)

write.table(wqs_weights, file = "wqs_weights_restore_original_seed.csv",
            sep=",", quote = FALSE, row.names=TRUE, col.names=FALSE)

