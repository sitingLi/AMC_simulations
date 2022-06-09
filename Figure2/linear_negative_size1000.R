sim=function(size,nvariate,prob, mu, sigma, betay){
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
  
  y=(betay[1]*state[,1]+betay[2]*state[,2])
  y = rnorm(size) + y
  #y <- y-mean(y)
  
  state=cbind(y,state)
  contt=cbind(y,contt)
  return(data=list(state,contt))
}



###only linear term###
library(qgcomp)
library(gWQS)
psi = data.frame(matrix(nrow = 1000, ncol = 3))
colnames(psi) <- c("amc","gcomp","wqs")

set.seed(1)
for (j in 1:1000){
  data=sim(size = 1000, nvariate = 5,prob = c(0.4, 0.3, 0.2, 0.1), mu=c(1,2,3,4), 
           sigma = c(1/3,1/3,1/3,1/3), betay = c(1.5, -0.5))
  
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
  
  output <- cbind(data[[2]][,1],output)
  colnames(output)[1] <- "y"
  result1 = qgcomp.noboot(y~., data=output, family="gaussian", q = NULL)
  psi$amc[j]=result1[["psi"]]
  
  obs=data[[2]]
  result2 = qgcomp.noboot(y~., data=obs, family="gaussian", q = 4)
  psi$gcomp[j]=result2[["psi"]]
  
  exposures = paste0("X",1:5)
  wqsfit  <- suppressWarnings(gwqs(y ~ wqs, mix_name=exposures, q=4, data = obs, family = "gaussian"))
  psi$wqs[j]=wqsfit[["fit"]][["coefficients"]][2]
}

write.csv(psi,"linear_negative_size1000.csv")


