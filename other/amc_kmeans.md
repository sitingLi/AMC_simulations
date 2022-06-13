
``` r
input <- data.frame(matrix(ncol = 10, nrow = 1000))
ground_truth <- data.frame(matrix(ncol = 10, nrow = 1000))
set.seed(123)
for(j in 1:10){
  a0 = rnorm(100, mean = 3, sd=1)
  a1 = rnorm(400, mean = 6, sd=1)
  a2 = rnorm(400, mean = 9, sd=1)
  a3 = rnorm(100, mean = 12, sd=1)
  input[,j] = c(a0, a1, a2, a3)
  b0 = rep(1,100)
  b1 = rep(2,400)
  b2 = rep(3,400)
  b3 = rep(4,100)
  ground_truth[,j] = c(b0, b1, b2, b3)
}

library(amc)
result = amc(input)
output = result$output

predicted_value <- factor(c(t(output)))
expected_value <- factor(c(t(ground_truth)))
confusionMatrix(data = predicted_value, reference = expected_value)

library(factoextra)

input2 = scale(input)                        

res = get_clust_tendency(input2, 50)       
res$hopkins_stat                          
fviz_nbclust(input2, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2) 

km_data <- kmeans(input2, 4, nstart = 16)  



library(dplyr)

output_sim = matrix(NA,1000,10)
order = rep(1:1000)                       


for (i in 1:10)                           
{
  km_data_temp <- kmeans(input2[,i], 4, nstart = 1)
  
  temp = cbind(order, input2[,i], km_data_temp$cluster, rep(NA))  
  temp = data.frame(temp)
  temp = arrange(temp, V2)               
  s=1
  temp[1, 4] = s
  
  for(j in 2:1000)
  {
    
    if(temp[j,3]==temp[j-1,3])
    {
      temp[j,4]=s
    }
    else                                 
    {
      s=s+1
      temp[j,4]=s
    }
  }
  
  temp = arrange(temp, order)             
  
  
  output_sim[,i] = temp[,4]               
}

predicted_value2 <- factor(c(t(output_sim)))
confusionMatrix(data = predicted_value2, reference = expected_value)

```
