# fig2a
size200_cov5 <- read.csv("C:/Users/lisit/Documents/Dartmouth/thesis/simulation/simulation9/linear_positive_size200.csv",
                  header = TRUE)[,-1]
size200_cov5 <- size200_cov5 - 1
size200_cov20 <- read.csv("C:/Users/lisit/Documents/Dartmouth/thesis/simulation/simulation9/linear_positive_size200_covariate20.csv",
                          header = TRUE)[,-1]
size200_cov20 <- size200_cov20 - 1
size1000_cov5 <- read.csv("C:/Users/lisit/Documents/Dartmouth/thesis/simulation/simulation9/linear_positive_size1000.csv",
                               header = TRUE)[,-1]
size1000_cov5 <- size1000_cov5 - 1
size1000_cov20 <- read.csv("C:/Users/lisit/Documents/Dartmouth/thesis/simulation/simulation9/linear_positive_size1000_covariate20.csv",
                                header = TRUE)[,-1]
size1000_cov20 <- size1000_cov20 - 1


DF <- data.frame(
  x = c(c(size200_cov5$amc, size200_cov5$gcomp, size200_cov5$wqs), 
        c(size200_cov20$amc, size200_cov20$gcomp, size200_cov20$wqs),
        c(size1000_cov5$amc, size1000_cov5$gcomp, size1000_cov5$wqs),
        c(size1000_cov20$amc, size1000_cov20$gcomp, size1000_cov20$wqs)),
  y = rep(c("1size200_cov5", "2size200_cov20", "3size1000_cov5", "4size1000_cov20"), each = 3000),
  z = rep(rep(c("amc", "gcomp", "wqs"), each=1000), 4),
  stringsAsFactors = FALSE
)


cols <- rainbow(3, s = 0.5)
boxplot(x ~ z + y, data = DF,
        at = c(1:3, 5.5:7.5, 10:12, 14.5:16.5), col = cols,
        names = c("n=200", "", "m=5", "n=200", "", "m=20", "n=1000", "", "m=5", "n=1000", "", "m=20"), xaxs = FALSE,
        ylab = "Bias", xlab = "")
legend("topright", fill = cols, legend = c("AMC + g-Computation", "Quatile g-Computation", "WQS"), horiz = F, cex = 0.9)
abline(h=0, col="brown", lty=2)


# fig2b
size200_cov5 <- read.csv("C:/Users/lisit/Documents/Dartmouth/thesis/simulation/simulation9/linear_negative_size200.csv",
                         header = TRUE)[,-1]
size200_cov5 <- size200_cov5 - 1
size200_cov20 <- read.csv("C:/Users/lisit/Documents/Dartmouth/thesis/simulation/simulation9/linear_negative_size200_covariate20.csv",
                          header = TRUE)[,-1]
size200_cov20 <- size200_cov20 - 1
size1000_cov5 <- read.csv("C:/Users/lisit/Documents/Dartmouth/thesis/simulation/simulation9/linear_negative_size1000.csv",
                          header = TRUE)[,-1]
size1000_cov5 <- size1000_cov5 - 1
size1000_cov20 <- read.csv("C:/Users/lisit/Documents/Dartmouth/thesis/simulation/simulation9/linear_negative_size1000_covariate20.csv",
                           header = TRUE)[,-1]
size1000_cov20 <- size1000_cov20 - 1


DF <- data.frame(
  x = c(c(size200_cov5$amc, size200_cov5$gcomp, size200_cov5$wqs), 
        c(size200_cov20$amc, size200_cov20$gcomp, size200_cov20$wqs),
        c(size1000_cov5$amc, size1000_cov5$gcomp, size1000_cov5$wqs),
        c(size1000_cov20$amc, size1000_cov20$gcomp, size1000_cov20$wqs)),
  y = rep(c("1size200_cov5", "2size200_cov20", "3size1000_cov5", "4size1000_cov20"), each = 3000),
  z = rep(rep(c("amc", "gcomp", "wqs"), each=1000), 4),
  stringsAsFactors = FALSE
)


cols <- rainbow(3, s = 0.5)
boxplot(x ~ z + y, data = DF,
        at = c(1:3, 5.5:7.5, 10:12, 14.5:16.5), col = cols,
        names = c("n=200", "", "m=5", "n=200", "", "m=20", "n=1000", "", "m=5", "n=1000", "", "m=20"), xaxs = FALSE,
        ylab = "Bias", xlab = "")
legend("topright", fill = cols, legend = c("AMC + g-Computation", "Quatile g-Computation", "WQS"), horiz = F, cex = 0.9)
abline(h=0, col="brown", lty=2)