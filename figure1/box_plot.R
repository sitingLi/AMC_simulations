# figure 1
scenario1 <- read.csv("scenario1.csv",
                         header = TRUE)[,-1]
scenario1 <- scenario1 - 1 # the ground truth of joint mixture effect is 1
scenario2 <- read.csv("scenario2.csv",
                          header = TRUE)[,-1]
scenario2 <- scenario2 - 1
scenario3 <- read.csv("scenario3.csv",
                          header = TRUE)[,-1]
scenario3 <- scenario3 - 1
scenario4 <- read.csv("scenario4.csv",
                           header = TRUE)[,-1]
scenario4 <- scenario4 - 1


DF <- data.frame(
  x = c(c(scenario1$amc, scenario1$gcomp, scenario1$wqs), 
        c(scenario2$amc, scenario2$gcomp, scenario2$wqs),
        c(scenario3$amc, scenario3$gcomp, scenario3$wqs),
        c(scenario4$amc, scenario4$gcomp, scenario4$wqs)),
  y = rep(c("scenario1", "scenario2", "scenario3", "scenario4"), each = 3000),
  z = rep(rep(c("amc", "gcomp", "wqs"), each=1000), 4),
  stringsAsFactors = FALSE
)

par(mar = c(4.5, 4.5, 1, 1))
cols <- rainbow(3, s = 0.5)
boxplot(x ~ z + y, data = DF,
        at = c(1:3, 5.5:7.5, 10:12, 14.5:16.5), col = cols, xaxt = "n",
        ylab = "Estimate Bias",  ylim = c(-1, 1.4), xlab = "Simulation Scenarios")
axis(side = 1, at = c(2,6.5, 11, 15.5), labels = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4"))
legend("topleft", fill = cols, legend = c("AMC-based g-computation", "Quantile-based g-computation", "WQS"), horiz = F, cex = 0.9)
abline(h=0, col="brown", lty=2)
