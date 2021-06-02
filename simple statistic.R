score <- c(11,8,14,5,17,18,9,13,15,19,15,12,18,17,15,17,16,16,3,17)
range(score)
mean(score)
median(score)
quantile(score)

var(score)
sd(score)


#overview data 
summary(score)
IQR(score)

#mode 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(score)



# frequency distribution 
bins <- seq(2.5,22.5,by=5) 
score_table <- cut(score,bins)
transform(table(score_table))

#histogram
hist(score,
     breaks = bins,
     main = "histogram of  Test Score",
     xlab = "Test score",
     col="pink")

