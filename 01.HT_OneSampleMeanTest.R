setwd("/Times/Statistics/Testing")
########################################################################
########################################################################
#   One Sample Hypothesis Testing for Sample Mean with Unknown Variance
########################################################################
########################################################################
# -------------------- Two Sided Test ----------------------------------
# Testing Mean (Small Sample) 
# SampleData=c(6.8,9.9,8.9,11.4,14.2,5.6,8.5,8.5,8.4,7.5,9.3,9.4,16.6,9.1,10.1,10.6)
# write.csv(matrix(SampleData,nrow=length(SampleData),byrow=FALSE),"SampleSalarySmallData.csv")
# ----------------------------------------------------------------------
myData=read.csv("OfficeSalarySmallData.csv")
monthlySalary=myData[,2]    # Sample
n=length(monthlySalary)     # Sample Size
muhat=mean(monthlySalary)   # Sample Mean
shat=sd(monthlySalary)      # Sample Standard Deviation
munull=10                   # Null H0: mu=10, Alternate Ha: mu!=10
testStatistics=(muhat-munull)/(shat/sqrt(n))
alpha=.05                               # Level of Significance
lcv=qt(alpha/2,n-1)                     # Lower Critical Value (t-dist.)
ucv=qt(alpha/2,n-1,lower.tail = FALSE)  # Upper Critical Value (t-dist.)
# The test
if(testStatistics<lcv || testStatistics>ucv){
  print("Null Hypothesis is rejected")  # RR = Below alpha/2 quantile or above alpha/2 quantile of t
}else{
  print("Null Hypothesis is accepted")
}
# The alternate method (p value)
pValue=2*pt(abs(testStatistics),n-1,lower.tail = FALSE)
if(pValue<alpha){
  print("Null Hypothesis is rejected")
}else{
  print("Null Hypothesis is accepted")
}
########################################################################
# -------------------- Two Sided Test ----------------------------------
# Testing Mean (Large Sample)
# ----------------------------------------------------------------------
myData=read.csv("OfficeSalaryData.csv")
monthlySalary=myData[,2]    # Sample
n=length(monthlySalary)     # Sample Size
muhat=mean(monthlySalary)   # Sample Mean
shat=sd(monthlySalary)      # Sample Standard Deviation
munull=10                   # Null H0: mu=10, Alternate Ha: mu!=10
testStatistics=(muhat-munull)/(shat/sqrt(n))
alpha=.05                               # Level of Significance
lcv=qt(alpha/2,n-1)                     # Lower Critical Value (t-dist.)
ucv=qt(alpha/2,n-1,lower.tail = FALSE)  # Upper Critical Value (t-dist.)
if(testStatistics<lcv || testStatistics>ucv){
  print("Null Hypothesis is rejected")  # RR = Below alpha/2 quantile or above (1-alpha/2) quantile of t
}else{
  print("Null Hypothesis is accepted")
}
# ----- Almost Same Results, if we use normal distribution -------------
lcv=qnorm(alpha/2)                      # Lower Critical Value (normal-dist.)
ucv=qnorm(alpha/2,lower.tail = FALSE)   # Upper Critical Value (normal-dist.)
if(testStatistics<lcv || testStatistics>ucv){
  print("Null Hypothesis is rejected")  # RR = Below alpha/2 quantile or above (1-alpha/2) quantile of normal
}else{
  print("Null Hypothesis is accepted")
}
########################################################################
# -------------------- One Sided Test ----------------------------------
# Testing Mean (Small Sample)
# SampleData=c(6.8,9.9,8.9,11.4,14.2,5.6,8.5,8.5,8.4,7.5,9.3,9.4,16.6,9.1,10.1,10.6)
# write.csv(matrix(SampleData,nrow=length(SampleData),byrow=FALSE),"SampleSalarySmallData.csv")
# ----------------------------------------------------------------------
myData=read.csv("OfficeSalarySmallData.csv")
monthlySalary=myData[,2]    # Sample
n=length(monthlySalary)     # Sample Size
muhat=mean(monthlySalary)   # Sample Mean
shat=sd(monthlySalary)      # Sample Standard Deviation
munull=10                   # Null H0: mu=10, Alternate Ha: mu<10
testStatistics=(muhat-munull)/(shat/sqrt(n))
alpha=.05                               # Level of Significance
lcv=qt(alpha,n-1)                       # Lower Critical Value (t-dist.)
if(testStatistics<lcv){
  print("Null Hypothesis is rejected")  # RR = Below alpha quantile of t
}else{
  print("Null Hypothesis is accepted")
}
# The alternate method (p value)
pValue=pt(abs(testStatistics),n-1,lower.tail = FALSE)
if(pValue<alpha){
  print("Null Hypothesis is rejected")
}else{
  print("Null Hypothesis is accepted")
}
########################################################################
# -------------------- One Sided Test ----------------------------------
# Testing Mean (Large Sample)
# SampleData=c(6.8,9.9,8.9,11.4,14.2,5.6,8.5,8.5,8.4,7.5,9.3,9.4,16.6,9.1,10.1,10.6,11.1,6.4,13.3,12.8,13.7,17.9,21.8,18.4,34.3,9.6,9.0,11.7,12.8,9.9,14.3,14.0,15.5,9.4,13.7,11.5,12.0,11.5,11.8,16.9,18.0,7.8,7.1,10.6,11.1,12.3,12.3,13.9,12.9)
# write.csv(matrix(SampleData,nrow=length(SampleData),byrow=FALSE),"SampleSalaryData.csv")
# ----------------------------------------------------------------------
myData=read.csv("OfficeSalaryData.csv")
monthlySalary=myData[,2]    # Sample
n=length(monthlySalary)     # Sample Size
muhat=mean(monthlySalary)   # Sample Mean
shat=sd(monthlySalary)      # Sample Standard Deviation
munull=10                   # Null H0: mu=10, Alternate Ha: mu<10
testStatistics=(muhat-munull)/(shat/sqrt(n))
alpha=.05                               # Level of Significance
lcv=qt(alpha,n-1)                       # Lower Critical Value
if(testStatistics<lcv){
  print("Null Hypothesis is rejected")  # RR = Below alpha quantile t
}else{
  print("Null Hypothesis is accepted")
}
# ----- Almost Same Results, if we use normal distribution -------------
lcv=qnorm(alpha)                      # Lower Critical Value
if(testStatistics<lcv){
  print("Null Hypothesis is rejected")  # RR = Below alpha quantile normal
}else{
  print("Null Hypothesis is accepted")
}