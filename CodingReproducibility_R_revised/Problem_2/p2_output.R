library(MASS)

#Plotting sum of the elements of random vectors

plot.ts(Process(0.9,mean,Cov,p,n), ylab = "Sum of process components", main = "Trace Plot")
