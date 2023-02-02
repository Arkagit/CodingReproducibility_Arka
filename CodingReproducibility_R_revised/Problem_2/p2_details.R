library(MASS)



alpha = 0.9 #multiplier



p = 10 #dimension

Cov = matrix(0,nrow = p, ncol = p) #Initializing covariance matrix

rho = 0.75 #correlation

for(i in 1:p){
  
  for(j in 1:p){
    
    Cov[i,j] = rho^abs(i-j)
    
  }
  
}



mean = rep(0,10) #mean of noise vectors



n = 1000 #sample size




#Function for producing element-sum of VAR process

Process = function(multiplier, mean, Var, dimension, size){ 
  
  vec = matrix(0, nrow = p, ncol = n) #Initializing vectors
  
  Y = numeric(0)
  
  Y[1] = 0
  
  for(i in 2:n){#Generating Auto-regressive Process
    
    vec[,i] = multiplier*vec[,i-1] + mvrnorm(1,mean,Var)
    
    Y[i] = sum(vec[,i])
    
  }
  
  return(Y)
  
}
