size = c(100,500,1000,5000,10000) #Sample sizes

iter = 500 #Number of iterations

shape = 0.01 ; rate = 0.01 #Parameters

alpha = 0.05 #confidence level



coverage_freq = function(shape, rate, alpha){#function for getting coverage probability
  
  samp = rgamma(1e4 ,shape= shape, rate= rate) #n1 samples generated from Gamma(shape, scale)
  
  bar_x = c(mean(samp[1:1e2]),mean(samp[1:5e2]),mean(samp[1:1e3]),mean(samp[1:5e3]),mean(samp[1:1e4])) #sample mean 
  
  s2 = c(var(samp[1:1e2]), var(samp[1:5e2]), var(samp[1:1e3]),var(samp[1:5e3]),var(samp[1:1e4])) #sample variance
  
  low_lim = bar_x - qnorm(1 - alpha/2)*sqrt(s2/size) #lower limit of CI
  
  up_lim = bar_x + qnorm(1 - alpha/2)*sqrt(s2/size); #upper limit of CI
  
  m = shape/rate #True mean
  
  count = c(0,0,0,0,0) #countar variable for mean inside CI
  
  for(i in 1:5){
    
    if(m >= low_lim[i] & m <= up_lim[i]){
      
      count[i] = 1
      
    } 
    
  }
  
  return(count)
  
}



prob1 = c(0,0,0,0,0) #Initializing relative frequency for inclusion of true mean

for(j in 1:iter){
  
  prob1 = prob1 + coverage_freq(0.01,0.01,0.05)/iter
  
}