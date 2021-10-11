#Description: Calculates the Gini coefficient of a lorenz curve based on either GQ or Beta distribution parameters.
#Author: Dan W
#Creation date: 2019
#Last revision:
#Notes:

gini_calc <- function(a,b,c,type=list("GQ","Beta")){
  if(type=="GQ"){
    e <- -(a+b+c+1)
    m <- b^2-4*a
    n <- 2*b*e-4*c
    r <- (n^2-4*m*e^2)^(0.5)
    if(m>0){
      G <- (e/2)-((n*(b+2))/(4*m))-((r^2)/(8*m*sqrt(m)))*log(abs((2*m+n+2*sqrt(m)*(a+c-1))/(n-2*e*sqrt(m))))
    } else {
      G <- (e/2)-((n*(b+2))/(4*m))+((r^2)/(8*m*sqrt(-m)))*(asin((2*m+n)/r)-asin(n/r))
    }
  }else{
    G <- 2*a*((gamma(b+1)*gamma(c+1))/gamma(b+c+2)) #2*a*beta(b+1,c+1)
  }
  return(G)
}