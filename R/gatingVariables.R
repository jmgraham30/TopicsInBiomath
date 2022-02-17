alpha_h <- function(v){
  alpha_h<-0.07*exp(-(v+70)/20)  
}


alpha_m <- function(v){
  if (abs(v+45)>1.0e-8){
    alpha_m<-(v+45)/10./(1-exp(-(v+45)/10))
  }else{
    alpha_m=1  
  }
  
}


alpha_n <- function(v){
  alpha_n<-0.01*(-60.0-v)/(exp((-60-v)/10)-1)  
}


beta_h <- function(v){
  beta_h<-1./(exp(-(v+40)/10)+1)  
}


beta_m <- function(v){
  beta_m<-4*exp(-(v+70)/18)  
}


beta_n <- function(v){
  beta_n<-0.125*exp(-(v+70)/80)  
}