alpha_h <- function(v){
  alpha_h<-0.07*exp(-(v+70)/20)  
}


alpha_m <- function(v){
  
  a_m <- ifelse(abs(v+45)>1.0e-8,(v+45)/10./(1-exp(-(v+45)/10)),1)
  
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

h_inf <- function(v){alpha_h(v)/(alpha_h(v) + beta_h(v))}
m_inf <- function(v){alpha_m(v)/(alpha_m(v) + beta_m(v))}
n_inf <- function(v){alpha_n(v)/(alpha_n(v) + beta_n(v))}

tau_h <- function(v){1/(alpha_h(v) + beta_h(v))}
tau_m <- function(v){1/(alpha_m(v) + beta_m(v))}
tau_n <- function(v){1/(alpha_n(v) + beta_n(v))}