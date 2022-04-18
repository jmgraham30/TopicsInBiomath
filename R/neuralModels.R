source("R/gatingVariables.R")

library(rootSolve)

HH <- function (t, y, parameters) {
  with(as.list(c(y,parameters)),{
    # variables
    v <- y[1]
    h <- y[2]
    m <- y[3]
    n <- y[4]
    
    dv <- (gna*m^3*h*(vna - v) + gk*n^4*(vk - v) + gl*(vl - v) + iext)/C
    dh <- (h_inf(v) - h)/tau_h(v)
    dm <- (m_inf(v) - m)/tau_m(v)
    dn <- (n_inf(v) - n)/tau_n(v)
    
    return(list(c(dv, dh, dm, dn)))
  })
}

HH_reduced <- function (t, y, parameters) {
  with(as.list(c(y,parameters)),{
    # variables
    v <- y[1]
    n <- y[2]
    
    dv <- (gna*m_inf(v)^3*(0.83-n)*(vna - v) + gk*n^4*(vk - v) + gl*(vl - v) + iext)/C
    dn <- (n_inf(v) - n)/tau_n(v)
    
    return(list(c(dv, dn)))
  })
}

FN <- function (t, y, parameters) {
  with(as.list(c(y,parameters)),{
    # variables
    v <- y[1]
    n <- y[2]
    
    dv <- v - v^3/3 - n + iext
    dn <- (a*v - n)/t_n
    
    return(list(c(dv, dn)))
  })
}

HH_v_null <- function(v,n,parameters){
  with(as.list(c(v,n,parameters)),{
    
    rhs <- gna*m_inf(v)^3*(0.83-n)*(vna - v) + gk*n^4*(vk - v) + gl*(vl - v) + iext
    
    return(rhs)
    
  })
}

HH_parameters <- list(C=1,gk=36,gna=120,gl=0.3,vk=-82,vna=45,vl=-59,iext=10)


compute_v_null <- function(v_val){
  if (v_val < -81.52){
    n_val <- 1.0
  }else if(v_val >= -81.52 && v_val <= 44.78 ){
    f <- function(n){HH_v_null(n,v=v_val,parameters=HH_parameters)}
    n_val <- uniroot(f,interval=c(0,1))$root 
  }else {
    n_val <- 0.0
  }
  
  return(n_val)
  
}
