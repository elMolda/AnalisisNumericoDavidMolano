t = c(0,0.25,0.5,0.75,1)
x = c(-1,0,1,0,1)
y = c(0,1,0.5,0,-1)

lagrange_aux <- function(t, cor) {
  
  l <- list() 
  k <- 1
  
  for (i in t) {
    
    num <- 1
    denom <- 1
    
    p <- t[! t %in% i]
    
    for (j in p) {
      num <- paste(num, "*", "(", 't', " - ", as.character(j), ")", sep = "", collapse = "")
      denom <- paste(denom, "*", "(", as.character(i)," - ", as.character(j), ")", sep = "", collapse = "")
    }
    
    l[k] <- paste("(", num, ")", "/", "(", denom, ")", sep = "", collapse = "")
    k <- k + 1
  }
  
  eq <- 0
  
  for (i in 1:length(cor)) {
    eq <- paste(eq, '+', as.character(cor[i]), "*", l[[i]], sep = "", collapse = "")
  }
  
  return(eq)
}


lagrange_parametric <- function(t, x, y){
  
  x_cor = lagrange_aux(t, x)
  y_cor = lagrange_aux(t, y)
  return(list(x_cor,y_cor))
  
}

lagrange_parametric(t,x,y)
