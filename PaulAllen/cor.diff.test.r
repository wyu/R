cor.diff.test = function(r1, r2, n1, n2, alternative = c("two.sided", "less", "greater")) {
  
  Z1 = 0.5 * log( (1+r1)/(1-r1) )
  Z2 = 0.5 * log( (1+r2)/(1-r2) )
  
  diff = Z1 - Z2
  SEdiff = sqrt( 1 / (n1 - 3) + 1 / (n2 - 3))
  diff.Z = diff / SEdiff
  
  if (alternative == "less") {
    return(pnorm(diff.Z, lower.tail=F))
  } else if (alternative == "greater") {
    return(pnorm(-diff.Z, lower.tail=F))
  } else if (alternative == "two.sided") {
    return(2 * pnorm( abs(diff.Z), lower.tail=F))
  } else {
    warning(paste("Invalid alterantive", alternative), domain=NA)
    return(NA)
  }
}

quadratic=function(coeff) {
  a=coeff[1]
  b=coeff[2]
  c=coeff[3]
  d=b^2 - (4*a*c)
  cat("The discriminant is: ",d,"\n")
  if(d < 0) cat("There are no real roots. ","\n")
  if(d >= 0){
    root1=(-b + sqrt(d))/(2*a)
    root2=(-b-sqrt(d))/(2*a)
    cat("root1: ",root1,"\n")
    cat("root2: ",root2,"\n")
  }
}