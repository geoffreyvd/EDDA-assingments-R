p.value=function(n,m,mu,nu,sd,B=1000){
  p=numeric(B) 
  for (b in 1:B) {
    x=rnorm(n,mu,sd); y=rnorm(m,nu,sd)
    p[b]=t.test(x,y,var.equal=TRUE)[[3]]
  }
  return(p)
}

n = m = 100; mu = 180; sd = 5;
nu = seq(175,185,by=0.25)
p = p.value(n,m,nu,mu,sd)
power = mean(p<0.05)
hist(p, freq = F, main = "Histogram of p, nu=seq(175,185,by=0.25)")
