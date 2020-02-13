n = m = 30; mu = 180; nu = 175; sd = 5; B = 1000
p = numeric(B)
for(b in 1:B){
  x = rnorm(n, mu, sd)
  y = rnorm(m, nu, sd) 
  p[b] = t.test(x, y, var.equal = TRUE)[[3]]
}
power = mean(p < 0.05)
