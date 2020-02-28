#Basic parameters 
n = m = 30; mu = 180; sd = 5; B = 1000
nu = seq(175,185,by=0.25)
p = numeric(B)

#(a)
power_a = numeric(40)
for(i in 1:40){
  for(b in 1:B){
    x = rnorm(n, mu, sd)
    y = rnorm(m, nu[i], sd) 
    p[b] = t.test(x, y, var.equal = TRUE)[[3]]
  }
  power_a[i] = mean(p < 0.05)
}

#(b)
m_b = n_b = 100
power_b = numeric(40)
for(i in 1:40){
  for(b in 1:B){
    x = rnorm(n_b, mu, sd)
    y = rnorm(m_b, nu[i], sd) 
    p[b] = t.test(x, y, var.equal = TRUE)[[3]]
  }
  power_b[i] = mean(p < 0.05)
}

#(c)
sd_c = 15
power_c = numeric(40)
for(i in 1:40){
  for(b in 1:B){
    x = rnorm(n, mu, sd_c)
    y = rnorm(m, nu[i], sd_c) 
    p[b] = t.test(x, y, var.equal = TRUE)[[3]]
  }
  power_c[i] = mean(p < 0.05)
}

#Plot the power function
plot(power_a, col = "blue", main = "Power as a function of nu", xlab = "Range", ylab = "power")
points(power_b,  col = "green")
points(power_c,  col = "red")
