# ASSIGNMENT 1 EXERCISE 3 #

# A

data = read.table(file = "telephone.txt", header=TRUE)
nonzero = apply(data, 1, function(row) all(row > 0))
clean_data = data[nonzero, ]
hist(clean_data,prob=T, main="Histogram of telephone bills", xlab="amount on bill")

# Most frequent customer types are customers 
# with a relatively low phone bill and a realatively high phone bill. 
# Therefore, the markerters can target both groups
# and should not invest too much in advertisement for the middle segment.

# B

# Bootstrap Test
# test statistic T = median(X 1 , . . . , X 200 )
# exponential distribution Exp(λ) with some λ from [0.01, 0.1]

# h0 does clean_data stem from the exponential distr with lambda 0.01-0.1?

T = median(clean_data)
T

B = 1000

Tstar = numeric(B)

n = length(clean_data)
n

for (i in 1:B) {
  l = runif(1, 0.01, 0.1)
  Xstar = rexp(n, l)  
  Tstar[i] = median(Xstar)
}
hist(Tstar, prob=T)

pl=sum(Tstar<T)/B
pr=sum(Tstar>T)/B
p=2*min(pl,pr)
pl;pr;p

#C

# Construct a 95% bootstrap confidence interval 
# for the population median for the sample

clean_data
T
B = 1000
Tstar = numeric(B)

for (i in 1:B){
  Xstar = sample(clean_data, replace=TRUE)
  Tstar[i] = median(Xstar)
}

Tstar25 = quantile(Tstar,0.025)
Tstar975 = quantile(Tstar,0.975)
sum(Tstar<Tstar25)
c(2*T-Tstar975,2*T-Tstar25)

#The 95% bootstrap confidence interval for the population media of telephone data is [18, 37] around its median T = 29.

# D

# Assume this sample originate from an exponential distribution for an unknown lambda
# Use Central Limit Theorem for sample mean
# Estimate lambda
# Construct 95% confidence interval for the population media
# Comment on findings 





# ASSIGNMENT 1 EXERCISE 4 #

# A

data = read.table(file = "run.txt", header=TRUE)

# Check for normality
par(mfrow=c(1,2))
qqnorm(data$before, main = "Normal Q-Q Plot data before drink")
qqnorm(data$after, main = "Normal Q-Q Plot data after drink")

# Pearson's correlation test
cor.test(data$before,data$after)

# B

# H0 : soda_before - soda_after = 0
soda = subset(data, drink=='lemo', select=c(before, after))
t.test(soda$before, soda$after, paired=TRUE)

# H0 energy_before - energy_after = 0
energy = subset(data, drink=='energy', select=c(before, after))
t.test(energy$before, energy$after, paired=TRUE)

# C 

n = nrow(soda)
diff_soda = numeric(n)
for (i in 1:n) {
  diff_soda[i] = soda$before[i] - soda$after[i]
}

m = nrow(energy)
diff_energy = numeric(m)
for (i in 1:m) {
  diff_energy[i] = energy$before[i] - energy$after[i]
}

# H0 : mean_diff_soda = mean_diff_energy

hist(diff_soda)
hist(diff_energy)
boxplot(diff_soda,diff_energy)

t.test(diff_soda,diff_energy)

# D
