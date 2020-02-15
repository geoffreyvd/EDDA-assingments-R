# ASSIGNMENT 1 EXERCISE 3 #

# A

data = read.table(file = "telephone.txt", header=TRUE)
nonzero = apply(data, 1, function(row) all(row > 0))
clean_data = data[nonzero, ]
hist(clean_data,prob=T, main="Histogram of telephone bills", xlab="amount on bill")

# B

# Test statistic is the median of the data
t = median(clean_data)
B = 1000
tstar = numeric(B)
n = length(clean_data)
l = 0.05 # lambda

for (i in 1:B) {
  xstar = rexp(n, l)  
  tstar[i] = median(xstar)
}
hist(tstar, prob=T)

# Calculate the p-value
pl=sum(tstar<t)/B
pr=sum(tstar>t)/B
p=2*min(pl,pr)

#C

B = 1000
tstar = numeric(B)

for (i in 1:B){
  xstar = sample(clean_data, replace=TRUE)
  tstar[i] = median(xstar)
}

tstar25 = quantile(tstar,0.025)
tstar975 = quantile(tstar,0.975)
sum(tstar<tstar25)
c(2*t-tstar975,2*t-tstar25)

# D

# Assume this sample originate from an exponential distribution for an unknown lambda
# Use Central Limit Theorem for sample mean
# Estimate lambda
# Construct 95% confidence interval for the population media
# Comment on findings 


# ----------------------------------------------------------------------


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
