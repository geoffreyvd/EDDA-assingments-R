# ASSIGNMENT 1 EXERCISE 3 #

# A

data = read.table(file = "telephone.txt", header=TRUE)
nonzero = apply(data, 1, function(row) all(row > 0))
clean_data = data[nonzero, ]
# 2 plots on 1 figure
par(mfrow=c(1,2))
hist(clean_data,prob=T, main="A) Histogram of telephone bills", xlab="amount on bill")

# B

# Median is test statistic
t = median(clean_data)
n = length(clean_data)
B = 1000
tstar = numeric(B)
# Lambda's ranging from 0.01, 0.02 ... up to 0.1
l = seq(0.01,0.1,by=0.01)

# Create a set of representative medians 
# for the exponential distribution
# with lambda varying between [0.01,0.1]
# and sample length equal to our data sample
for (i in 1:10) {
  for (j in 1:B) {
    xstar = rexp(n, l[i])  
    tstar[j] = median(xstar)
  }
  # Calculate the p-value
  pl=sum(tstar<t)/B
  pr=sum(tstar>t)/B
  p=2*min(pl,pr)
  # Print the p-value
  cat("p-value = ", p, "for lambda = ", l[i], "\n")
  if (p > 0.05){ # h0 is not rejected
    cat("H0 is not rejected with lambda = ", l[i], "\n")
    hist(tstar, prob = T, main = "B) Histogram of T*", xlab = "median")  
  }
}

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
B = 1000
sampleSize = 30
tel_means = numeric(B)
for (i in 1:B){
  sample = sample(clean_data, size = sampleSize)
  tel_means[i] = mean(sample)
}
t = median(tel_means) #median tel_means  45.32217
hist(tel_means)

# Estimate lambda varying between [0.01, 0.1]
l = seq(0.01,0.1,by=0.01)
exp_means = numeric(B)
for (i in 1:10) {
  exp = rexp(1000, rate = l[i])
  for (j in 1:B){
    sample = sample(exp, size = sampleSize)
    exp_means[j] = mean(sample)
  }
  # Print the median
  cat("median = ", median(exp_means), "for lambda = ", l[i], "\n")
}

# Construct 95% confidence interval for the population median
B = 1000
tstar = numeric(B)

for (i in 1:B){
  xstar = sample(rexp(1000, rate = 0.02), replace=TRUE)
  tstar[i] = median(xstar)
}

tstar25 = quantile(tstar,0.025)
tstar975 = quantile(tstar,0.975)
sum(tstar<tstar25)
c(2*t-tstar975,2*t-tstar25)

# E

#Using an appropriate test, 
#test the null hypothesis that the median bill is bigger or equal to 40 euro
#against the alternative that the median bill is smaller than 40 euro. 

# H 0            median bill >=  40 euro
# H alternative  median bill <   40 euro

hist(clean_data)
median(clean_data) #28.905
#Because the data originates from one sample and is not normally distributed,
#we perform a sign test.
x = sum(clean_data>=40)
n = length(clean_data)
binom.test(x,n,p=0.5)

#Next, design and perform a test
#to check whether the fraction of the bills less than 10 euro is at most 25%.



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
