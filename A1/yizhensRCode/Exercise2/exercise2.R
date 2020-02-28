light = scan(file = "~/Desktop/light.txt")
light1879 = scan(file = "~/Desktop/light1879.txt")
light1882 = scan(file = "~/Desktop/light1882.txt")

light1879 = light1879 + 299000
light1882 = light1882 + 299000
light = 7.442 / (light/1000+24.8)*1000000

par(mfrow=c(1,3))
hist(light, prob=T)
hist(light1879, prob=T)
hist(light1882, prob=T)
#Through the histgram we could see that these three datasets are similar to each other, and the normality situation seems better in the second and third one. 
#The hisogram of "light" is right-skewed, which means in "light" the mean is typically GREATER THAN the median  
#There are more outliers in "light"

par(mfrow=c(1,3))
qqnorm(light, main = "light")
qqnorm(light1879, main = "light1879")
qqnorm(light1882, main = "light1882")
# The normality in 1879 seems quite good, while in light and 1882, the normality is doutful 


par(mfrow=c(1,3))
boxplot(light, main="light")
boxplot(light1879, main="light1879")
boxplot(light1882, main="light1882")
#The light 1879 and 1882 datasets have a wider range of data. The normality in first two datasets looks better than in the third one


B = 1000
T1star = numeric(B)
T1879star = numeric(B)
T1882star = numeric(B)

#mean 
for(i in 1:B){
  X1star = sample(light, replace=TRUE)
  X1879star = sample(light1879, replace=TRUE)
  X1882star = sample(light1882, replace=TRUE)
  T1star[i] = mean(X1star)
  T1879star[i] = mean(X1879star)
  T1882star[i] = mean(X1882star)
}

T1star25 = quantile(T1star, 0.025)
T1star975 = quantile(T1star, 0.975)
T1879star25 = quantile(T1879star, 0.025)
T1879star975 = quantile(T1879star, 0.975)
T1882star25 = quantile(T1882star, 0.025)
T1882star975 = quantile(T1882star, 0.975)

T1 = mean(light)
T1
T1879 = mean(light1879)
T1879
T1882 = mean(light1882)
T1882

#confidence interval 
c(2*T1 - T1star975, 2*T1-T1star25)
c(2*T1879 - T1879star975, 2*T1879-T1879star25)
c(2*T1882 - T1882star975, 2*T1882-T1882star25)


#median
for(i in 1:B){
  X1star = sample(light, replace=TRUE)
  X1879star = sample(light1879, replace=TRUE)
  X1882star = sample(light1882, replace=TRUE)
  T1star[i] = median(X1star)
  T1879star[i] = median(X1879star)
  T1882star[i] = median(X1882star)
}

T1star25 = quantile(T1star, 0.025)
T1star975 = quantile(T1star, 0.975)
T1879star25 = quantile(T1879star, 0.025)
T1879star975 = quantile(T1879star, 0.975)
T1882star25 = quantile(T1882star, 0.025)
T1882star975 = quantile(T1882star, 0.975)

#median
T1 = median(light)
T1
T1879 = median(light1879)
T1879
T1882 = median(light1882)
T1882

#confidence interval 
c(2*T1 - T1star975, 2*T1-T1star25)
c(2*T1879 - T1879star975, 2*T1879-T1879star25)
c(2*T1882 - T1882star975, 2*T1882-T1882star25)

