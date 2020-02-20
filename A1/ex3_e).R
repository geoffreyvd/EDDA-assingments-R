#e) sign test
length(clean_data) #length = 192
t = median(clean_data) #t = 28.905
sum(clean_data>t) #sum = 96
binom.test(96,192,p=0.5)
#p-value=1, do not reject H0


# fraction 
t = length(clean_data[clean_data<10]) / length(clean_data)
B = 1000
tstar = numeric(B)
n = length(clean_data)
for(i in 1:B){
  xstar = sample(clean_data, replace = TRUE)
  tstar[i] = length(xstar[xstar<10]) / length(xstar)
}
pl = sum(tstar<t) / B
pr = sum(tstar>t) / B
p = 2*min(pl,pr)
pl;pr;p