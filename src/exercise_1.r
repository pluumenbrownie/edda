install.packages('pracma')

cholesterol <- read.table("data/cholesterol.txt", header = TRUE)
chol_frame <- data.frame(cholesterol)

p1 <- hist(chol_frame$Before, 8)
p2 <- hist(chol_frame$After8weeks, 6)
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,10))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,10), add=T)
qqnorm(chol_frame$Before)
qqline(chol_frame$Before)
qqnorm(chol_frame$After8weeks)
qqline(chol_frame$After8weeks)
# very small sample size, distributions are centered with large peak in center - appear normal
cor(chol_frame$Before, chol_frame$After8weeks)
# before and after heavily correlated


#the experiment outcomes are paired - same person at two time units


t.test(chol_frame$Before, chol_frame$After8weeks, paired=TRUE)
#H0 (no difference in mean) is rejected with mean decrease of 0.63 after 8 weeks.


# permutation test can be applied - paired samples
X = chol_frame$Before
Y = chol_frame$After8weeks
B = 10000
Tstar = numeric(B)
for(i in 1:B){
  xystar = t(apply(cbind(X, Y), 1, sample))
  Tstar[i] = mean(xystar[,1] - xystar[,2])
}
t = mean(X-Y)
hist(Tstar)
pl=sum(Tstar<t)/B;pr=sum(Tstar>t)/B
p = 2*min(pl, pr); p
# p<<0.95 so there is a significant difference after 8 weeks


# Mann-Whitney can be applied but is less strong - designed for samples from different populations
wilcox.test(X, Y)
# again null hypothesis is rejected


# we have a discrete sample whose true std is unknown -> student dist
X <- chol_frame$After8weeks
print("97 percent confidence interval assuming normality:")
# mean(X) + qnorm(c(0.015, 1-0.015)) * sd(X) / sqrt(mean(X))
mean(X) + qt(c(0.015, 1-0.015), df=length(X)-1) * sd(X) / sqrt(mean(X))

B = 1000
Tstar = numeric(B)
for(i in 1:B){
  Xstar = sample(X, replace=TRUE)
  Tstar[i] = mean(Xstar)
}
Tstar_q = quantile(-Tstar, c(0.015, 1-0.015))
mean(Tstar)
mean(X)
2*mean(Tstar) + Tstar_q

# with bootstrapping we get a significantly smaller confidence interval.


# d)
B = 1000
Tstar = numeric(B)
theta = seq(from=3, to=12, by=0.001)
num_test_pts = length(theta)
p = numeric(num_test_pts)
for(n in 1:num_test_pts){
  for(i in 1:B){
    Xstar = runif(length(X), 3,theta[n])
    Tstar[i] = max(Xstar)
  }
  t = max(X)
  pl=sum(Tstar<t)/B;pr=sum(Tstar>t)/B
  p[n] = 2*min(pl, pr)
}
plot(theta, p)
print(theta[p>0.95])

# for theta between 7.832 and 7.875 we have p>0.95 i.e. do not reject H0. 




mean(Tstar)
mean(X)
2*mean(Tstar) + Tstar_q









county_anova <- lm(Crops ~ County, data = crops_frame)
related_anova <- lm(Crops ~ Related, data = crops_frame)
both_anova <- lm(Crops ~ Related * County, data = crops_frame)

boxplot(crops$Crops)

print(crops_frame)
print("### County ###")
print(anova(county_anova))
# print(summary(county_anova))
print("### Related ###")
print(anova(related_anova))
# print(summary(related_anova))

print("### Country and Related ###")
print(anova(both_anova))

qqnorm(residuals(both_anova))
plot(fitted(both_anova), residuals(both_anova))

print("")
print("### 2a ###")
ancova_county_lm <- lm(Crops ~ Size + County, data = crops_frame)
ancova_related_lm <- lm(Crops ~ Size + Related, data = crops_frame)

print(drop1(ancova_county_lm, test = "F"))
print(drop1(ancova_related_lm, test = "F"))
print(" -> The significance for County and Related are to low")
print("when accounting for Size.")
qqnorm(residuals(ancova_county_lm))
qqnorm(residuals(ancova_related_lm))

