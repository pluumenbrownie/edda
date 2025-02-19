crops <- read.table("data/crops.txt", header = TRUE)
crops_frame <- data.frame(crops)
yes_filter <- data.frame(match = c("yes"))
crops_frame$Related <- c(0, 1)[(crops_frame$Related %in% yes_filter) + 1]
crops_frame$County <- factor(crops_frame$County)
crops_frame$Related <- factor(crops_frame$Related)

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
