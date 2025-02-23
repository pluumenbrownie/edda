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

writeLines("\n                          ### County ###")
print(anova(county_anova))
# print(summary(county_anova))
writeLines("\n                          ### Related ###")
print(anova(related_anova))
# print(summary(related_anova))

writeLines("\n                          ### Country and Related ###")
print(anova(both_anova))

qqnorm(residuals(both_anova))
plot(fitted(both_anova), residuals(both_anova))


writeLines("\n\n                          ### 2b ###")
size_anova <- lm(Crops ~ Size, data = crops_frame)
ancova_county_lm <- lm(Crops ~ Size + County, data = crops_frame)
ancova_related_lm <- lm(Crops ~ Size + Related, data = crops_frame)

writeLines("\n                          ### Size ###")
print(anova(size_anova))
writeLines("\n                          ### Size + County ###")
print(drop1(ancova_county_lm, test = "F"))
writeLines("\n                          ### Size + Related ###")
print(drop1(ancova_related_lm, test = "F"))
writeLines(" -> The significance for County and Related are to ")
writeLines("low when accounting for Size.")

qqnorm(residuals(size_anova))
qqnorm(residuals(ancova_county_lm))
qqnorm(residuals(ancova_related_lm))
