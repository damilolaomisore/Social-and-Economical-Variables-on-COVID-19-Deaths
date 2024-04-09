#-----Reading Data-------------------------------------------

setwd(dirname(file.choose()))
getwd()


cdeath_data <- read.csv("freshdata2.csv", stringsAsFactors = FALSE)
head(cdeath_data)    # Inspect top rows of the data
str(cdeath_data)

#-----EDA-------------------------------------------
attach(cdeath_data)


# check for missing data
apply(cdeath_data, MARGIN = 2, FUN = function(x) sum(is.na(x)))
 library(Amelia)
missmap(cdeath_data, y.labels =District, col = c("black", "pink"), legend = TRUE)

#----England Map--------------
library(rgdal)
library(GISTools)
library(RColorBrewer)

# Read in the shapefile of london_polygon
Map.polygon <- readOGR(".", "LAD_DEC_2021_UK_BGC")


write.csv(Map.polygon, file="Map.polygon.csv")
Map.polygon2 <- read.csv("Map.polygon.csv", stringsAsFactors = FALSE)
Map.polygon@data <- within(Map.polygon@data, death_number <- (Map.polygon2$death_number))
plot(Map.polygon, border = "black", col = "lightgrey")

names(Map.polygon@data)

# Set colour and number of classes
shades <- auto.shading(Map.polygon$death_number, n = 5, cols = brewer.pal(5, "Greens"))

# Draw the map polygons
choropleth(Map.polygon, Map.polygon$death_number, shades)
title("England")
choro.legend(557000, 182000, shades, fmt = "%g", title = "Deaths", cex=0.5)
box(which = "outer")


#-----Normality check-------------------------------------------

summary(Total_COVID_Deaths)

#Histogram with density curve
hist(Total_Covid_Deaths, col = "greenyellow", border = "dark green", freq = F, ylim = c(0,0.003),
     xlab = "Total_Covid_Deaths", main = "Histogram")
rug (Total_Covid_Deaths)
lines (density(sort(Total_Covid_Deaths)))
xfit <- seq(from = min(Total_Covid_Deaths), to = max(Total_Covid_Deaths), by = 0.1)
yfit = dnorm(xfit, mean(Total_Covid_Deaths), sd(Total_Covid_Deaths))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

boxplot(Total_Covid_Deaths, col="greenyellow")
#boxplot(Total_Covid_Deaths, ylim= c(0,1000))

qqnorm(Total_Covid_Deaths, xlab = "Theoretical Quantiles: COVID Deaths" )
qqline(Total_Covid_Deaths, col="green") ## red color

ks.test(Total_Covid_Deaths,"pnorm", mean(Total_Covid_Deaths), sd(Total_Covid_Deaths))



#-----Check the variables for outliers using boxplot-------------------------------------------
boxplot(Age_0_to_19, Age_20_to_29, Age_30_to_59, Age_60_and_above,
        names = c("Age0_to_19","Age_20_to_29","Age_30_to_59","Age_60_and_above"),
        xlab = "Age groups", ylab ="frequency", col = "greenyellow")

boxplot(Males, Females,
        names = c("Males","Females"),
        xlab = "Gender groups", ylab ="frequency", col = "greenyellow")

boxplot(White, Mixed, Asian, Black, Other_ethnic_group,
        names = c("White","Mixed", "Asian", "Black", "Other_ethnic_groups"),
        xlab = "Ethnic groups", ylab ="frequency", col = "greenyellow")

boxplot(Good_Health, Fair_Health, Bad_Health,
        names = c("Good_Health","Fair_Health", "Bad_Health"),
        xlab = "Health groups", ylab ="frequency", col = "greenyellow")

boxplot(No_heating, Heating,
        names = c("No_heating","Heating"),
        xlab = "Heating groups", ylab ="frequency", col = "greenyellow")

boxplot(Upper_class, Middle_class, Working_class, Lower_class,
        names = c("Upper_class","Middle_class", "Working_class", "Lower_class"),
        xlab = "Social class", ylab ="frequency", col = "greenyellow")

boxplot(Public_Transport, Private_Transport, Other_Transport, No_Transport,
        names = c("Public_Transport","Private_Transport", "Other_Transport", "No_Transport"),
        xlab = "Travel to work", ylab ="frequency", col = "greenyellow")

# barplot(Total_Covid_Deaths, xlab="Covid Deaths", ylab="Count", main="COVID Deaths by Area", names.arg=Area,col="Blue")

#-----Correlation test-------------------------------------------
cdeath_data2 <- cdeath_data[17:48]
cor_test <- cor(cdeath_data2)
cor_test <- round(cor_test, digits = 2)

#-----Normalizing data in per 1000-------------------------------------------
summary(Total_Covid_Deaths)
cdeath_data2 <- within(cdeath_data2, pDeaths <- (Total_Covid_Deaths/Total_age) * 1000)
cdeath_data2 <- within(cdeath_data2, pAge_0_to_19 <- (Age_0_to_19/Total_age) * 1000)
cdeath_data2 <- within(cdeath_data2, pAge_20_to_29 <- (Age_20_to_29/Total_age) * 1000)
cdeath_data2 <- within(cdeath_data2, pAge_30_to_59 <- (Age_30_to_59/Total_age) * 1000)
cdeath_data2 <- within(cdeath_data2, pAge_60_and_above <- (Age_60_and_above/Total_age) * 1000)
cdeath_data2 <- within(cdeath_data2, pMales <- (Males/Total_sex) * 1000)
cdeath_data2 <- within(cdeath_data2, pFemales <- (Females/Total_sex) * 1000)
cdeath_data2 <- within(cdeath_data2, pWhites <- (White/Total_ethnicity) * 1000)
cdeath_data2 <- within(cdeath_data2, pMixed <- (Mixed/Total_ethnicity) * 1000)
cdeath_data2 <- within(cdeath_data2, pAsian <- (Asian/Total_ethnicity) * 1000)
cdeath_data2 <- within(cdeath_data2, pBlack <- (Black/Total_ethnicity) * 1000)
cdeath_data2 <- within(cdeath_data2, pOther_ethnic_group <- (Other_ethnic_group/Total_ethnicity) * 1000)
cdeath_data2 <- within(cdeath_data2, pGood_Health <- (Good_Health/Total_Health) * 1000)
cdeath_data2 <- within(cdeath_data2, pFair_Health <- (Fair_Health/Total_Health) * 1000)
cdeath_data2 <- within(cdeath_data2, pBad_Health <- (Bad_Health/Total_Health) * 1000)
cdeath_data2 <- within(cdeath_data2, pNo_heating <- (No_heating/Total_heating) * 1000)
cdeath_data2 <- within(cdeath_data2, pHeating <- (Heating/Total_heating) * 1000)
cdeath_data2 <- within(cdeath_data2, pUpper_class <- (Upper_class/Total_s_grade) * 1000)
cdeath_data2 <- within(cdeath_data2, pMiddle_class <- (Middle_class/Total_s_grade) * 1000)
cdeath_data2 <- within(cdeath_data2, pWorking_class <- (Working_class/Total_s_grade) * 1000)
cdeath_data2 <- within(cdeath_data2, pLower_class <- (Lower_class/Total_s_grade) * 1000)
cdeath_data2 <- within(cdeath_data2, pPublic_Transport <- (Public_Transport/Total_transport) * 1000)
cdeath_data2 <- within(cdeath_data2, pPrivate_Transport <- (Private_Transport/Total_transport) * 1000)
cdeath_data2 <- within(cdeath_data2, pOther_Transport <- (Other_Transport/Total_transport) * 1000)
cdeath_data2 <- within(cdeath_data2, pNo_Transport <- (No_Transport/Total_transport) * 1000)


#-----Normality check for standardized data-------------------------------------------
detach(cdeath_data)
attach(cdeath_data2)

summary(pDeaths)

hist(pDeaths)
hist(pDeaths, col = "greenyellow", border = "dark green", freq = F, ylim = c(0,0.8),
     xlab = "pDeaths", main = "Histogram")
rug (pDeaths)
lines (density(sort(pDeaths)))
xfit <- seq(from = min(pDeaths), to = max(pDeaths), by = 0.1)
yfit = dnorm(xfit, mean(pDeaths), sd(pDeaths))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

boxplot(pDeaths, col="greenyellow")

qqnorm(pDeaths, xlab = "Theoretical Quantiles: COVID Deaths" )
qqline(pDeaths, col="greenyellow") ## red color

ks.test(pDeaths,"pnorm", mean(pDeaths), sd(pDeaths))

boxplot(pAge_0_to_19, pAge_20_to_29, pAge_30_to_59, pAge_60_and_above,
        names = c("pAge0_to_19","pAge_20_to_29","pAge_30_to_59","pAge_60_and_above"),
        xlab = "Age groups per 1000", ylab ="frequency", col = "greenyellow")

boxplot(pMales, pFemales,
        names = c("pMales","pFemales"),
        xlab = "Gender groups per 1000", ylab ="frequency", col = "greenyellow")

boxplot(pWhite, pMixed, pAsian, puBlack, pOther_ethnic_group,
        names = c("pWhite","pMixed", "pAsian", "pBlack", "pOther_ethnic_groups"),
        xlab = "Ethnic groups", ylab ="frequency", col = "greenyellow")

boxplot(Good_Health, Fair_Health, Bad_Health,
        names = c("Good_Health","Fair_Health", "Bad_Health"),
        xlab = "Health groups", ylab ="frequency", col = "greenyellow")

boxplot(No_heating, Heating,
        names = c("No_heating","Heating"),
        xlab = "Heating groups", ylab ="frequency", col = "greenyellow")

boxplot(Upper_class, Middle_class, Working_class, Lower_class,
        names = c("Upper_class","Middle_class", "Working_class", "Lower_class"),
        xlab = "Social class", ylab ="frequency", col = "greenyellow")

boxplot(Public_Transport, Private_Transport, Other_Transport, No_Transport,
        names = c("Public_Transport","Private_Transport", "Other_Transport", "No_Transport"),
        xlab = "Travel to work", ylab ="frequency", col = "greenyellow")

#-----Correlation Matrix-------------------------------------------
cdeath_data3 <- cdeath_data2[33:57]
cor_test2 <- cor(cdeath_data3)
cor_test2 <- round(cor_test2, digits = 2)

library(corrplot)
corrplot(cor_test2, type = "upper", tl.col = "black", tl.srt = 45)

detach(cdeath_data2)
attach(cdeath_data3)

pairs(~ pDeaths + pAge_0_to_19 + pAge_20_to_29 + pAge_30_to_59 + pAge_60_and_above, data = cdeath_data3, main = "Multivariate scatterplot matrix of pDeath & pAge", col = "greenyellow")

pairs(~ pDeaths + pMales + pFemales, data = cdeath_data3, main = "Multivariate scatterplot matrix of pDeath & pGender", col = "greenyellow")

pairs(~ pDeaths + pWhites + pMixed + pAsian + pBlack + pOther_ethnic_group, data = cdeath_data3, main = "Multivariate scatterplot matrix of pDeath & pEthnicity", col = "greenyellow")

pairs(~ pDeaths + pGood_Health + pFair_Health + pBad_Health, data = cdeath_data3, main = "Multivariate scatterplot matrix of pDeath & pHealth", col = "greenyellow")

pairs(~ pDeaths + pNo_heating + pHeating, data = cdeath_data3, main = "Multivariate scatterplot matrix of pDeath & pHeating", col = "#8B7355")

pairs(~ pDeaths + pUpper_class + pMiddle_class + pLower_class, data = cdeath_data3, main = "Multivariate scatterplot matrix of pDeath & pSocial_grade", col = "#8B2252")

pairs(~ pDeaths + pPublic_Transport + pPrivate_Transport + pOther_Transport + pNo_Transport, data = cdeath_data3, main = "Multivariate scatterplot matrix of pDeath & pTransport", col = "#8B4513")

# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.6, proceed to factor analysis
library(psych)
KMO(cor(cdeath_data3))

#---------Multiple Regression Modelling---------------------
# model with all variables 
model1 <- lm(pDeaths ~ pAge_0_to_19 + pAge_20_to_29 + pAge_30_to_59 + pAge_60_and_above + pMales + pFemales + pWhites 
             + pMixed + pAsian + pBlack + pOther_ethnic_group + pGood_Health + pFair_Health + pBad_Health + pNo_heating 
             + pHeating + pUpper_class + pMiddle_class +pWorking_class + pLower_class + pPublic_Transport + pPrivate_Transport + pOther_Transport 
             + pNo_Transport)

summary(model1)
# calculate variance inflation factor
library(car)
vif(model1)
sqrt(vif(model1)) > 2  # if > 2 vif too high


model2 <- lm(pDeaths ~ pAge_0_to_19 + pAge_20_to_29 + pAge_60_and_above + pFemales + pOther_ethnic_group + pGood_Health + pBad_Health + pNo_heating 
             + pHeating + pMiddle_class + pLower_class + pPublic_Transport + pNo_Transport)

summary(model2)
# calculate variance inflation factor
vif(model2)
sqrt(vif(model2)) > 2  # if > 2 vif too high

model2b <- lm(pDeaths ~ pAge_0_to_19 +  pNo_heating + pPublic_Transport)

summary(model2b)
# calculate variance inflation factor
vif(model2b)
sqrt(vif(model2b)) > 2  # if > 2 vif too high

model3 <- lm(pDeaths ~ pAge_0_to_19 + pAge_60_and_above + pBad_Health + pNo_heating 
              + pMiddle_class + pPublic_Transport)

summary(model3)
# calculate variance inflation factor
vif(model3)
sqrt(vif(model3)) > 2  # if > 2 vif too high


model4 <- lm(pDeaths ~ pAge_0_to_19 + pAge_60_and_above + pBad_Health + pNo_heating + pPublic_Transport)

summary(model4)

hist(model4$residuals)
rug(model4$residuals)
plot(model4$residuals ~ model4$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model4$residuals, "pnorm", mean(model4$residuals), sd(model4$residuals))
# calculate variance inflation factor
vif(model4)
sqrt(vif(model4)) > 2  # if > 2 vif too high

# test whether model3 and model4 are significantly different using F test
anova(model3, model4, test = "F")


model5 <- lm(pDeaths ~ pAge_0_to_19 + pAge_60_and_above + pBad_Health + pNo_heating + pMiddle_class+ pPublic_Transport + pAsian)

summary(model5)
# calculate variance inflation factor
vif(model5)
sqrt(vif(model5)) > 2  # if > 2 vif too high

library(RcmdrMisc)
library(relaimpo)

model6 <- stepwise(model1, direction = "forward")
summary(model6)
vif(model6)
sqrt(vif(model6)) > 2  # if > 2 vif too high
hist(model6$residuals)
rug(model6$residuals)
plot(model5$residuals ~ model5$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model5$residuals, "pnorm", mean(model5$residuals), sd(model5$residuals))
sqrt(vif(model6)) > 2
calc.relimp(model6, type = c("lmg"), rela = TRUE)
# test whether model4 and model6 are significantly different using F test
anova(model4, model6, test = "F")
