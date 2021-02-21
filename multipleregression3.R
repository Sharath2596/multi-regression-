# Consider only the below columns and prepare a prediction model for predicting Price.

# Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

library(readr)
ToyotaCorolla <- read_csv("ToyotaCorolla.csv")
View(ToyotaCorolla)

attach(ToyotaCorolla)

summary(ToyotaCorolla)

Corolla <- read_csv("ToyotaCorolla.csv")
Corolla <- Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Corolla)

summary(Corolla)

sd(Price)
sd(Age_08_04)
sd(KM)
sd(HP)
sd(cc)
sd(Doors)
sd(Gears)
sd(Quarterly_Tax)
sd(Weight)

var(Price)
var(Age_08_04)
var(KM)
var(HP)
var(cc)
var(Doors)
var(Gears)
var(Quarterly_Tax)
var(Weight)

install.packages("moments")
library(moments)

skewness(Price)
skewness(Age_08_04)
skewness(KM)
skewness(HP)
skewness(cc)
skewness(Doors)
skewness(Gears)
skewness(Quarterly_Tax)
skewness(Weight)

kurtosis(Price)
kurtosis(Age_08_04)
kurtosis(KM)
kurtosis(HP)
kurtosis(cc)
kurtosis(Doors)
kurtosis(Gears)
kurtosis(Quarterly_Tax)
kurtosis(Weight)


plot(Corolla)

plot(Age_08_04, Price)
plot(KM, Price)
plot(HP, Price)
plot(cc, Price)
plot(Doors, Price)
plot(Grears, Price)
plot(Quarterly_Tax, Price)
plot(Weight, Price)

# Correlation coefficient

cor(Corolla)

# partial correlation matrix
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Corolla))

# Scatter plot matrix with correlations inserted in graph

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Corolla, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

# from the correlation result and from the plot.
# we see that there is no collinearity

# Linear model of interest

model1 <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(model1)

# R square value is 0.8638
# But the coefficient values for cc and Doors are insignificant as p-value is greater than 0.05
# Overall model is significant

# Predicting model based on individual record
model.cc <- lm(Price ~ cc)
summary(model.cc)

# Model is significant but R sqaure is 0.01597 which is very less

model.door <- lm(Price ~ Doors)
summary(model.door)

# Model is significant but R sqaure is 0.01597 which is very less

# build with both cc and Doors

model.both <- lm(Price ~ cc + Doors)
summary(model.both)

# Model is significant 


# It is better to delete a single observation rather than entire variable
# Deletion diagnostics for identifying influential variable

install.packages("car")
library(car)
influence.measures(model1)
influenceIndexPlot(model1)


influenceIndexPlot(model1)
influencePlot(model1)

# from this Diagnotic plot we see that we have to delete 81th influential observations 

# Deleting influential observation and build the model

model.inf <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = Corolla[-c(81),])
summary(model.inf)

# R square is 0.8694
# But the coefficient for Doors is insignificant as p-value is greater than 0.05
# Overall model is significant

vif(model1)
avPlots(model1)

# From the avplot we see that the variable Doors must be deleted

model.final <- lm(Price ~ Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight)
summary(model.final)
model.final <- lm(Price ~ Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight, data = Corolla[-c(81),])
summary(model.final)

# Hence the final model is significant 
# R square is 0.8693
# hence the model is a good fit for predicting purpose.

# Evaluating model line assumption

plot(model.final)

qqPlot(model1)

