setwd("C:/Users/Sharmad/OneDrive - Rensselaer Polytechnic Institute/Documents/ITWS6600/Lab 1")
dir()
epi_data <- read.csv("2010EPI_data.csv", header = TRUE)
names(epi_data)[1] <- "Code"
View(epi_data)
attach(epi_data)
fix(epi_data)

### EXERCISE 1 ###

# Variable EPI

EPI
tf <- is.na(EPI)
EPI2 <- EPI[!tf]
EPI2 <- as.numeric(EPI2)
View(EPI2)
summary(EPI2)
fivenum(EPI2)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))
rug(EPI)
help(stem)
help(ecdf)
plot(ecdf(EPI), do.points=FALSE, verticals = TRUE)
par(pty="s")
qqnorm(EPI); qqline(EPI)
x <- seq(30,95,1)
help(qqplot)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for EPI")
qqline(x)

# Variable DALY

DALY
tf <- is.na(DALY)
DALY2 <- DALY[!tf]
DALY2 <- as.numeric(DALY2)
View(DALY2)
summary(DALY2)
fivenum(DALY2)
stem(DALY)
hist(DALY)
hist(DALY, seq(0., 92., 2.0), prob=TRUE)
lines(density(DALY,na.rm=TRUE,bw=1.))
rug(DALY)
plot(ecdf(DALY), do.points=FALSE, verticals = TRUE)
par(pty="s")
qqnorm(DALY); qqline(DALY)
x2 <- seq(0,92,2)
qqplot(qt(ppoints(250), df = 5), x2, xlab = "Q-Q plot for DALY")
qqline(x2)

# Variable WATER_H

WATER_H
tf <- is.na(WATER_H)
WATER_H2 <- WATER_H[!tf]
WATER_H2 <- as.numeric(WATER_H2)
View(WATER_H2)
summary(WATER_H2)
fivenum(WATER_H2)
stem(WATER_H)
hist(WATER_H)
hist(WATER_H, seq(0., 100., 5.0), prob=TRUE)
lines(density(WATER_H,na.rm=TRUE,bw=1.))
rug(WATER_H)
plot(ecdf(WATER_H), do.points=FALSE, verticals = TRUE)
par(pty="s")
qqnorm(WATER_H); qqline(WATER_H)
x3 <- seq(0,100,5)
qqplot(qt(ppoints(250), df = 5), x3, xlab = "Q-Q plot for WATER_H")
qqline(x3)

# Comparing Distributions

boxplot(EPI,DALY,WATER_H)
qqplot(EPI, DALY)
qqplot(EPI, WATER_H)
qqplot(DALY, WATER_H)

help("distributions")

### EXERCISE 2 ###

EPILand <- EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
lines(density(Eland,na.rm=TRUE,bw=1.))
summary(Eland)
fivenum(Eland)
stem(Eland)
rug(Eland)
plot(ecdf(Eland), do.points=FALSE, verticals = TRUE)
par(pty="s")
qqnorm(Eland); qqline(Eland)
x4 <- seq(30.,95.,1.0)
qqplot(qt(ppoints(250), df = 5), x4, xlab = "Q-Q plot for Eland")
qqline(x4)

# Filtering on regions

EPI_South_Asia <- EPI[GEO_subregion == "South Asia"]
table(GEO_subregion)

## GPW3 DATA ##

gpw_data <- read.csv("GPW3_GRUMP_SummaryInformation_2010.csv", header = TRUE)
View(gpw_data)
names(gpw_data)[1] <- "Country"
attach(gpw_data)


# EDA on one of the variables

Largest.Extent..sq.km.
tf <- is.na(Largest.Extent..sq.km.)
Largest.Extent..sq.km.2 <- Largest.Extent..sq.km.[!tf]
Largest.Extent..sq.km.2 <- as.numeric(gsub(",","", Largest.Extent..sq.km.2))
View(Largest.Extent..sq.km.2)
summary(Largest.Extent..sq.km.2)
fivenum(Largest.Extent..sq.km.2)
stem(Largest.Extent..sq.km.2)
hist(Largest.Extent..sq.km.2)
hist(Largest.Extent..sq.km.2, seq(0., 40000., 2000.), prob=TRUE)
#lines(density(Largest.Extent..sq.km.2,na.rm=TRUE,bw=1.))
rug(Largest.Extent..sq.km.2)
plot(ecdf(Largest.Extent..sq.km.2), do.points=FALSE, verticals = TRUE)
par(pty="s")
qqnorm(Largest.Extent..sq.km.2); qqline(Largest.Extent..sq.km.2)
x5 <- seq(0,40000,2000)
qqplot(qt(ppoints(250), df = 5), x5, xlab = "Q-Q plot for Largest.Extent..sq.km.")
qqline(x5)

#Filtering on region

GPW_Caribbean <- Largest.Extent..sq.km.2[UNRegion == "Caribbean"]
table(UNRegion)
GPW_Caribbean

## 02/11/2021
# Creating plots

plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
?qplot
qplot(wt,mpg,data = mtcars)
ggplot(mtcars, aes(x=wt,y=mpg)) +
  geom_point()
plot(pressure$temperature,pressure$pressure, type='l')
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col='red')
points(pressure$temperature, pressure$pressure/2, col='blue')
qplot(pressure$temperature,pressure$pressure, geom = 'line')
qplot(temperature,pressure,data=pressure,geom='line')
ggplot(pressure,aes(x=temperature,y=pressure)) +
  geom_line() +
  geom_point()

barplot(BOD$demand,names.arg=BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl), data=mtcars)
ggplot(mtcars,aes(x=factor(cyl))) +
  geom_bar()

hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
qplot(mpg,data=mtcars, binwidth=4)
ggplot(mtcars,aes(x=mpg)) +
  geom_histogram(binwidth = 4)
ggplot(mtcars,aes(x=mpg)) +
  geom_histogram(binwidth = 5)


plot(ToothGrowth$supp,ToothGrowth$len)
boxplot(len ~ supp, data = ToothGrowth)
View(ToothGrowth)
boxplot(len ~ supp + dose, data = ToothGrowth)
qplot(ToothGrowth$supp,ToothGrowth$len, geom = 'boxplot')
qplot(supp, len, data=ToothGrowth, geom='boxplot')
ggplot(ToothGrowth,aes(x=supp, y=len)) +
  geom_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = 'boxplot')
qplot(interaction(supp,dose), len, data = ToothGrowth, geom = 'boxplot')
ggplot(ToothGrowth, aes(x=interaction(supp,dose), y=len)) +
  geom_boxplot()
