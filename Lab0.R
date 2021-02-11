install.packages("MASS") # installing the MASS package
library(MASS) # load the library MASS
attach(Boston) # attaching the dataset
?Boston # help function with "?"
head(Boston) # show the head of the dataset
dim(Boston) # dimensions of the dataset
names(Boston) # column names
str(Boston) # str function shows the structure of the dataset
nrow(Boston) # function shows the number of rows
ncol(Boston) # function shows the number of columns
summary(Boston) # summary() function shows the summary statistics
summary(Boston$crim) # summary of the "crime" column in the Boston dataset

install.packages("ISLR") # installing the ISLR package
library(ISLR)
data(Auto)
head(Auto)
names(Auto)
summary(Auto)
summary(Auto$mpg)
fivenum(Auto$mpg)
boxplot(Auto$mpg)
hist(Auto$mpg)
summary(Auto$horsepower)
summary(Auto$weight)
fivenum(Auto$weight)
boxplot(Auto$weight)
mean(Auto$weight)
median((Auto$weight))

data1 <- read.csv(
  file.choose(), header=T
)

head(data1)
View(data1)
boxplot(data1$EPI)
fivenum(data1$EPI, na.rm = TRUE)
