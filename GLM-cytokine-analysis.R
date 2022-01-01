###########  Finding relationship between IRIS status and cytokines
##### Loading the packages 
library(tidyverse)
install.packages("ggpubr")
library(ggpubr)

#### Working directory
setwd("~/2. Other docs/10. Seth/New/Dec2021/Analysis2")
getwd()

#### upload dataset
iris <- read.csv("20211231-Analysis.csv", header = TRUE, sep = ",")
str(iris)

#### getting the header of the data
names(iris[5:8])

###  Transforming data (iris)
plot(iris$IL1_beta_W3)
iris.log <- log(iris)
iris.log$IRIS_status <- 10^iris.log$IRIS_status

head(iris.log)

plot(iris.log$IRIS_status)
plot(iris.log$MCP1_W3)

###  GLM Univariate procedure
names(iris.log[1])

iris.glm <- glm(IRIS_status~
                  IL10_W3,
                data = iris.log,
                family = binomial(link = 'logit'))
summary(iris.glm)

OR.iris <- exp(cbind(OR = coef(iris.glm), confint(iris.glm)))
OR.iris
