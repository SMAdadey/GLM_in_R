# GLM_in_R
Generalized Linear Models in R
library(tidyverse)
cyto <- cytokine_da

#######Check continuous variables
continuous <- select_if(cyto, is.numeric)
summary(continuous)

####Checking the distribution (Histogram)
library(ggplot2)
ggplot(continuous,aes(x = `IL-10_W3`)) + geom_density(alpha = .2, fill = "green")

######computing for the top percentiles 
top_Pecent <- quantile(cyto$`IL-10_W3`, .99)
top_Pecent

#####standardize continuous variables 
cyto.std <- cyto%>% 
      mutate_if(is.numeric, funs(as.numeric(scale(.))))
head(cyto.std)
ggplot(cyto.std, aes(x = `IL-1_beta_W12`)) + geom_density(alpha = .2, fill = "red")

#######Check factor variables (select categorical variables)
factor <- data.frame(select_if(cyto, is.factor))
ncol(factor)

######Check visualization for correlation
#convert to numeric variables 
corr.numeric <- data.frame(lapply(cyto.std, as.integer))
head(corr.numeric)

install.packages("GGally")
library(GGally)

ggcorr(corr.numeric,
       method = c("pairwise", "spearman"),
       nbreaks = 6,
       hjust = 0.8,
       label = TRUE,
       label_size = 3,
       color = "gray50")


########Generalized Linear Model

logistic_cyto <- glm(status~
                        var1+
                        var2+
                        var3+
                        var4,
                     data = cytokine_data,
                     family = binomial(link = 'logit'))
summary(logistic_cyto)
exp(cbind(OR = coef(logistic_cyto), confint(logistic_cyto)))
