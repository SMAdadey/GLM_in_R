##### R- script for ethel project
####### setting the working directory 
setwd("C:/Users/ADDSAM004/Desktop/tekpor")
getwd()

####### importing data
CC.HIV <- read.csv("ethel.csv", header = TRUE, sep = ",")

######## Fewquency tables 
freq.var <- table(CC.HIV$cat.viral.load)
freq.var
percent.var <- round(100 * prop.table(freq.var),1) 
percent.var

####### Chi square test
chisq.test(freq.var)

####### plot for reproductive characters
freq.mon <- table(CC.HIV$menarchy)
barplot(freq.mon, 
        xlab = "Age (years)",
        ylab = "Frequency (n)",
        col = "#808080",
        font.axis = 1)

freq.chd <- table(CC.HIV$parity.children)
barplot(freq.chd, 
        xlab = "Number of children",
        ylab = "Frequency (n)",
        col = "#808080",
        font.axis = 1)

freq.mis <- table(CC.HIV$parity.miscarry)
barplot(freq.mis, 
        xlab = "Number of miscarriages/abortions ",
        ylab = "Frequency (n)",
        col = "#808080",
        font.axis = 1)

freq.contraceptive1 <- table(CC.HIV$Currently.on.Contraceptive)
freq.contraceptive1
#barplot(freq.contraceptive1)
pie(freq.contraceptive1[2:3], radius = 0.8)


freq.contraceptive2 <- table(CC.HIV$past.history.of.contraceptive)
freq.contraceptive2
#barplot(freq.contraceptive2)
pie(freq.contraceptive2[2:3], radius = 0.8)







###### HIV
freq.HIV.diag <- table(CC.HIV$HIV.duration)
freq.HIV.diag
barplot(freq.HIV.diag[2:6])
pie(freq.HIV.diag[2:6], radius = 2)

freq.viral.load <- table(CC.HIV$viral.load)
barplot(freq.viral.load[2:6],
        xlab = "Viral load",
        ylab = "Frequency (n)",
        col = "#808080",
        font.axis = 1)


freq.HIV.treat <- table(CC.HIV$duration.HIV.medication)
freq.HIV.treat
#barplot(freq.HIV.treat[2:7])
pie(freq.HIV.treat[2:6], radius = 2)


#########  Cervical lesion prevalence 
CC.HIV <- read.csv("ethel.csv", header = TRUE, sep = ",")

freq.vulva <- table(CC.HIV$vulva.inspection)
freq.vulva

freq.vagina <- table(CC.HIV$vaginal.inspection)
freq.vagina 


freq.colposcopy <- table(CC.HIV$COLPOSCOPY.findings)
freq.colposcopy
barplot(freq.colposcopy,
        #xlab = "Viral load",
        ylab = "Frequency (n)",
        col = "#808080",
        font.axis = 1)

freq.lesion <- table(CC.HIV$LESION.size)
freq.lesion
barplot(freq.lesion[2:5],
        #xlab = "Viral load",
        ylab = "Frequency (n)",
        col = "#808080",
        font.axis = 1)





######  Risk factors associated with the development of cervical lesion among HIV positive women
CC.HIV <- read.csv("ethel.csv", header = TRUE, sep = ",")
#
CC.HIV$cat.age <- as.factor(CC.HIV$cat.age)
CC.HIV$Lesion <- as.factor(CC.HIV$Lesion)
CC.HIV$cat.age <- as.factor(CC.HIV$cat.age)
CC.HIV$cat.hse.no <- as.factor(CC.HIV$cat.hse.no)
CC.HIV$religion <- as.factor(CC.HIV$religion)
CC.HIV$cat.edu <- as.factor(CC.HIV$cat.edu)
CC.HIV$cat.occupation <- as.factor(CC.HIV$cat.occupation)
CC.HIV$cat.income.month <- as.factor(CC.HIV$cat.income.month)
CC.HIV$cat.marital <- as.factor(CC.HIV$cat.marital)
CC.HIV$add.wives <- as.factor(CC.HIV$add.wives)


table(CC.HIV$cat.marital)


names(CC.HIV)
str(CC.HIV$Lesion)
str(CC.HIV$cat.edu)


demo.glm <- glm(Lesion~ 
                   cat.age+
                   cat.hse.no+
                   cat.religion+
                   cat.edu+
                   cat.occupation+
                   cat.income.month+
                   cat.marital,
                   #add.wives,
                 data = CC.HIV,
                 family = binomial(link = 'logit'))

summary(demo.glm)
exp(cbind(OR = coef(demo.glm), confint(demo.glm)))




######  Risk factors associated with the development of cervical lesion among HIV positive women
CC.HIV <- read.csv("ethel.csv", header = TRUE, sep = ",")
#

CC.HIV$Lesion <- as.factor(CC.HIV$Lesion)
CC.HIV$cat.age <- as.factor(CC.HIV$cat.age)
CC.HIV$cat.edu <- as.factor(CC.HIV$cat.edu)
CC.HIV$cat.occupation <- as.factor(CC.HIV$cat.occupation)
CC.HIV$cat.marital <- as.factor(CC.HIV$cat.marital)
CC.HIV$cat.income.month <- as.factor(CC.HIV$cat.income.month)
CC.HIV$cat.Currently.on.Contraceptive <- as.factor(CC.HIV$cat.Currently.on.Contraceptive)
CC.HIV$cat.viral.load <- as.factor(CC.HIV$cat.viral.load)
CC.HIV$cat.HPV.16 <- as.factor(CC.HIV$cat.HPV.16)
CC.HIV$cat.HPV.18 <- as.factor(CC.HIV$cat.HPV.18)
CC.HIV$cat.OTHER.HIGH.RISK.HPV <- as.factor(CC.HIV$cat.OTHER.HIGH.RISK.HPV)
CC.HIV$cat.Treatment.given <- as.factor(CC.HIV$cat.Treatment.given)
CC.HIV$cat.duration.HIV.medication <- as.factor(CC.HIV$cat.duration.HIV.medication)
CC.HIV$cat.vulva.inspection <- as.factor(CC.HIV$cat.vulva.inspection)
CC.HIV$cat.vaginal.inspection <- as.factor(CC.HIV$cat.vaginal.inspection)
CC.HIV$cat.cervical.inspection <- as.factor(CC.HIV$cat.cervical.inspection)
CC.HIV$parity.children <- as.factor(CC.HIV$parity.children)
CC.HIV$parity.miscarry <- as.factor(CC.HIV$parity.miscarry)
CC.HIV$menarchy <- as.factor(CC.HIV$menarchy)
CC.HIV$cat.HIV.duration <- as.factor(CC.HIV$cat.HIV.duration)


##### package to save output as .csv
#install.packages("writexl")
#library(writexl)

all.glm <- glm(Lesion~ 
                 #cat.age+
                 #cat.edu+
                 #cat.occupation+
                 cat.marital,
                 #cat.income.month+
                 #parity.children+
                 #parity.miscarry+
                 #menarchy+
                 #t.Currently.on.Contraceptive+
                 #cat.past.history.of.contraceptive+
                 #cat.viral.load+
                 #cat.HPV.16+
                 #cat.HPV.18+
                 #cat.OTHER.HIGH.RISK.HPV+
                 #cat.Treatment.given+
                 #cat.HIV.duration+
                 #cat.duration.HIV.medication+
                 #cat.vulva.inspection+
                 #cat.vaginal.inspection+
                 #cat.cervical.inspection,
               data = CC.HIV,
               family = binomial(link = 'logit'))

summary(all.glm)

OR.all <- exp(cbind(OR = coef(all.glm), confint(all.glm)))
OR.all
write_xlsx(as.data.frame(OR.all), "OR.xlsx")



###### Multi-variate 

all.var.glm <- glm(Lesion~ 
                 cat.age+
                 cat.viral.load+
                 cat.HPV.16+
                 cat.HPV.18+
                 cat.HIV.duration+
                 #cat.duration.HIV.medication+
                 cat.vulva.inspection,
                 #cat.cervical.inspection
               data = CC.HIV,
               family = binomial(link = 'logit'))

summary(all.var.glm)

OR.all <- exp(cbind(OR = coef(all.var.glm), confint(all.var.glm)))
OR.all
write_xlsx(as.data.frame(OR.all), "OR.xlsx")

confint(all.var.glm)
str(CC.HIV$cat.HIV.duration)




######## no.CERVICAL.QUADRANT.LESION.COVERED
quadrant.cover <- table(CC.HIV$no.CERVICAL.QUADRANT.LESION.COVERED)
quadrant.cover
barplot(quadrant.cover,
        ylab = "Frequency (n)",
        xlab = "Number of quadrants the cervical lesions covers",
        col = "#808080",
        font.axis = 1)









