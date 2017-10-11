library(readxl)

OSA=read_excel(
  "C:/MSTC_BD/R/WORKING/DATASETS/APNEA/OSA_DB_UPM.xlsx",na="-1")
fix(OSA)
dim(OSA)

OSA=na.omit(OSA)
dim(OSA)
names(OSA)

attach(OSA)


lm.fit=lm(IAH~Weight+Height+Cervical+Age)


OSA_male=subset(OSA, Gender==0)

# Another way
# OSA_male = OSA[OSA$Gender == 0, ]

names(OSA_male)
attach(OSA_male)

lm_male.fit=lm(IAH~Height+Weight+Cervical+Age)

summary(lm_male.fit)

plot(predict(lm_male.fit), residuals(lm_male.fit))

hist(sqrt(IAH))

lm_male2.fit=lm(sqrt(IAH)~Height+Weight+Cervical+Age)

summary(lm_male2.fit)

plot(predict(lm_male2.fit), residuals(lm_male2.fit))
plot(predict(lm_male2.fit), rstudent(lm_male2.fit))

OSA_female=subset(OSA, Gender==1)

names(OSA_female)
attach(OSA_female)

lm_female.fit=lm(IAH~Weight+Height+Cervical+Age)

summary(lm_female.fit)





