lfs <- read.csv("data.csv")
summary(lfs)

cond <- lfs$ wages>1300000 | lfs$ wages<10000 | lfs$ nocs>11 | lfs$ naics>20 

lfs_wk <- lfs[!cond,]
summary(lfs_wk)

#Creating dummy
lfs_wk$city <- ifelse(lfs_wk$cma==421 | lfs_wk$cma==462 | lfs_wk$cma==462 | lfs_wk$cma==505 | lfs_wk$cma==535,1,0)

lfs_wk$married <- ifelse(lfs_wk$marsth==2,1,0)

lfs_wk$hschool <- ifelse(lfs_wk$hdgree==2,1,0)
lfs_wk$college <- ifelse(lfs_wk$hdgree %in% 3:7, 1,0)
lfs_wk$university <- ifelse(lfs_wk$hdgree %in% 8:9, 1,0)
lfs_wk$pgraduate <- ifelse(lfs_wk$hdgree %in% 10:13, 1,0)

lfs_wk$UNIL_ENGLISH <- ifelse(lfs_wk$mtnen==1 & lfs_wk$kol==1 & lfs_wk$lwaen==1 & lfs_wk$lwbfr==0, 1,0)
lfs_wk$BIL_MEFE <- ifelse(lfs_wk$mtnen==1 & lfs_wk$kol==3 & lfs_wk$lwaen==1 & lfs_wk$lwbfr==0, 1,0)
lfs_wk$BIL_MEFF <- ifelse(lfs_wk$mtnen==1 & lfs_wk$kol==3 & lfs_wk$lwaen==1 & lfs_wk$lwbfr==1, 1,0)
lfs_wk$BIL_FRENCH <- ifelse(lfs_wk$mtnen==1 & lfs_wk$kol==3 & lfs_wk$lwafr==1  & lfs_wk$lwben==1, 1,0)

lfs_wk$management <- ifelse(lfs_wk$nocs==1 | lfs_wk$nocs==2, 1,0 )
lfs_wk$professional <- ifelse(lfs_wk$nocs %in% 3:5, 1,0)
lfs_wk$smprofessional <- ifelse(lfs_wk$nocs %in% 6:7, 1,0)
lfs_wk$bluecollar <- ifelse(lfs_wk$nocs %in% 8:10, 1,0)

lfs_wk$public <- ifelse(lfs_wk$naics== 3 | lfs_wk$naics== 14 | lfs_wk$naics== 15 | lfs_wk$naics== 19, 1,0)
lfs_wk$goods <- ifelse(lfs_wk$naics %in% 1:2 | lfs_wk$naics %in% 4:8, 1,0)
lfs_wk$service <- ifelse(lfs_wk$naics %in% 9:13 | lfs_wk$naics %in% 16:18, 1,0)

lfs_new <- data.frame( lfs_wk$wages, lfs_wk$city, lfs_wk$age, lfs_wk$married, lfs_wk$hscool, lfs_wk$sex, lfs_wk$college, lfs_wk$university, lfs_wk$pgraduate,
                       lfs_wk$UNIL_ENGLISH, lfs_wk$BIL_MEFE, lfs_wk$BIL_MEFF, lfs_wk$BIL_FRENCH, lfs_wk$management,
                       lfs_wk$professional, lfs_wk$smprofessional, lfs_wk$bluecollar, lfs_wk$public, lfs_wk$goods, lfs_wk$service)

lfs_wk$learnings <- log(lfs_wk$wages)
lfs_wk$married <- ifelse(lfs_wk$marsth==1,1,0)
lfs_wk$age2 <- lfs_wk$agegrp^2

#split the data and calculate sample statistics
temp <- split(lfs_wk,lfs_wk$sex)
female <- temp[[1]]
male <- temp[[2]]

stats <- rbind(
  unlist(lapply(male,function(x) mean(x))),
  unlist(lapply(male,function(x) sd(x))),
  unlist(lapply(female,function(x) mean(x))),
  unlist(lapply(female,function(x) sd(x)))
)

#Split the data
lfs_wk$sex <- ifelse(lfs_wk$sex == 1, "female", "male")
lfs_mf <- split(lfs_wk, lfs_wk$sex)

unlist(lapply(lfs_mf, function(x) mean(x$wages)))
unlist(lapply(lfs_mf, function(x) mean(x$city)))
unlist(lapply(lfs_mf, function(x) mean(x$agegrp)))
unlist(lapply(lfs_mf, function(x) mean(x$married)))
unlist(lapply(lfs_mf, function(x) mean(x$hschool)))
unlist(lapply(lfs_mf, function(x) mean(x$college)))
unlist(lapply(lfs_mf, function(x) mean(x$university)))
unlist(lapply(lfs_mf, function(x) mean(x$pgraduate)))
unlist(lapply(lfs_mf, function(x) mean(x$UNIL_ENGLISH)))
unlist(lapply(lfs_mf, function(x) mean(x$BIL_MEFE)))
unlist(lapply(lfs_mf, function(x) mean(x$BIL_MEFF)))
unlist(lapply(lfs_mf, function(x) mean(x$BIL_FRENCH)))
unlist(lapply(lfs_mf, function(x) mean(x$management)))
unlist(lapply(lfs_mf, function(x) mean(x$professional)))
unlist(lapply(lfs_mf, function(x) mean(x$smprofessional)))
unlist(lapply(lfs_mf, function(x) mean(x$bluecollar)))
unlist(lapply(lfs_mf, function(x) mean(x$public)))
unlist(lapply(lfs_mf, function(x) mean(x$goods)))
unlist(lapply(lfs_mf, function(x) mean(x$service)))

unlist(lapply(lfs_mf, function(x) sd(x$wages)))
unlist(lapply(lfs_mf, function(x) sd(x$city)))
unlist(lapply(lfs_mf, function(x) sd(x$agegrp)))
unlist(lapply(lfs_mf, function(x) sd(x$married)))
unlist(lapply(lfs_mf, function(x) sd(x$hschool)))
unlist(lapply(lfs_mf, function(x) sd(x$college)))
unlist(lapply(lfs_mf, function(x) sd(x$university)))
unlist(lapply(lfs_mf, function(x) sd(x$pgraduate)))
unlist(lapply(lfs_mf, function(x) sd(x$UNIL_ENGLISH)))
unlist(lapply(lfs_mf, function(x) sd(x$BIL_MEFE)))
unlist(lapply(lfs_mf, function(x) sd(x$BIL_MEFF)))
unlist(lapply(lfs_mf, function(x) sd(x$BIL_FRENCH)))
unlist(lapply(lfs_mf, function(x) sd(x$management)))
unlist(lapply(lfs_mf, function(x) sd(x$professional)))
unlist(lapply(lfs_mf, function(x) sd(x$smprofessional)))
unlist(lapply(lfs_mf, function(x) sd(x$bluecollar)))
unlist(lapply(lfs_mf, function(x) sd(x$public)))
unlist(lapply(lfs_mf, function(x) sd(x$goods)))
unlist(lapply(lfs_mf, function(x) sd(x$service)))


stats <- rbind(
  unlist(lapply(male,function(x) mean(x))),
  unlist(lapply(male,function(x) sd(x))),
  unlist(lapply(female,function(x) mean(x))),
  unlist(lapply(female,function(x) sd(x)))
  )
#sample distribution
lfs_on <- aggregate(wages ~ sex + UNIL_ENGLISH + BIL_MEFE + BIL_MEFF +BIL_FRENCH, data= lfs_wk, FUN=mean)


#Count dummies
library(plyr)
count(lfs_wk$UNIL_ENGLISH, vars=NULL, wt_var=NULL)
count(lfs_wk$BIL_MEFE, vars=NULL, wt_var=NULL)
count(lfs_wk$BIL_MEFF, vars=NULL, wt_var=NULL)
count(lfs_wk$BIL_FRENCH, vars=NULL, wt_var=NULL)

count(lfs_wk$sex)

#log-linear regression

fw <- female$weight
mw <- male$weight

Male2 <- lm(learnings ~  married + age2 + agegrp + college + university + pgraduate + BIL_MEFE + BIL_MEFF + BIL_FRENCH +  professional + smprofessional + bluecollar + goods + service, data=male, weights = mw)
Female2 <- lm(learnings ~  married + age2 + agegrp + college + university + pgraduate + BIL_MEFE + BIL_MEFF + BIL_FRENCH +  professional + smprofessional + bluecollar + goods + service, data=female, weights = fw)

rslts <- list(M1, F1, M2, F2, M3, F3, M4, F4)
sink("regression.txt")
lapply(rslts, function(x) print(summary(x)))
sink()

rslts2 <- list(Male2, Female2)
sink("regression2.txt")
lapply(rslts2, function(x) print(summary(x)))
sink()

# excel 
install.packages("psych")
library(psych)
describeBy(lfs_wk, group="sex")


write.csv(lfs_wk, "excel.csv")

#comparison of means
lfs_edu <- aggregate(wages ~ sex + UNIL_ENGLISH + BIL_MEFE + BIL_MEFF + BIL_FRENCH, data=lfs_wk, FUN=mean)
