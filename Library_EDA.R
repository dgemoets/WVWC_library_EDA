library(dplyr)
library(ggplot2)
library(xtable)
dat <- read.csv(file="Room_Use_Fall_2016_DG_for_R.csv", header=TRUE)
dat$date <- as.Date(dat$Date,"%m/%d/%Y")

dat$day <- weekdays(dat$date)
dat$factday <- factor(dat$day,levels=c("Monday","Tuesday","Wednesday",
                                       "Thursday","Friday","Saturday","Sunday"))
dat$factmonth <- factor(dat$Month,levels=c("7","8","9","10","11","12"))
## To create date column from month day year columns
##temp <- paste(dat$Month,'/',dat$Day,'/',dat$Year)
##temp.date <- as.Date(temp,"%m / %d / %Y")    ## need spaces

names(dat)
summary(dat$Month);summary(dat$Day);summary(dat$Year)
##summary(dat$Room);summary(dat$Event);
summary(dat$Event_Cat1);summary(dat$Event_Cat2)
summary(dat$Name);summary(dat$factday)

## Room by day of the week
x <- table(dat$factday,dat$Room)##, data=dat)

## Room by event category
table(dat$Event_Cat1,dat$Room)##, data=dat)
table(dat$Event_Cat2,dat$Room)##, data=dat)

## events by month as given in Totals for Fall 2016
table(dat$Room,dat$factmonth)
##summary(subset(dat,Month==8)$Room)
xtabs(Count~Room+factmonth, data=dat)
xtabs(Count~Event_Cat1+factmonth, data=dat)


## old code.  Ignore below
################################################################
################################################################
################################################################
mult.grades <- names(which(summary(dat.grades$child.Initials)==1))  


dat.grades.multi <- subset(dat.grades, !(child.Initials %in% mult.grades))
dim(distinct(dat.grades.multi,child.Initials))  ## from 84 to 72 kids with score>1
                                           
plot(Average.Grade~date, data=dat.grades.multi, main="Average Grade vs Date for all kids")
##dev.copy2pdf(file="gradevsdate.pdf")
##dev.off()
plot(Days.Absent~date, data=dat.grades.multi, main="Days Absent vs Date for all kids" )
##dev.copy2pdf(file="absentvsdate.pdf")
##dev.off()
## find kids with decrease in absences and increase grade averages
grades.smry.stats <- absent.smry.stats <- data.frame(1:84,-99,-99,-99,-99)
names(grades.smry.stats) <-  c("std.dev","max.diff","ID","num.obs","sigma")
grades.smry.stats$range <- grades.smry.stats$slope <- 0

names(absent.smry.stats) <-  c("std.dev","max.diff","ID","num.obs","sigma")
absent.smry.stats$range <- absent.smry.stats$slope <- 0

for (i in 1:dim(distinct(dat.grades,child.Initials))[1]){ ## go through all 89 kids
    temp <- levels(dat.grades$child.Initials)[i]
    temp1 <- subset(dat.grades,child.Initials==temp)
    n <- dim(temp1)[1]  ## number of grades
    if(n>1){  ## more than one grades
        temp2 <- range(temp1$Average.Grade,na.rm=TRUE)
        tmp.first <- ifelse(is.na(temp1$Average.Grade[1]),temp1$Average.Grade[2],temp1$Average.Grade[1]) 
        tmp.last <- ifelse(is.na(temp1$Average.Grade[n]),temp1$Average.Grade[n-1],temp1$Average.Grade[n])
        grades.smry.stats$range[i] <- tmp.first-tmp.last
        grades.smry.stats$std.dev[i] <- sd(temp1$Average.Grade,na.rm=TRUE)
        grades.smry.stats$max.diff[i] <- temp2[2]-temp2[1]
        grades.smry.stats$ID[i] <- temp
        grades.smry.stats$num.obs[i] <- n
        temp.lm <- lm(Average.Grade~date, data=temp1)
        grades.smry.stats$slope[i] <- coef(temp.lm)[2]
        grades.smry.stats$sigma[i] <- summary(temp.lm)$sigma
        temp2 <- range(temp1$Days.Absent,na.rm=TRUE)
        tmp.first <- ifelse(is.na(temp1$Days.Absent[1]),temp1$Days.Absent[2],temp1$Days.Absent[1]) 
        tmp.last <- ifelse(is.na(temp1$Days.Absent[n]),temp1$Days.Absent[n-1],temp1$Days.Absent[n])
        absent.smry.stats$range[i] <- tmp.first-tmp.last
        absent.smry.stats$std.dev[i] <- sd(temp1$Days.Absent,na.rm=TRUE)
        absent.smry.stats$max.diff[i] <- temp2[2]-temp2[1]
        absent.smry.stats$ID[i] <- temp
        absent.smry.stats$num.obs[i] <- n
        ##temp.lm <- lm(Days.Absent~date, data=temp1)
        ##absent.smry.stats$slope[i] <- coef(temp.lm)[2]
        ##absent.smry.stats$sigma[i] <- summary(temp.lm)$sigma
    } else {
        grades.smry.stats$num.obs[i] <- 1
        absent.smry.stats$num.obs[i] <- 1
        }


}

## subset of contact list corresponding to greatest score increase
## positive increases
grades.smry.stats$total.contact <- 0
absent.smry.stats$total.contact <- 0

for (i in 1:dim(grades.smry.stats)[1]){
    if (grades.smry.stats$num.obs[i]!=1){  ## if more than one observation
        temp <- smry.stats$ID[i]
        temp1 <- subset(dat.contacts,Identifier==temp)
        grades.smry.stats$total.contact[i] <- sum(temp1$Duration.in.Minutes,na.rm=TRUE)
        absent.smry.stats$total.contact[i] <- sum(temp1$Duration.in.Minutes,na.rm=TRUE)
    }
    }
## remove kids with only one score
tempp <- subset(grades.smry.stats,num.obs!=1 & slope>0)
##subset(dat.contacts,Identifier=="mi tu")
subset(dat.grades,child.Initials=="ca qu")

plot(slope~total.contact,data=tempp, main="Grade Average Trend (Std. Dev.) vs. Total contact hours",
     xlab="Total contact hours",ylab="Std. Deviation over time of Grade Average")
abline(a=0,b=0)
dev.copy2pdf(file="stddev.vs.contacts.pdf")
dev.off()


temp.pos.slope <- subset(dat.grades, (child.Initials %in% tempp$ID))
ggplot(temp.pos.slope, aes(x = date, y = Average.Grade, colour = child.Initials)) + geom_line()
##temp <- levels(dat.score$Identifier)[19]
temp1 <- subset(dat.score,Identifier=='al cr')
plot(Total.Problems~date,temp1)
subset(dat.contacts,Identifier=='al cr')

## find kids with increasing grades
g <- lm(Average.Grade~date, subset(dat.grades,Identifier=='da an'))
## number of distinct patients
sort(distinct(dat.score,Identifier))
dim(distinct(dat.score,Identifier))

## plot of total contact time and diff in test score
## total contact time
xx <- summarize(group_by(dat.contacts,Identifier),total=sum(Duration.in.Minutes))
## diff in total achenbach
diff.achen <- dfd

## pick a few kids to see trends

temp <- levels(dat.score$Identifier)[19]
temp1 <- subset(dat.score,Identifier==temp)
plot(Total.Problems~date,temp1)

## sort by Identifyer
dat.score.sort <- arrange(dat.score, desc(Identifier))
dat.contacts.sort <- arrange(dat.contacts, desc(Identifier))
names(dat.contacts)
head(dat.contacts)

levels(dat.score$Identifier)
##for (i in 1:dim(distinct(dat.score,Identifier))[1]){
i <- 4  
temp <- levels(dat.score$Identifier)[1]
  temp1 <- subset(dat.score,Identifier==temp)
  temp2 <- subset(dat.contacts,Identifier==temp)
  
##}
summary(dat.contacts$Number.of.times.contacted)  ## all values are 1
summary(dat.contacts$Number.of.contacts)  ##
summary(dat.contacts$Medicaid.Billed)  ## all values are 0


## looked at lots of plots
plot(Total.Problems~date,data=dat.score)

newdata <- subset(, age >= 20 | age < 10, 
                  select=c(ID, Weight))

################################################################
## from first analysis
## active scores for teachers
dat.teach.active <- subset(dat.teach,Active.==1)
summary(dat.teach.active$Achenbach_2-dat.teach.active$Achenbach_1)
wilcox.test(dat.teach.active$Achenbach_2,dat.teach.active$Achenbach_1, paired=TRUE,conf.int = TRUE, conf.level = 0.95,alternative="less")##,correct=FALSE)
##t.test(dat.teach.active$Achenbach_1,dat.teach.active$Achenbach_2,paired=TRUE)
wilcox.test(dat.teach.active$Achenbach_3,dat.teach.active$Achenbach_1, paired=TRUE,conf.int = TRUE, conf.level = 0.95,alternative="less")##,correct=FALSE)
##t.test(dat.teach.active$Achenbach_3,dat.teach.active$Achenbach_1,paired=TRUE,alternative="less")
sum(is.na(dat.teach.inactive$Achenbach_3-dat.teach.inactive$Achenbach_1))

## inactive scores for teachers
dat.teach.inactive<- subset(dat.teach,Active.==0)
summary(dat.teach.inactive$Achenbach_2-dat.teach.inactive$Achenbach_1)
wilcox.test(dat.teach.inactive$Achenbach_2,dat.teach.inactive$Achenbach_1, paired=TRUE,conf.int = TRUE, conf.level = 0.95,alternative="less")##,correct=FALSE)
##t.test(dat.teach.inactive$Achenbach_2,dat.teach.inactive$Achenbach_1,paired=TRUE,alternative="less")
wilcox.test(dat.teach.inactive$Achenbach_3,dat.teach.inactive$Achenbach_1, paired=TRUE,conf.int = TRUE, conf.level = 0.95,alternative="less")##,correct=FALSE)
##t.test(dat.teach.inactive$Achenbach_3,dat.teach.inactive$Achenbach_1,paired=TRUE,alternative="less")

## for parents
summary(dat.parent$Second_Achenbach-dat.parent$First_Achenbach)
summary(dat.parent$Achenbach_2)
sum(is.na(dat.parent.inactive$Second_Achenbach-dat.parent.inactive$First_Achenbach))
wilcox.test(dat.parent$Second_Achenbach,dat.parent$First_Achenbach, paired=TRUE,conf.int = TRUE, conf.level = 0.95,alternative="less")##,correct=FALSE)
## active scores
dat.parent.active <- subset(dat.parent,prog_connection=="Active")
summary(dat.parent.active$Second_Achenbach-dat.parent.active$First_Achenbach)
wilcox.test(dat.parent.active$Second_Achenbach,dat.parent.active$First_Achenbach, paired=TRUE,conf.int = TRUE, conf.level = 0.95,alternative="less")##,correct=FALSE)
##t.test(dat.parent.active$Second_Achenbach,dat.parent.active$First_Achenbach,paired=TRUE,alternative="less")

## inactive scores
dat.parent.inactive <- subset(dat.parent,prog_connection!="Active")
summary(dat.parent.inactive$Second_Achenbach-dat.parent.inactive$First_Achenbach)
wilcox.test(dat.parent.inactive$Second_Achenbach,dat.parent.inactive$First_Achenbach, paired=TRUE,conf.int = TRUE, conf.level = 0.95,alternative="less")##,correct=FALSE)
##t.test(dat.parent.inactive$Second_Achenbach,dat.parent.inactive$First_Achenbach,paired=TRUE,alternative="less")


################################################################
## old work below
frst <- dat$First..Achenbach.Score.Tot
scnd <- dat$Most.Recent.Ach.Score.Tot.1
thrd <- dat$Achenbach..2.Tot
chng1 <- dat$CHANGE.SCORE..1
chng2 <- dat$X.2.Change.from.first
wilcox.test(frst,scnd, paired=TRUE,conf.int = TRUE, conf.level = 0.95,correct=FALSE)
wilcox.test(chng1, conf.int = TRUE, conf.level = 0.95,correct=FALSE)
##alternative = c("two.sided", "less", "greater", mu = 0, paired = FALSE, exact = NULL, correct = TRUE)
t.test(frst,scnd)
t.test(chng1)

wilcox.test(frst,thrd, paired=TRUE,conf.int = TRUE, conf.level = 0.95,correct=FALSE)
wilcox.test(chng2, conf.int = TRUE, conf.level = 0.95,correct=TRUE)

##alternative = c("two.sided", "less", "greater", mu = 0, paired = FALSE, exact = NULL, correct = TRUE)
t.test(dat$CHANGE.SCORE..1)

## are the zeros missing?
