###########################################
#####    Getting to Know your Data    #####
###########################################

# to remove objects from workspace
rm(list=ls())
# garbage collection good for more efficent memory storage
gc()

### PRE-PROCESSING THE INFORMATION AND GETTING TO 
### KNOW YOUR DATA

# We take a look at 3 data sets. All data sets and the 
# R programs for all examples in this book are listed 
# on the webpage that accompanies this book 
# (http://www.biz.uiowa.edu/faculty/jledolter/DataMining).

# Example 1: 2006 Birth Data Is 2006 birth data set that 
# is used in the book R In a Nutshell: A Desktop Quick
# Reference (Adler, 2009). The data set births2006.smpl 
# consists of 427,323 records and 13 variables, including 
# the day of birth according to the month and the day of
# week (DOB_MM, DOB_WK), the birth weight of the baby 
# (DBWT) and the weight gain of the mother during pregnancy
# (WTGAIN), the sex of the baby and its APGAR score at 
# birth (SEX and APGAR5), whether it was a single or
# multiple birth (DPLURAL), and the estimated gestation
# age in weeks (ESTGEST).

# Information on first five births:
## Install packages from CRAN; use any USA mirror.
# We use trellis graphics and lattice to condition density
# histograms on values of a third variable (multiple births,
# and method of delivery). We have separate histograms
# for birth weight by these variables.

library(lattice)  # for visualization

library(nutshell.bbdb)
library(nutshell.audioscrobbler)
library(nutshell) 


data(births2006.smpl)
births2006.smpl[1:5,]
# Shows number of births and number of variables,
# that is, number of rows and columns in the data:
dim(births2006.smpl)

# data for bar charts of frequency of births:
births.dow=table(births2006.smpl$DOB_WK)
births.dow 
barchart(births.dow,ylab="Day of Week",col="red")
## for color, use col="red" or omit the color argument
dob.dm.tbl=table(WK=births2006.smpl$DOB_WK,
                 MM=births2006.smpl$DMETH_REC)
dob.dm.tbl
dob.dm.tbl=dob.dm.tbl[,-2]
dob.dm.tbl
trellis.device()
barchart(dob.dm.tbl,ylab="Day of Week")
barchart(dob.dm.tbl,horizontal=FALSE,
         groups=FALSE,xlab="Day of Week")#,col="black")
## for color, omit the color argument

# separate histograms:
histogram(~DBWT|DPLURAL,data=births2006.smpl,
          layout=c(1,5))#,col="black")
histogram(~DBWT|DMETH_REC,data=births2006.smpl,
          layout=c(1,3))#,col="black")

# birth weight decreases with multiple births, but birth
# weight is largely unaffected by the method of delivery.

# Smoothed versions of the histograms, using the
# lattice command density plot, are also shown. 
densityplot(~DBWT|DPLURAL,data=births2006.smpl,
            layout=c(1,5),plot.points=FALSE)#,col="black")
densityplot(~DBWT,groups=DPLURAL,data=births2006.smpl,
            plot.points=FALSE)

# Because of the very small sample sizes for quintuplet
# and even more births, the density of birth weight for
# this small group is quite noisy.

# Dot plot shows quite clearly that there are only few
# observations in that last group, while most other 
# groups have many observations:
dotplot(~DBWT|DPLURAL,data=births2006.smpl,
        layout=c(1,5),plot.points=FALSE)#,col="black")

# Scatter plots are shown for birth weight against
# weight gain, and the scatter plots are stratified 
# further by multiple births.

# birth weight by day of the week:
xyplot(DBWT~DOB_WK,data=births2006.smpl)#,col="black")

# here is conditioned on plurality of births:
xyplot(DBWT~DOB_WK|DPLURAL,data=births2006.smpl,
       layout=c(1,5))#,col="black")

xyplot(DBWT~DOB_WK,data=births2006.smpl)


# birth weight by weight gain:
xyplot(DBWT~WTGAIN,data=births2006.smpl)#,col="black")

# conditioned on plurality:
xyplot(DBWT~WTGAIN|DPLURAL,data=births2006.smpl,
       layout=c(1,5))#,col="black")

# The last smoothed scatter plot shows there is little 
# association between birth weight and weight gain
# during the course of the pregnancy.
smoothScatter(births2006.smpl$WTGAIN,
              births2006.smpl$DBWT)

# We show box plots of birth weight against the APGAR 
# score and box plots of birth weight against the day 
# of week of delivery. We do not expect a relationship
# between the birth weight and the day of week of
# delivery. 

# The APGAR score is an indication of the health status
# of a newborn, with low scores indicating that the 
# newborn has problems.

# The box plot of birth weight against the APGAR score
# shows a strong relationship. Babies of low birth 
# weight often have low APGAR scores as their health
# is compromised by the low birth weight and its 
# associated complications.

## boxplot is the command for a box plot in standard graphics
## package
boxplot(DBWT~APGAR5,data=births2006.smpl,
        ylab="DBWT",xlab="AGPAR5")
boxplot(DBWT~DOB_WK,data=births2006.smpl,
        ylab="DBWT",xlab="Day of Week")

## bwplot is the command for a box plot in the lattice graphics
## package. There you need to declare the conditioning variables as 
## factors 
bwplot(DBWT~factor(APGAR5)|factor(SEX),
       data=births2006.smpl,xlab="AGPAR5")
bwplot(DBWT~factor(DOB_WK),data=births2006.smpl,
       xlab="Day of Week")

# We calculate the average birth weight as function of 
# multiple births, and we do this for males and females
# separately. For that we use the tapply function.

# Note missing observations in the data set and the 
# option na.rm=TRUE (means remove missing observations 
# from the calculation) is needed to omit the missing
# observations from the calculation of the mean. 

# The bar plot illustrates graphically how average birth
# weight decreases with multiple deliveries. It also 
# shows that average birth weight for males is slightly
# higher than that for females.
help(factor)
fac=factor(births2006.smpl$DPLURAL)
res=births2006.smpl$DBWT
t4=tapply(res,fac,mean,na.rm=TRUE)
t4
t5=tapply(births2006.smpl$DBWT,
          INDEX=list(births2006.smpl$DPLURAL,
                     births2006.smpl$SEX),
          FUN=mean,na.rm=TRUE)
t5
barplot(t4,ylab="DBWT")
barplot(t5,beside=TRUE,ylab="DBWT")

# Here we illustrate the levelplot and the contourplot 
# from the R package lattice. For these plots we first 
# create a cross-classification of weight gain and 
# estimated gestation period by dividing the two continuous 
# variables into 11 nonoverlapping groups.

# For each of the resulting groups, we compute average 
# birth weight. An earlier frequency distribution table
# of estimated gestation period indicates that "99" is 
# used as the code for "unknown". For the subsequent 
# calculations, we omit all records with unknown gestation
# period (i.e., value 99). The graphs show that birth weight 
# increases with the estimated gestation period, but 
# that birth weight is little affected by the weight 
# gain. Note that the contour lines are essentially
# horizontal and that their associated values increase 
# with the estimated gestation period.
t5=table(births2006.smpl$ESTGEST)
t5
new=births2006.smpl[births2006.smpl$ESTGEST != 99,]
t51=table(new$ESTGEST)
t51
t6=tapply(new$DBWT,
          INDEX=list(cut(new$WTGAIN,breaks=10),
                     cut(new$ESTGEST,breaks=10)),
          FUN=mean,na.rm=TRUE)
t6
levelplot(t6,scales = list(x = list(rot = 90)))
contourplot(t6,scales = list(x = list(rot = 90)))

# But what questions would we want to have answered with 
# these data? One may wish to predict the birth weight 
# from characteristics such as the estimated gestation 
# period and the weight gain of the mother; for that, 
# one could use regression and regression trees. Or, 
# one may want to identify births that lead to very
# low APGAR scores, for which purpose, one could use 
# classification methods.


### Example 2: Alumni Donations

# The file contribution.csv received by a selective private 
# liberal arts college in the Midwest. The college keeps 
# detailed records on alumni donations. We analyze the 
# contributions of five graduating classes (cohorts who 
# graduated in 1957, 1967, 1977, 1987, and 1997). The data
# consists of n = 1230 living alumni and contains their 
# contributions for the years 2000-2004. 

# Also, the data set includes several other variables such
# as gender, marital status, college major, subsequent 
# graduate work, and attendance at fund-raising events, 
# all variables that may play an important role in 
# assessing the success of future capital campaigns.

# The data contains no missing observations. The first 
# five records of the file are shown below. Alumni not 
# contributing have the entry "0" in the related column. 
# The 1957 cohort is the smallest group. This is because 
# of smaller class sizes in the past and deaths of
# older alumni.

## Install packages from CRAN; use any USA mirror 
library(lattice)
# "drill down" to the file in your stored course materials:
don <- read.csv(file.choose())
# view first five records:
don[1:5,]
# this is a tabulate function, it counts frequencies:
table(don$Class.Year)
# is an integer column but table() sees the integers
# as factors
class(don$Class.Year)
# create a barchart
barchart(table(don$Class.Year),horizontal=FALSE,
         xlab="Class Year",col="black")

# Total contributions for 2000-2004 are calculated for 
# each graduate. Summary statistics (mean, standard 
# deviation, and percentiles) are shown below. More than
# 30% of the alumni gave nothing; 90% gave $1050 or less; 
# and only 3% gave more than $5000. 

# The largest contribution was $172,000. The first histogram
# of total contributions shown below is not very informative
# as it is influenced by both a sizable number of the 
# alumni who have not contributed at all and a few alumni 
# who have given very large contributions. Omitting
# contributions that are zero or larger than $1000 provides 
# a more detailed view of contributions in the $1-$1000 
# range; this histogram is shown to the right of the
# first one. Box plots of total contributions are also 
# shown. The second box plot omits the information from
# outliers and shows the three quartiles of the distribution
# of total contributions (0, 75, and 400).

# add up the donations
don$TGiving=don$FY00Giving+don$FY01Giving+don$FY02Giving+don$FY03Giving+don$FY04Giving
# take a look, note it was adding columns:
don$TGiving
# take mean of total giving, is vectorized
# function so we get a single number
mean(don$TGiving)
# compute standard deviation of total giving,
# is very large, why is this?
sd(don$TGiving)
# quantile() provides a variety of summary information
# in 5% increments:
quantile(don$TGiving,probs=seq(0,1,0.05))
# in 1% increments starting at 95%....we see that
# one huge donation at the end
quantile(don$TGiving,probs=seq(0.95,1,0.01))
# finally, draw a histogram, is distorted because
# of large donation
hist(don$TGiving)
# look at donations up to $1,000 max:
hist(don$TGivin[don$TGiving!=0][don$TGiving[don$TGiving!=0]<=1000])

x=densityplot(don$TGivin[don$TGiving!=0][don$TGiving[don$TGiving!=0]<=1000])
x
str(x)

## or, if you want to achieve the above histogram slower in two steps
## ff1=don$TGiving[don$TGiving!=0]
## ff1
## ff2=ff1[ff1<=1000]
## ff2
## hist(ff2,main=paste("Histogram of TGivingTrunc"),xlab="TGivingTrunc")

boxplot(don$TGiving,horizontal=TRUE,
        xlab="Total Contribution")
boxplot(don$TGiving,outline=FALSE,
        horizontal=TRUE,xlab="Total Contribution")

# We identify below the donors who gave at least $30,000 
# during 2000-2004. We also list their major and their 
# next degree. The top donor has a mathematics-physics
# double major with no advanced degree. Four of the top 
# donors have law degrees.

ddd=don[don$TGiving>=30000,]
ddd
# selectively list columns 1 thru 5, and 12
ddd1=ddd[,c(1:5,12)]
ddd1
# put them in order, from highest to lowest
ddd1[order(ddd1$TGiving,decreasing=TRUE),]

# For a university foundation, it is important to know 
# who is contributing, as such information allows the 
# foundation to target their fund-raising resources to 
# those alumni who are most likely to donate. We show 
# below box plots of total 5-year donation for the 
# categories of class year, gender, marital status, 
# and attendance at a foundation event. 

# We have omitted in these graphs the outlying 
# observations (those donors who contribute generously).
# Targeting one's effort to high contributors involves 
# many personal characteristics that are not included 
# in this database (such as special information about
# personal income and allegiance to the college). 

# It may be a safer bet to look at the median amount 
# of donation that can be achieved from the various 
# groups. Class year certainly matters greatly;
# older alumni have access to higher life earnings, 
# while more recent graduates may not have the resources
# to contribute generously. Attendance at a foundation
# sponsored event certainly helps; this shows that it 
# is important to get alumni to attend such events. 

# This finding reminds the author about findings in 
# his consulting work with credit card companies: if 
# one wants someone to sign up for a credit card,
# card, one must first get that person to open up 
# the envelope and read the advertising message.


# We provide box plots of total giving against the 
# alumni's major and second degree. In these, we only
# consider those categories with frequencies exceeding
# a certain threshold (10); otherwise, we would have 
# to look at the information from too many groups
# with low frequencies of occurrence. Alumni with an
# economics/business major contribute most. Among 
# alumni with a second degree, MBAs and lawyers give
# the most.

boxplot(TGiving~Class.Year,data=don,outline=FALSE)
boxplot(TGiving~Gender,data=don,outline=FALSE)
boxplot(TGiving~Marital.Status,data=don,outline=FALSE)
boxplot(TGiving~AttendenceEvent,data=don,outline=FALSE)

# get mean by major:
t4=tapply(don$TGiving,don$Major,mean,na.rm=TRUE)
t4
# tabulate by major
t5=table(don$Major)
t5
# combine those two columns
t6=cbind(t4,t5)
t6
# get t6's where t6 in second column is more than 10
t7=t6[t6[,2]>10,]
t7
# put them in decreasing order
t7[order(t7[,1],decreasing=TRUE),]
# make a barchart out of this information
barchart(t7[,1],col="black")
# get mean of giving by those who have next degree
t4=tapply(don$TGiving,don$Next.Degree,mean,na.rm=TRUE)
# take a look
t4
# tabulate it
t5=table(don$Next.Degree)
# take a look
t5
# bind these two columns
t6=cbind(t4,t5)
# take a look
t6
# select by those who gave more than 10 times
t7=t6[t6[,2]>10,]
# put in decreasing order
t7[order(t7[,1],decreasing=TRUE),]
# make a barchart
barchart(t7[,1],col="black")

# A plot of histogram densities, stratified according
# to year of graduation, shows the distributions of 
# 5-year giving among alumni who gave $1-$1000. 

# It gives a more detailed description of the 
# distribution than the earlier histogram of all
# contributions.

densityplot(~TGiving|factor(Class.Year),
            data=don[don$TGiving<=1000,][don[don$TGiving<=1000,]$TGiving>0,],
            plot.points=FALSE,col="black")

# We now calculate the total of the 5-year donations 
# for the five graduation cohorts. We do this by using 
# the tapply function (applying the summation function
# to the total contributions of each of the graduation 
# classes). The result shows that the 1957 cohort has 
# contributed $560,000, compared to $35,000 of the 1997
# cohort.

t11=tapply(don$TGiving,don$Class.Year,
           FUN=sum,na.rm=TRUE)
t11
barplot(t11,ylab="Average Donation")

# Below we calculate the annual contributions (2000-2004)
# of the five graduation classes. The 5 bar charts are 
# drawn on the same scale to facilitate ready comparisons.
# The year 2001 was the best because of some very large
# contributions from the 1957 cohort.

barchart(tapply(don$FY04Giving,don$Class.Year,
                FUN=sum,na.rm=TRUE),horizontal=FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY03Giving,don$Class.Year,
                FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY02Giving,don$Class.Year,
                FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY01Giving,don$Class.Year,
                FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY00Giving,don$Class.Year,
                FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")

# Finally, we compute the numbers and proportions 
# of individuals who contributed.

# We do this by first creating an indicator variable 
# for total giving, and displaying the numbers of the 
# alumni who did and did not contribute. About 66%
# of all alumni contribute. The mosaic plot shows that
# the 1957 cohort has the largest proportion of 
# contributors; the 1997 cohort has the smallest 
# proportion of contributors, but includes the 
# largest number of individuals (the area of the bar
# in a mosaic plot expresses the size of the group). 

# The proportions of contributors shown indicate 
# that 75% of the 1957 cohort contributes, while 
# only 61% of the 1997 graduating class does so. 

# We can do the same analysis for each of the
# 5 years (2000-2004). The results for the most 
# recent year 2004 are also shown.

don$TGivingIND=cut(don$TGiving,
                   c(-1,0.5,10000000),
                   labels=FALSE)-1
# take a look
don$TGivingIND
# calculate mean
mean(don$TGivingIND)
# tabulate it
t5=table(don$TGivingIND,don$Class.Year)
# take a look
t5 

barplot(t5,beside=TRUE)
mosaicplot(factor(don$Class.Year)~factor(don$TGivingIND))
t50=tapply(don$TGivingIND,don$Class.Year,
           FUN=mean,na.rm=TRUE)
t50
barchart(t50,horizontal=FALSE,col="black")

don$FY04GivingIND=cut(don$FY04Giving,
                      c(-1,0.5,10000000),
                      labels=FALSE)-1
t51=tapply(don$FY04GivingIND,don$Class.Year,
           FUN=mean,na.rm=TRUE)
t51
barchart(t51,horizontal=FALSE,col="black")

# Below we explore the relationship between the alumni 
# contributions among the 5 years. For example, if we 
# know the amount an alumnus gives in one year
# (say in year 2000) does this give us information 
# about how much that person will give in 2001? 

# Pairwise correlations and scatter plots show that 
# donations in different years are closely related. 

# We use the command plotcorr in the package
# ellipse to express the strength of the correlation 
# through ellipse-like confidence regions.

Data=data.frame(don$FY04Giving,don$FY03Giving,
                don$FY02Giving,don$FY01Giving,
                don$FY00Giving)
correlation=cor(Data)
correlation
plot(Data)
library(ellipse)  
plotcorr(correlation)

# We conclude our analysis of the contribution data
# set with several mosaic plots that illustrate the
# relationships among categorical variables. The 
# proportion of alumni making a contribution is the 
# same for men and women. Married alumni are most
# likely to contribute, and the area of the bars 
# in the mosaic plot indicates that married alumni
# constitute the largest group. Alumni who have 
# attended an informational meeting are more likely
# to contribute, and more than half of all alumni
# have attended such a meeting. Separating the 
# alumni into groups who have and have not attended
# an informational meeting, we create mosaic plots 
# for giving and marital status. The likelihood of 
# giving increases with attendance, but the relative
# proportions of giving across the marital status 
# groups are fairly similar. This tells us that 
# there is a main effect of attendance, but that 
# there is not much of an interaction effect.

mosaicplot(factor(don$Gender)~factor(don$TGivingIND))
mosaicplot(factor(don$Marital.Status)~factor(don$TGivingIND))
t2=table(factor(don$Marital.Status),factor(don$TGivingIND))
mosaicplot(t2)
mosaicplot(factor(don$AttendenceEvent)~factor(don$TGivingIND))
t2=table(factor(don$Marital.Status),
         factor(don$TGivingIND),
         factor(don$AttendenceEvent))
t2
mosaicplot(t2[,,1])
mosaicplot(t2[,,2])




### Example 3: Orange Juice 

# This section analyzes the weekly sales data of 
# refrigerated 64-ounce orange juice containers 
# from 83 stores in the Chicago area. There are 
# many stores throughout the city, many time 
# periods, and also three different brands 
# (Dominicks, MinuteMaid, and Tropicana). 

# The data are arranged in rows, with each row 
# giving the recorded store sales (in logarithms; 
# logmove), as well as brand, price, presence/absence
# of feature advertisement, and the demographic 
# characteristics of the store. There are 28,947
# rows in this data set. The data is taken from P. 
# Rossi's bayesm package for R, and it has been used 
# earlier in Montgomery (1987).

# Time sequence plots of weekly sales, averaged over 
# all 83 stores, are shown for the three brands. 

# We create these plots by first obtaining the average 
# sales for a given week and brand (averaged over the 
# 83 stores). For this, we use the R function
# tapply. Time sequence plots of the averages are 
# then graphed for each brand, and the plots are 
# arranged on the same scale for easy comparison.

# An equivalent display, as three panels on the same 
# plotting page, is produced through the xyplot function
# of the lattice package. Box plots, histograms,
# and smoothed density plots for sales, stratified for 
# the three brands, are also shown.

# These displays average the information across the 
# 83 stores and the 121 weeks.

## Install packages from CRAN; use any USA mirror 
library(lattice)  
# drill down to oj.csv:
oj <- read.csv(file.choose())
oj$store <- factor(oj$store)
oj[1:2,]
t1=tapply(oj$logmove,oj$brand,
          FUN=mean,na.rm=TRUE)
t1
t2=tapply(oj$logmove,
          INDEX=list(oj$brand,oj$week),
          FUN=mean,na.rm=TRUE)
t2
plot(t2[1,],type= "l",xlab="week",
     ylab="dominicks",ylim=c(7,12))
plot(t2[2,],type= "l",xlab="week",
     ylab="minute.maid",ylim=c(7,12))
plot(t2[3,],type= "l",xlab="week",
     ylab="tropicana",ylim=c(7,12))
logmove=c(t2[1,],t2[2,],t2[3,])
week1=c(40:160)
week=c(week1,week1,week1)
brand1=rep(1,121)
brand2=rep(2,121)
brand3=rep(3,121)
brand=c(brand1,brand2,brand3)
xyplot(logmove~week|factor(brand),
       type= "l",layout=c(1,3),col="black")

boxplot(logmove~brand,data=oj)
histogram(~logmove|brand,data=oj,
          layout=c(1,3))
densityplot(~logmove|brand,data=oj,
            layout=c(1,3),plot.points=FALSE)
densityplot(~logmove,groups=brand,
            data=oj,plot.points=FALSE) 

# The previous displays ignore price and the presence
# of feature advertisement.

# Below we graph sales against price, and we do this 
# for each brand separately but aggregating over weeks
# and stores. The graph shows that sales decrease with
# increasing price. 

# A density plot of sales for weeks with and without 
# feature advertisement, and a scatter plot of sales 
# against price with the presence of feature
# advertisement indicated by the color of the plotting
# symbol both indicate the very positive effect of 
# feature advertisement.

xyplot(logmove~week,data=oj,col="black")
xyplot(logmove~week|brand,data=oj,
       layout=c(1,3),col="black")
xyplot(logmove~price,data=oj,col="black")
xyplot(logmove~price|brand,data=oj,
       layout=c(1,3),col="black")
smoothScatter(oj$price,oj$logmove)

densityplot(~logmove,groups=feat, 
            data=oj, plot.points=FALSE)
xyplot(logmove~price,groups=feat, data=oj)

# Next we consider one particular store. Time sequence 
# plots of the sales of store 5 are shown for the three
# brands. Scatter plots of sales against price, separately
# for the three brands, are also shown; sales decrease
# with increasing price. Density histograms of sales 
# and scatter plots of sales against price, with weeks
# with and without feature advertisement coded in color,
# are shown for each of the three brands. 

# These graphs show very clearly that feature advertisement
# increases the sales.

oj1=oj[oj$store == 5,]
xyplot(logmove~week|brand,data=oj1,type="l",
       layout=c(1,3),col="black")
xyplot(logmove~price,data=oj1,col="black")
xyplot(logmove~price|brand,data=oj1,
       layout=c(1,3),col="black")
densityplot(~logmove|brand,groups=feat,
            data=oj1,plot.points=FALSE)
xyplot(logmove~price|brand,groups=feat,
       data=oj1)

# The volume of the sales of a given store certainly 
# depends on the price that is charged and on the 
# feature advertisement that is being run. In addition,
# sales of a store may depend on the characteristics 
# of the store such as the income, age, and educational
# composition of its neighborhood. We may be interested
# in assessing whether the sensitivity (elasticity) of 
# the sales to changes in the price depends on the income
# of the customers who live in the store's neighborhood. 

# We may expect that the price elasticity is largest in
# poorer neighborhoods as poorer customers have to watch
# their spending budgets more closely. To follow up on 
# this hypothesis, we look for the stores in the wealthiest
# and the poorest neighborhoods.


# We find that store 62 is in the wealthiest area, while
# store 75 is in the poorest one.

# Lattice scatter plots of sales versus price, on separate
# panels for these two stores, with and without the presence
# of feature advertisments, are shown below. In order
# to get a better idea about the effect of price on sales, 
# we repeat the first scatter plot and add the best fitting
# (least squares) line to the graph; more discussion on
# how to determine that best fitting line is given later.

# The slope of the fitted line is more negative for the 
# poorest store, indicating that its customers are more
# sensitive to changes in the price.

t21=tapply(oj$INCOME,oj$store,
           FUN=mean,na.rm=TRUE)
t21
t21[t21==max(t21)]
t21[t21==min(t21)]

oj1=oj[oj$store == 62,]
oj2=oj[oj$store == 75,]
oj3=rbind(oj1,oj2)
xyplot(logmove~price|store,
       data=oj3)
xyplot(logmove~price|store,
       groups=feat,data=oj3)
## store in the wealthiest neighborhood
mhigh=lm(logmove~price,data=oj1)
summary(mhigh)
plot(logmove~price,data=oj1,
     xlim=c(0,4),ylim=c(0,13))
abline(mhigh)
## store in the poorest neighborhood
mlow=lm(logmove~price,data=oj2)
summary(mlow)
plot(logmove~price,data=oj2,
     xlim=c(0,4),ylim=c(0,13))
abline(mlow)
