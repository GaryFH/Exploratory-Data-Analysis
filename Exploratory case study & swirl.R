##Exploratory case study
        ## Particulate matter (less than 2.5 microns in diameter) is a fancy name for dust, and breathing in dust might pose
        ## health hazards to the population. We'll study data from two years, 1999 (when monitoring of particulate matter
        ## started) and 2012. Our goal is to see if there's been a noticeable decline in this type of air pollution between
        ## these two years.  
##Loading required package: spam,grid
##Loading required package: base packages(maps,backsolve,forwardsolve)
head(pm0)
##We also see that the column names, V1, V2, etc., | are not informative. 
##However, we know that the first line of the original file (a comment) explained 
##what information the columns contained.  We created the variable cnames containing
##the 28 column names of the original file

cnames<-strsplit(cnames,"|",fixed = TRUE)##this contains header info


##Assign to names(pm0) the output of a call to the function make.names with
##cnames[[1]][wcol] as the argument. The variable wcol holds the indices of the 
##5 columns we selected (from the 28) to use in this lesson 
##As the name suggests, the function "makes syntactically valid names"
names(pm0)<-make.names(cnames[[1]][wcol])

##The measurements of particulate matter (pm25) are in the column named Sample.Value.
## Assign this component of pm0 to the variable x0. Use the m$n notation

x0<-pm0$Sample.Value

names(pm1)<-make.names(cnames[[1]][wcol]) ##names(pm1) is the same as names(pm0)
x1<-pm1$Sample.Value

summary(x0)
summary(x1)

boxplot(x0,x1)  ##looks squished - try log10(x0)

boxplot(log10(x0),log10(x1)) ##looks better

##First, form the vector negative by assigning to it the boolean x1<0.
negative<-x1<0
sum(negative,na.rm=TRUE)##So there are over 26000 negative values. Sounds like a lot
mean(negative,na.rm=TRUE)## We see that just 2% of the x1 values are negative.

##dates are not in good form
dates<-pm1$Date
dates<-as.Date(as.character(dates),"%Y%m%d")

hist(dates[negative],"month") ##this tells negative values on a monthly basis


##We'll narrow our search and look just at monitors in New York State We subsetted off 
##the New York State monitor identification data for 1999 and 2012 into 
##   2 vectors, site0 and site1

both<-intersect(site0,site1) ##tells sites of monitors in both 1999 and 2012

##To save you some time and typing, we modified the data frames pm0 and pm1 slightly 
##by adding to each of them a new component, county.site.


##The subsets will filter for 2 characteristics. The first is State.Code equal to 36 
##(the code for New York), and the second is that the county.site (the component we added)
## is in the vector both
cnt0<-subset(pm0,State.Code==36&county.site%in%both)
cnt1<-subset(pm1,State.Code==36&county.site%in%both)


##Now run the command sapply(split(cnt0, cnt0$county.site), nrow). This will split cnt0 into 
##several data frames | according to county.site (that is, monitor IDs) and 
##tell us how many measurements each monitor recorded.
sapply(split(cnt0,cnt0$county.site),nrow)
sapply(split(cnt1,cnt1$county.site),nrow)

##We want to examine a monitor with a reasonable number of measurements so let's look at 
##the monitor with ID 63.2008. | Create a variable pm0sub

pm0sub<-subset(cnt0,County.Code==63&Site.ID==2008)
pm1sub<-subset(cnt1,County.Code==63&Site.ID==2008)


##Now we'd like to compare the pm25 measurements of this particular monitor (63.2008) 
##for the 2 years. First, create the vector x0sub by assigning to it the Sample.Value of pm0sub
x0sub<-pm0sub$Sample.Value
x1sub<-pm1sub$Sample.Value

##We'd like to make our comparison visually so we'll have to create a time series of these 
##pm25 measurements. First,| create a dates0
dates0<-as.Date(as.character(pm0sub$Date),"%Y%m%d")
dates1<-as.Date(as.character(pm1sub$Date),"%Y%m%d")

##Now we'll plot these 2 time series in the same panel using the base plotting system
par(mfrow=c(1,2),mar=c(4,4,2,1))

plot(dates0,x0sub,pch=20)
abline(h=median(x0sub,na.rm=TRUE),lwd=2)

plot(dates1,x1sub,pch=20)
abline(h=median(x1sub,na.rm=TRUE),lwd=2)

rng<-range(x0sub,x1sub,na.rm=TRUE)##this helps determine ylim for both plots so they have
                                        ##the same scale


##The last avenue of this data we'll explore (and we'll do it quickly) concerns a 
##comparison of all the states' mean pollution levels.

mn0<-with(pm0,tapply(Sample.Value,State.Code,mean,na.rm=TRUE))
mn1<-with(pm1,tapply(Sample.Value,State.Code,mean,na.rm=TRUE))

##Now we'll create 2 new dataframes containing just the state names and their 
##mean measurements for each year. First, we'll do this for 1999. Create the
##data frame d0 by calling the function data.frame with 2 arguments. The first is 
##state set equal to names(mn0), and the second is mean set equal to mn0.

d0<-data.frame(state=names(mn0),mean=mn0)
d1<-data.frame(state=names(mn1),mean=mn1)


##Create the array mrg by calling the R command merge with 3 arguments, d0, d1, 
##and the argument by set equal to the string "state".
mrg<-merge(d0,d1,by="state")

with(mrg, plot(rep(1, 52), mrg[, 2], xlim = c(.5, 2.5)))
with(mrg, points(rep(2, 52), mrg[, 3]))  ##points not plot since it is the same plot


##We see a shorter column of points at x=2. Now let's connect the dots. Use the R function
##segments with 4 arguments. The first 2 are the x and y coordinates of the 1999 points 
##and the last 2 are the x and y coordinates of the 2012 points. As in the previous calls
## specify the x coordinates with calls to rep and the y coordinates with references to
##the appropriate columns of mrg  

segments(rep(1, 52), mrg[, 2], rep(2, 52), mrg[, 3])##lines connect to portions of plot


## For fun, let's see which states had higher means in 2012 than in 1999. Just use the 
##mrg[mrg$mean.x < mrg$mean.y, ]notation to find the rows of mrg with this particulate property.
mrg[mrg$mean.x<mrg$mean.y,]



