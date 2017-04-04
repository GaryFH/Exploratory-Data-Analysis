##Peer-graded Assignment: Course Project 2
##Download files from working directory
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")



d1<-tbl_df(NEI)
ds1<-tbl_df(SCC)

totale<-summarise(group_by(d1,year),Emissions=sum(Emissions))
clrs<-c("red","green","blue","yellow")
p1<-barplot(height = totale$Emissions/1000,names.arg = totale$year,xlab="years",col=clrs,ylab = expression('total PM'[2.5]*'emission in kilotons'),ylim = c(0,8000),main=expression(('total PM'[2.5]*'emissions per year/kilotons')))

text(x = p1, y = round(totale$Emissions/1000,2), label = round(totale$Emissions/1000,2), pos = 3, cex = 0.8, col = "black")

##Plot 1 Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
##Using the base plotting system, make a plot showing the total PM2.5 emission from all 
##sources for each of the years 1999, 2002, 2005, and 2008.

png("plot1.png")
totale<-summarise(group_by(d1,year),Emissions=sum(Emissions))
clrs<-c("red","green","blue","yellow")
p1<-barplot(height = totale$Emissions/1000,names.arg = totale$year,xlab="years",col=clrs,ylab = expression('total PM'[2.5]*'emission in kilotons'),ylim = c(0,8000),main=expression(('total PM'[2.5]*'emissions per year/kilotons')))

text(x = p1, y = round(totale$Emissions/1000,2), label = round(totale$Emissions/1000,2), pos = 3, cex = 0.8, col = "black")


##Plot 2  Have total emissions from PM2.5 decreased in the Baltimore City, Maryland ##
##(fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot 
##answering this question.

png("plot2.png")
totalbalt<-summarise(group_by(filter(d1,fips=="24510"),year),Emissions=sum(Emissions))
clrs<-c("red","green","blue","yellow")
p2<-barplot(height = totalbalt$Emissions/1000,names.arg = totalbalt$year,xlab="years",col=clrs,ylab = expression('total PM'[2.5]*'emission in kilotons'),ylim = c(0,4),main=expression(('total PM'[2.5]*'Baltimore emissions per year/kilotons')))

text(x = p2, y = round(totalbalt$Emissions/1000,2), label = round(totalbalt$Emissions/1000,2), pos = 3, cex = 0.8, col = "black")







