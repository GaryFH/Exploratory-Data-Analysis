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

png("plot1.png")
totale<-summarise(group_by(d1,year),Emissions=sum(Emissions))
clrs<-c("red","green","blue","yellow")
p1<-barplot(height = totale$Emissions/1000,names.arg = totale$year,xlab="years",col=clrs,ylab = expression('total PM'[2.5]*'emission in kilotons'),ylim = c(0,8000),main=expression(('total PM'[2.5]*'emissions per year/kilotons')))

text(x = p1, y = round(totale$Emissions/1000,2), label = round(totale$Emissions/1000,2), pos = 3, cex = 0.8, col = "black")




