##Peer-graded Assignment: Course Project 2
##Download files from working directory
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


d1<-tbl_df(NEI)
ds1<-tbl_df(SCC)

##Plot 1 Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
##Using the base plotting system, make a plot showing the total PM2.5 emission from all 
##sources for each of the years 1999, 2002, 2005, and 2008.

png("plot1.png")
totale<-summarise(group_by(d1,year),Emissions=sum(Emissions))
clrs<-c("orange","brown","magenta","blue")
p1<-barplot(height = totale$Emissions/1000,names.arg = totale$year,xlab="years",col=clrs,ylab = expression('total PM'[2.5]*'emission in kilotons'),ylim = c(0,8000),main=expression(('total PM'[2.5]*'emissions per year/kilotons')))

text(x = p1, y = round(totale$Emissions/1000,2), label = round(totale$Emissions/1000,2), pos = 3, cex = 0.8, col = "black")

dev.off()

##Plot 2  Have total emissions from PM2.5 decreased in the Baltimore City, Maryland ##
##(fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot 
##answering this question.

png("plot2.png")
totalbalt<-summarise(group_by(filter(d1,fips=="24510"),year),Emissions=sum(Emissions))
clrs<-c("orange","brown","magenta","blue")
p2<-barplot(height = totalbalt$Emissions/1000,names.arg = totalbalt$year,xlab="years",col=clrs,ylab = expression('total PM'[2.5]*'emission in kilotons'),ylim = c(0,4),main=expression(('total PM'[2.5]*'Baltimore emissions per year/kilotons')))

text(x = p2, y = round(totalbalt$Emissions/1000,2), label = round(totalbalt$Emissions/1000,2), pos = 3, cex = 0.8, col = "black")

dev.off()

        ##PLOT 3
##Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
##variable, which of these four sources have seen decreases in emissions from 1999-2008 
##for Baltimore City? Which have seen increases in emissions from 1999-2008? 
##Use the ggplot2 plotting system to make a plot answer this question.

png("plot3.png")

totalbaltyr<-summarise(group_by(filter(d1,fips=="24510"),year,type),Emissions=sum(Emissions))

ggplot(totalbaltyr,aes(x=factor(year),y=Emissions,fill=type,label=round(Emissions,2)))+geom_bar(stat="identity")+
        facet_grid(.~type)+xlab("year")+ylab(expression("total PM"[2.5]*"emission tons"))+
        ggtitle(expression("PM"[2.5]*paste("Baltimore Emission","City types",sep="")))+
        geom_label(aes(fill=type),color="red",fontface="bold")

dev.off()


        ##PLOT 4
##Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

png("plot4.png")
coal<-grepl("Fuel Comb.*Coal",ds1$EI.Sector)
coal_source<-ds1[coal,]
coale<-d1[d1$SCC %in% coal_source$SCC,]
coaler<-summarise(group_by(coale,year),Emissions=sum(Emissions))

ggplot(coaler,aes(x=factor(year),y=Emissions/1000,fill=year, label=round(Emissions/1000,2)))+
        geom_bar(stat="identity")+
        xlab("year")+ylab(expression("total PM"[2.5]*"emissions Kiloton"))+
        ggtitle("Emissions Coal related - Kilotons")+
        geom_label(aes(fill=year),color="green")

dev.off()


        ##PLOT 5
##How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City
png("plot5.png")
baltem<-d1[(d1$fips=="24510")&(d1$type=="ON-ROAD"),]
baltemyr<-summarise(group_by(baltem,year),Emissions=sum(Emissions))
ggplot(baltemyr,aes(x=factor(year),y=Emissions,fill=year,label=round(Emissions,2)))+
        geom_bar(stat = "identity")+xlab("year")+ylab(expression("Total PM"[2.5]*"emissions in tons"))+
        ggtitle("Baltimore vehicle emissions")+geom_label(aes(fill=year),color="yellow")

dev.off()
                                                          

        ##PLOT 6
##Compare emissions from motor vehicle sources in Baltimore City with emissions from 
##motor vehicle sources in Los Angeles County, California (fips == "06037").
## Which city has seen greater changes over time in motor vehicle emissions?


baltem2<-summarise(group_by(filter(d1,fips=="24510"&type=="ON-ROAD"),year),Emissions=sum(Emissions))
LAem<-summarise(group_by(filter(d1,fips=="06037"&type=="ON-ROAD"),year),Emissions=sum(Emissions))

baltem2$County<-"Baltimore City, MD"
LAem$County<-"Los Angeles County, CA"
balt_LA<-rbind(baltem2,LAem)

png("plot6.png")
ggplot(balt_LA,aes(x=factor(year),y=Emissions,fill=County,label=round(Emissions,2)))+
        geom_bar(stat="identity")+
        facet_grid(County~.,scales = "free")+
        xlab("year")+
        ylab(expression("total PM"[2.5]*"emissions in tons"))+
        ggtitle(expression("Vehicle emission difference - Baltimore vs LA - Units = tons"))+
        geom_label(aes(fill=County), color="white")

dev.off()



