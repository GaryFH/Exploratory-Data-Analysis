##Peer-graded Assignment: Course Project 2

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")



d1<-tbl_df(NEI)
ds1<-tbl_df(SCC)

totale<-summarise(group_by(d1,year),Emissions=sum(Emissions))
clrs<-c('red','green','blue','yellow')
