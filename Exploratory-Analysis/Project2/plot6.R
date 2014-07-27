# -----------------------------------------------------------------------------
# (1)   set working directory
#       Please put this script file in the same folder of data files
#       (summarySCC_PM25.rds and Source_Classification_Code.rds)
#       setwd("<path_to_exdata_data_NEI_data>")
# -----------------------------------------------------------------------------
# (2)   read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# -----------------------------------------------------------------------------
#
# Task 6:   Compare emissions from motor vehicle sources changed from
#           1999–2008 in Baltimore City with Los Angeles County
#           finding SCC values presenting motor vehicle sources
            SCC.motor.vehicle = as.character(SCC[grep("Mobile(.*)Vehicles",
                                  SCC$EI.Sector, perl=T),1]) 
#           using subset to get NEI data with SCC in the result above and 
#           (fips == "24510" OR flips == "06037")
            NEI.bc.mv = subset(NEI, (NEI$SCC %in% SCC.motor.vehicle) &
                       (NEI$fips %in% c("24510","06037")))
#           Using aggregate by year
            pm_sum_bcmv_year= aggregate(Emissions~year+fips,
                                data=NEI.bc.mv, sum)
#           load ggplot2 library
            library(ggplot2)
#           using gglot to show sum of PM2.5 by different year and source type
            g6 = ggplot(data=pm_sum_bcmv_year, aes(x=as.character(year),
                           y=Emissions, fill=fips)) +
            geom_bar(stat="identity",position=position_dodge()) +
                        xlab("Year") +
                        ylab(expression("Sum of " * PM[2.5])) +
                        ggtitle(expression("Sum of " * PM[2.5] *
                           " from Mortor Vehicle Sources")) +
                        scale_fill_discrete(name="City",
                            breaks=c("06037","24510"),
                            labels=c("LA County","Baltimore"))
            print(g6)
#           save to png file
            png("plot6.png", width=800, height=600)
            print(g6)
            dev.off()
#           Still need to calculate the ratio of increasing and decreasing
#           LA County
            ca_1999 = subset(pm_sum_bcmv_year,pm_sum_bcmv_year$year==1999
                 & pm_sum_bcmv_year$fips=="06037",3)
            ca_2008 = subset(pm_sum_bcmv_year,pm_sum_bcmv_year$year==2008
                 & pm_sum_bcmv_year$fips=="06037",3)
#           Baltimore
            ba_1999 = subset(pm_sum_bcmv_year,pm_sum_bcmv_year$year==1999
                 & pm_sum_bcmv_year$fips=="24510",3)
            ba_2008 = subset(pm_sum_bcmv_year,pm_sum_bcmv_year$year==2008
                 & pm_sum_bcmv_year$fips=="24510",3)
#           Compare changes of emmisions
            ca_change = (ca_2008-ca_1999)
            ca_change
#           the vaue is 170.201
            ba_change = (ba_2008-ba_1999)
            ba_change
#           the value is -258.5445
            ca_change_rate = ca_change/ca_1999
            sprintf("%1.2f%%", 100*ca_change_rate)
#           the Rate is 4.33% (increased)
            ba_change_rate = ba_change/ba_1999
            sprintf("%1.2f%%", 100*ba_change_rate)
#           the rate is -74.55% (decreased)

