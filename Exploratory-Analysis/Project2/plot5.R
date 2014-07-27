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
# Task 5:   How have emissions from motor vehicle sources changed from
#           1999–2008 in Baltimore City?
#           finding SCC values presenting motor vehicle sources
SCC.motor.vehicle = as.character(SCC[grep("Mobile(.*)Vehicles",
                                    SCC$EI.Sector, perl=T),1]) 
#           using subset to get NEI data with SCC in the result above and 
#           (fips == "24510")
            NEI.ba.mv = subset(NEI, (NEI$SCC %in% SCC.motor.vehicle) &
                       NEI$fips == "24510")
#           Using aggregate by year
            pm_sum_bmv_year= aggregate(Emissions~year, data=NEI.ba.mv, sum)
#           specify year
            years=c(1992,2002,2005,2008)
#           Use bar plot to show the change PM 2.5
            barplot(pm_sum_bmv_year$Emissions, names.arg=years,
                    ylab=expression(PM[2.5]),
                    main=expression("Sum of " * PM[2.5] *
                        " from Motor Vehicle Sources in Baltimore, MD"))
#           save to png file
            png("plot5.png", width=480, height=480)
            barplot(pm_sum_bmv_year$Emissions, names.arg=years,
                ylab=expression(PM[2.5]),
                main=expression("Sum of " * PM[2.5] *
                    " from Motor Vehicle Sources in Baltimore, MD"))
            dev.off()
