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
# Task 1:   Have total emissions from PM2.5 decreased in the United States from
#           1999 to 2008?
#
#           Using aggregate by year
pm_sum_year= aggregate(Emissions~year, data=NEI, sum)
#           specify year
years=c(1992,2002,2005,2008)
#           Use bar plot to show if PM 2.5 decreased
barplot(pm_sum_year$Emissions, names.arg=years,
        ylab=expression(PM[2.5]),
        main=expression("Sum of " * PM[2.5]))
#           save to png file
png("plot1.png", width=480, height=480)
barplot(pm_sum_year$Emissions,
        names.arg=years,
        ylab=expression(PM[2.5]),
        main=expression("Sum of " * PM[2.5]))
dev.off()
