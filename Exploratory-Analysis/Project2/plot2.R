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
# Task 2:   Have total PM2.5 emissions decreased in the Baltimore, MD
#           (fips == "24510") from 1992 to 2008?
#
#           using subset to get NEI data in (fips == "24510")
            NEI.ba = subset(NEI,NEI$fips=="24510")
#           Using aggregate by year
            pm_sum_year.ba= aggregate(Emissions~year, data=NEI.ba, sum)
#           specify year
            years=c(1992,2002,2005,2008)
#           Use bar plot to show if PM 2.5 decreased
            barplot(pm_sum_year.ba$Emissions, names.arg=years,
                ylab=expression(PM[2.5]),
                main=expression("Sum of " * PM[2.5] * " (Baltimore, MD)"))
#           save to png file
            png("plot2.png", width=480, height=480)
            barplot(pm_sum_year.ba$Emissions, names.arg=years,
                ylab=expression(PM[2.5]),
                main=expression("Sum of " * PM[2.5] * " (Baltimore, MD)"))
            dev.off()
