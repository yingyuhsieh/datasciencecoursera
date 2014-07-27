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
# Task 4:   How have emissions from coal combustion-related sources changed
#           from 1999–2008?
#           finding SCC values with combustion (comb) and coal
            SCC.comb.coal = as.character(SCC[grep("Comb(.*)Coal",
                                      SCC$EI.Sector, perl=T),1])
#           using subset to get NEI data with SCC in the result above
            NEI.comb.coal = subset(NEI, NEI$SCC %in% SCC.comb.coal)
#           Using aggregate by year
            pm_sum_cc_year= aggregate(Emissions~year, data=NEI.comb.coal, sum)
#           specify year
            years=c(1992,2002,2005,2008)
#           Use bar plot to show the change PM 2.5
            barplot(pm_sum_cc_year$Emissions, names.arg=years,
                    ylab=expression(PM[2.5]),
                    main=expression("Sum of " * PM[2.5] *
                            " from Coal Combustion-related Sources"))
#           save to png file
            png("plot4.png", width=480, height=480)
            barplot(pm_sum_cc_year$Emissions, names.arg=years,
                    ylab=expression(PM[2.5]),
                    main=expression("Sum of " * PM[2.5] *
                            " from Coal Combustion-related Sources"))
            dev.off()
