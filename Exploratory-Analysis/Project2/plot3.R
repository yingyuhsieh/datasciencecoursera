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
# Task 3:   Which sources have decreases/increase in emissions from 1999–2008
#           for Baltimore City?
#           using subset to get NEI data in (fips == "24510")
            NEI.ba = subset(NEI,NEI$fips=="24510")
#           specify type of source
            types=c("point", "nonpoint", "onroad", "nonroad")
#           Using aggregate by year and type
            pm_sum_year_type.ba= aggregate(Emissions~year+type,
                                    data=NEI.ba, sum)
#           load ggplot2 library
            library(ggplot2)
#           using gglot to show sum of PM2.5 by different year and source type

            g3 = ggplot(data=pm_sum_year_type.ba, aes(x=as.character(year),
                         y=Emissions, fill=type)) +
                        geom_bar(stat="identity",position=position_dodge()) +
                        xlab("Year") +
                        ylab(expression("Sum of " * PM[2.5])) +
                        ggtitle(expression("Sum of " * PM[2.5] *
                           " by Year and Type  (Baltimore, MD)"))
            print(g3)
#           save to png file
            png("plot3.png", width=800, height=600)
            print(g3)
            dev.off()
