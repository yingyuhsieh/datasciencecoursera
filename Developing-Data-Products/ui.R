library(shiny)

dfa <- read.csv("agent_results.txt", sep="\t")
maxy <- ceiling(max(dfa$Second/50))*50
library(fpc)
pamk.best <- pamk(dfa$Second)
maxc <- pamk.best$nc


shinyUI(fluidPage(
    titlePanel("Active thread of server-client application"),
    column(12, includeHTML("01.explain.html")),
    sidebarLayout(
        sidebarPanel(
            h5("Step 1: Check if active thread is true"),
            radioButtons("sort", "Sort:",
                         c("by index" = "0",
                           "by time" = "1")),
            sliderInput("maxy",
                        "Max duration time (y-axis):",
                        min = 0,
                        max = maxy,
                        value = maxy,
                        step = 50),
            br(),br(),
            h5("Step 2: Find out thread number by K-mean clustering"),
            sliderInput("maxc",
                        "Cluster number:",
                        min = 1,
                        max = (maxc+5),
                        value = 1,
                        step = 1),
            br(),br()
        ),
        mainPanel(
            plotOutput("dataPlot"),
            plotOutput("resPlot")
        )
    )     
))