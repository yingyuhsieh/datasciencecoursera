library(shiny)
library(ggplot2)
dfa <- read.csv("agent_results.txt", sep="\t")
timeformat="%m/%d/%Y %H:%M:%S"
dfa <- transform(dfa,Start=strptime(Start, timeformat))
dfa <- transform(dfa,Stop=strptime(Stop, timeformat))
x <- dfa$Second
pam.best <- pamk(x)
best <- pam.best$nc


shinyServer(
    function(input, output) {
        set.seed(1024)
        output$dataPlot <- renderPlot({
            maxy = input$maxy
            if (input$sort == 0)
            {
                x <- dfa$Second
            } else {
                x <- sort(dfa$Second)
            }
            
            dc <- kmeans(x, input$maxc)
            if (best==input$maxc)
            {
                result=paste("Derived thread number (total_agents/cluster_number) = ", round(length(x)/input$maxc,1),
                             " (best of K-mean)")
            }
            else
            {
                result=paste("Derived thread number (total_agents/cluster_number) = ", round(length(x)/input$maxc,1))
            }
            output$resPlot <- renderPlot({
                c <- 1:input$maxc
                ws <- sapply(c, function(i)
                {
                    sum(kmeans(x, center=i)$withinss)  
                })
                plot(c, ws, xlab="Number of Clusters", 
                     ylab="Within groups sum of squares", type="b", 
                     main=result)
            })
            
            
            plot(x, xlab="Agent", ylab="Task durating (seconds)",
                 pch=20, main="Time needed to complete task",
                 ylim=c(0,maxy), col=dc$cluster)
        })
    }
)
