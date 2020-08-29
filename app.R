library(shiny)
library(ggplot2)
library(gridExtra)
library(quantmod)

data<-data.frame(date=usdata$date, V=V$Close,ZM=ZM$Close,PG=PG$Close,deathIncrease=usdata$deathIncrease,positiveIncrease=usdata$positiveIncrease,hospitalized=usdata$hospitalized)

ui = fluidPage(
    titlePanel("Covid-19"),
    sidebarLayout(
        sidebarPanel(
            selectInput("usdata",
                        "Select Data:",
                        choices = c("deathIncrease", 
                                    "positiveIncrease",
                                    "hospitalized"),
                        selected = "deathIncrease"),
            selectInput("stock",
                        "Select Stock:",
                        choices = c("V", 
                                    "ZM",
                                    "PG"),
                        selected = "V"
                        
            )
        ),
        mainPanel( 
                plotOutput("plot1"),
                plotOutput("plot2"),
                plotOutput("plot3")
                
        )
    ) 
)



server = function(input, output) {

    output$plot1 <- renderPlot({
        
        p1<-ggplot(usdata,aes(x=date,y=deathIncrease)) +
            geom_point(colour='red')
        p2<-ggplot(data,aes(x=date,y=positiveIncrease)) +
            geom_point(colour='red') 
        p3<-ggplot(data,aes(x=date,y=hospitalized)) +
            geom_point(colour='red') 
        grid.arrange(p1,p2,p3, ncol=3,widths=c(1,1,1))
    })
    
    output$plot2<-renderPlot({
        p4<-ggplot(data,aes(x=date,y=V)) +
            geom_point(colour='red') 
        p5<-ggplot(data,aes(x=date,y=ZM)) +
            geom_point(colour='red') 
        p6<-ggplot(data,aes(x=date,y=PG)) +
            geom_point(colour='red') 
        grid.arrange(p4,p5,p6, ncol=3,widths=c(1,1,1))
    })
    
    output$plot3<-renderPlot({
        data1 <- data[, c(input$stock, input$usdata)]
        colnames(data1) <- c("col1", "col2")
        ggplot(data1,aes(x=col1,y=col2)) +
            geom_line(colour='red') +
            labs(x = input$stock, y = input$usdata)
    }, height = 200, width = 600)
}

shinyApp(ui = ui, server)