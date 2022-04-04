
library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Generate Random Gaussian data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(

        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            numericInput("Xmean", label = h4("Mean of X"), value = 0),
            numericInput("Ymean",label = h4("Mean of Y"), value = 0),
            numericInput("Xvar", label = h4("Variance of X"), value = 1),
            numericInput("Yvar", label = h4("Variance of Y"), value = 1),
        numericInput("num", label = h4("Num of Obs"), value = 500)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Histogram",plotOutput("distPlot_1"),plotOutput("distPlot_2")),
                tabPanel("Scatter",plotOutput("sc",brush = brushOpts(id = "sc_brush")),
                         plotOutput("response1")),
                tabPanel("Estimation",tableOutput("result"),tableOutput('result1'))
    )
)
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot_1 <- renderPlot({
        set.seed(100)
        x <- rnorm(input$num,mean = input$Xmean,sd =sqrt(input$Xvar))
        y<- rnorm(input$num,mean = input$Ymean,sd =sqrt(input$Yvar))
        data = data.frame(x,y)
        # draw the histogram with the specified number of bins
        ggplot(data, aes(x=x)) + geom_histogram(bins = input$bins)
    })
    output$distPlot_2 <- renderPlot({
        set.seed(100)
        x <- rnorm(input$num,mean = input$Xmean,sd =sqrt(input$Xvar))
        y<- rnorm(input$num,mean = input$Ymean,sd =sqrt(input$Yvar))
        data = data.frame(x,y)
        # draw the histogram with the specified number of bins
        ggplot(data, aes(x=y)) + geom_histogram(bins = input$bins)
    })
    output$sc <- renderPlot({
        set.seed(100)
        x <- rnorm(input$num,mean = input$Xmean,sd =sqrt(input$Xvar))
        y<- rnorm(input$num,mean = input$Ymean,sd =sqrt(input$Yvar))
        data = data.frame(x,y)
        # draw the histogram with the specified number of bins
        ggplot(data, aes(x,y)) + geom_point()+geom_smooth(method='lm')
    })
    selectedData <- reactive({
        set.seed(100)
        x <- rnorm(input$num,mean = input$Xmean,sd =sqrt(input$Xvar))
        y<- rnorm(input$num,mean = input$Ymean,sd =sqrt(input$Yvar))
        data = data.frame(x,y)
        data.scatter <- brushedPoints(data, input$sc_brush)
        if (nrow(data.scatter) == 0){data.scatter <- data}
        data.scatter
    })
    output$response1<-renderPlot({
        ggplot(selectedData(),aes(x,y))+
            geom_point()+
            geom_smooth(method='lm')})
    
    output$result <- renderTable({
        set.seed(100)
        x <- rnorm(input$num,mean = input$Xmean,sd =sqrt(input$Xvar))
        y <- rnorm(input$num,mean = input$Ymean,sd =sqrt(input$Yvar))
        Model_total = c('intercept','x')
        model = summary(lm(y~x))
        r = cbind(c('intercept','x'),model$coefficients)
        r = data.frame(r)
        colnames(r) = c('Model_total',"Est","Std.error",'t-value','P-value')
        r
        
    } )
    output$result1 <- renderTable({
        data = selectedData()
        model = summary(lm(data[,2]~data[,1]))
        r = cbind(c('intercept','x'),model$coefficients)
        r = data.frame(r)
        colnames(r) = c('Model_brush',"Est","Std.error",'t-value','P-value')
        r
        
    } )
}

shinyApp(ui = ui, server = server)
