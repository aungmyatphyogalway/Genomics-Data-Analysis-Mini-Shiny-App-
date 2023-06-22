#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Microarray_Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("file",
                        "Choose CSV File:",
                        buttonLabel = "Browse...",
                        accept = c('text/csv', 'text/comma-separated-values,text/plain','.csv')
            ),
            numericInput("num",
                         "Number of Genes to Display",
                         value=10,
                         min=1,
                         max=1000),
            h3("Plot Thresholds:"),
            sliderInput("pvalue",
                        "Adj p-value",
                        min=0.0001,max=0.1,value=0.045,step =0.01),
            sliderInput("log2fc",
                        "log2FC",
                        min = 0,max=8,value=2,step=1)
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
          tableOutput("genelist"),
           plotOutput("histogram"),
          #plotOutput("volca"),
          align='center'
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  dataset<- reactive({
    inputfile<- input$file
    if (is.null(inputfile))
      return()
    mydata<-read.csv(inputfile$datapath, row.names = NULL,sep = '\t')
    return(mydata)
    
    
  })
  
  #render a table
  output$genelist<-renderTable({
    head(dataset(),n=input$num)
  })
  
  #render volcanoplot
  output$histogram<-renderPlot({
    mydata<- dataset()
    if (is.null(mydata))
      return()
    x<-mydata$adj.P.Val
    bins <- seq(min(x)-5, max(x)+5, length.out = 20)
    hist(x, breaks = bins, col = 'blue', border = 'white', main='Our Distribution')
  })
  
  #output$volca<-renderPlot({
   # mydata<-dataset()
    #if(is.null(mydata))
      #return()
    
    #g<-input$logFC
    #j<-input$adj.P.val
    #j<-as.numeric(j)
    #j<--log10(input$adj.P.val)
   # g<-as.numeric(g)
    #j<-as.numeric(j)
   # ggplot(data=mydata,aes(x=g,y=j))+geom_point(size=3.5)
  }

    
        

# Run the application 
shinyApp(ui = ui, server = server)
