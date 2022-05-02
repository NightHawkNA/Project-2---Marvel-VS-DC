#STA 6233 Project 2 by Nils Antonson Spring 2022

#Load the Library
library(shiny)
library(ggplot2)
library(DT)
library(plotly)
options(scipen = 999)#Turns off scientific notation in my graphs
load(url("https://github.com/NightHawkNA/Project-2---Marvel-VS-DC/raw/main/mdc2.RData"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title and image
  titlePanel(title=div(img(src="imageVS.jpg", height='125'), "Marvel VS DC Movie Studio Comparison Project"), windowTitle = "Marvel VS DC Movie Studio Comparison Project"),
  
  fluidRow(
    column(9,
           fluidRow(
             column(12,
                    tabsetPanel(
                      
                      tabPanel("Main",#, 
                               includeHTML("main.html")),
                      
                      
                      tabPanel("Data Table",
                               
                               br(),
                               
                               h5("You may view and search the dataset below:"),
                               div(tableOutput("mdc2"), style = "font-size:80%"),
                               
                               ui <- basicPage(
                                 h2("Marvel VS DC Dataset"),
                                 DT::dataTableOutput("mytable")
                               )
                      ),
                      
                      tabPanel("Graph Comparison",
                               
                               br(),
                               
                               p("The following graphs can be used to compare the two movie studios. Red dots represent Marvel Studio and blue dots represent DC Studio:"),
                               
                               br(),
                               
                               fluidRow(
                                 column(4,
                                        plotlyOutput("graph1", height = 400, width = 450)
                                 ),
                                 column(4,
                                        plotlyOutput("graph2", height = 400, width = 450)
                                 ),
                                 column(4,
                                        plotlyOutput("graph3", height = 400, width = 450)
                                 )),
                               
                               br(), br(),
                               
                               fluidRow(
                                 column(4,
                                        plotlyOutput("graph4", height = 400, width = 450)
                                 ),
                                 column(4,
                                        plotlyOutput("graph5", height = 400, width = 450)
                                 ),
                                 column(4,
                                        plotlyOutput("graph6", height = 400, width = 450)
                                 )
                               )
                      ),
                      
                      tabPanel("Adjustable Scatter Plot",
                               
                               br(),
                               
                               p("Please select the x and y variables to make a comparison:"),
                               sidebarLayout(
                                 #Inputs: Select which inputs from the data we want to display
                                 sidebarPanel(
                                   #Select variable for y-axis
                                   selectInput(inputId = "y", 
                                               label = "Y-axis:",
                                               choices = c("year", "runtime", "imdb_rating", "imdb_votes", "imdb_gross", "tomato_meter", "tomato_meter",
                                                           "tomato_review", "tom_aud_score", "tom_ratings"), 
                                               selected = "year"),
                                   #select X-axis variables
                                   selectInput(inputId = "x", 
                                               label = "X-axis:",
                                               choices = c("year", "runtime", "imdb_rating", "imdb_votes", "imdb_gross", "tomato_meter", "tomato_meter",
                                                           "tomato_review", "tom_aud_score", "tom_ratings"), 
                                               selected = "imdb_rating")
                                 ),
                                 #Output: Type of plot
                                 mainPanel(
                                   plotOutput(outputId = "FreqTab")
                                 ))
                      ), 
                      tabPanel("About", 
                               includeHTML("About.html"))
                    )
             ))
           
    ) #column end
    
  ))






# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$FreqTab <- renderPlot({
    # draw the histogram with the specified number of bins
    sp<-ggplot(mdc2, aes_string(x=input$x, y=input$y)) + geom_point(aes(color = entity)) + theme(text=element_text(size=20))
    sp})
  
  #Output the raw table data
  output$mytable = DT::renderDataTable({
    mdc2})
  
  #Tomato Review VS Year plot
  output$graph1 <- renderPlotly({
    sp<-ggplot(mdc2, aes_string(x="tomato_review", y="year")) + geom_point(aes(color = entity, text = title), show.legend = FALSE) + ggtitle("Tomato Review VS Year") + theme(text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title=element_text(size=12)) + theme(plot.title = element_text(vjust = 4)) + theme(plot.title = element_text(colour = "Purple")) + labs(x="Tomato Review",y="Year") + theme(legend.position="none") 
    sp})
  
  
  #IMDB Score VS Year plot
  output$graph2 <- renderPlotly({
    sp<-ggplot(mdc2, aes_string(x="imdb_votes", y="year")) + geom_point(aes(color = entity, text = title), show.legend = FALSE) + ggtitle("IMDB Score VS Year") + theme(text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title=element_text(size=12)) + theme(plot.title = element_text(vjust = 4)) + theme(plot.title = element_text(colour = "Purple")) + labs(x="IMDB Score",y="Year") + theme(legend.position="none")
    sp})
  
  #IMDB Gross VS Year plot
  output$graph5 <- renderPlotly({
    sp<-ggplot(mdc2, aes_string(x="imdb_gross", y="year")) + geom_point(aes(color = entity, text = title), show.legend = FALSE) + ggtitle("IMDB Gross VS Year") + theme(text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title=element_text(size=12)) + theme(plot.title = element_text(vjust = 4)) + theme(plot.title = element_text(colour = "Purple")) + labs(x="Earnings in $",y="Year") + theme(legend.position="none")
    sp})
  
  #Tomato Audience Score VS Year plot
  output$graph3 <- renderPlotly({
    sp<-ggplot(mdc2, aes_string(x="tom_aud_score", y="year")) + geom_point(aes(color = entity, text = title), show.legend = FALSE) + ggtitle("Tomato Audience Score VS Year") + theme(text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title=element_text(size=12)) + theme(plot.title = element_text(vjust = 4)) + theme(plot.title = element_text(colour = "Purple")) + labs(x="Tomato Score",y="Year") + theme(legend.position="none")
    sp})
  
  #Tomato Review VS Revenue plot
  output$graph4 <- renderPlotly({
    sp<-ggplot(mdc2, aes_string(x="imdb_gross", y="tomato_review")) + geom_point(aes(color = entity, text = title), show.legend = FALSE) + ggtitle("Tomato Review VS Revenue") + theme(text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title=element_text(size=12)) + theme(plot.title = element_text(vjust = 4)) + theme(plot.title = element_text(colour = "Purple")) + labs(x="Tomator Review",y="Revenue") + theme(legend.position="none")
    sp})
  
  
  #Runtime VS IMDB Rating plot
  output$graph6 <- renderPlotly({
    sp<-ggplot(mdc2, aes_string(x="runtime", y="imdb_rating")) + geom_point(aes(color = entity, text = title), show.legend = FALSE) + ggtitle("Runtime VS IMDB Rating") + theme(text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title=element_text(size=12)) + theme(plot.title = element_text(vjust = 4)) + theme(plot.title = element_text(colour = "Purple")) + labs(x="Runtime",y="IMDB Rating") + theme(legend.position="none")
    sp})
  
}

# Run the application 
shinyApp(ui = ui, server = server)