

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)

netflix<-read_excel("netflix_titles.xlsx")

summary(netflix)
View(netflix)

ui <- fluidPage(

    # Application title
    titlePanel("Netflix Titles Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("type",
                        "Select Type:",
                        choices=unique(netflix$type),
                        selected="Movie"),
            sliderInput("range","Select Release Year Range: ",
                        min=min(netflix$release_year,na.rm=TRUE),
                        max=max(netflix$release_year,na.rm=TRUE),
                        value=c(2010,2020),
                        sep="")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("barPlot"),
           h3("Filtered Data Summary"),
           verbatimTextOutput("summarystats")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filtereddata<-reactive({
    netflix %>% filter(type==input$type,
                       release_year>=input$range[1],
                       release_year<=input$range[2])
  })

    output$barPlot <- renderPlot({
      filtereddata()%>% count(release_year) %>% ggplot(aes(x=release_year,y=n))+
        geom_col(fill="steelblue")+
        labs(
          title=paste("Number of",input$type,"Titles Released Per Year"),
          x="Release Year",
          y="Number of Titles"
        )+
        theme_minimal()
       
    }) 
    
    output$summarystats<-renderPrint({
      summary(filtereddata())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
