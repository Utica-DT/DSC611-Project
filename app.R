#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(haven)

Interest_2018.df <- read_spss('./Interest_2018_3.sav')

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("2018 ANES Attribute Distributions by Religion"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("ychoice",
                    "Variable Options",
                    names(Interest_2018.df)[1:length(names(Interest_2018.df))-1],
                    selected = "Political Ideology")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     # generate bins based on input$bins from ui.R
     column <- input$ychoice
     #print(column)
     #summary(Interest_2018.df[,c(1:3)])
     
     ylabtext <- ifelse(grepl("Favorability",column),
                        paste0(column,': Low - High'),
                        ifelse(grepl("Ideology",column),
                               'Ideology: 1 Very Liberal, 7 Very Conservative',
                        'Birth Year'))
     
     ## draw the histogram with the specified number of bins
     boxplot(unlist(unclass(Interest_2018.df[,column]))~
               unclass(Interest_2018.df$Religion),
             xlab = '',
             ylab = ylabtext,
             main = paste0('ANES 2018 Religion and ',column),
             xaxt = "n",
             sub = "Religion")
     axis(side = 1,at = c(1:10),
          labels = c('Protestant','Catholic','Mormon',
                     'Orthodox','Jewish','Muslim',
                     'Buddhist','Agnostic','Nothing','Other'),
          las= 2)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

