#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#--Load libraries to enable interactivity and to load the spss data file (.sav)
library(shiny)
library(haven)

#--Load ANES 2018 Data
#---Because this statement is outside the UI and Server functions, 
#---it is performed only once, saving resources on refreshes
Interest_2018.df <- read_spss('./Interest_2018_3.sav')

# Define UI for application that draws Box Plots
ui <- fluidPage(
   
   # Application title
   titlePanel("2018 ANES Attribute Distributions by Religion"),
   
   # Sidebar with a dropdown menu input for variable to compare
   sidebarLayout(
      sidebarPanel(
        selectInput("ychoice",
                    "Variable Options",
                    names(Interest_2018.df)[1:length(names(Interest_2018.df))-1],
                    selected = "Political Ideology")
      ),
      
      # Show a multiple box plot of the generated distributions
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a multiple box plot
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     # input which column was chosen by the user
     column <- input$ychoice
     
      #--Generate Y axis label, changes with variable choice
     ylabtext <- ifelse(grepl("Favorability",column),
                        paste0(column,': Low - High'),
                        ifelse(grepl("Ideology",column),
                               'Ideology: 1 Very Liberal, 7 Very Conservative',
                        'Birth Year'))
     
     ## draw multiple box plot with the specified column
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

