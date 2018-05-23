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
library(dplyr)
library(gapminder)

dat <- gapminder::gapminder 
dat <- dat %>% filter(country %in% c('United States','Canada'))


# Define UI for your shiny application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Gapminder"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("Year",
                    label = h5("Range of years:"),
                    min = 1952,
                    max = 2007,
                    value = c(1952, 2007), 
                    step = 5
                    ),
         
         radioButtons("Variable",
                      label = h5("Select Variable"),
                      choices = c("Population" = "pop",
                                  "Life Expectancy" = "lifeExp",
                                  "GDP Per Capita" = "gdpPercap")
                      )
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("GapminderPlot"),
         tableOutput("GapminderTable")
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$GapminderPlot <- renderPlot({
     
     

    dat %>% filter(year >= min(input$Year) & year <= max(input$Year)) %>%
       ## Use aes_string to map input variable to the aesthetics as string
     ggplot(aes_string(x = "year",y = input$Variable,colour = "country")) +
       geom_line(size = 1) +
       xlab("Year") +
       labs(title = (paste0("US - CANADA ",input$Variable," comparison between ",min(input$Year),"-",max(input$Year)))) +
       ylab(aes_string(input$Varible)) + theme_bw()



   })
   
   output$GapminderTable <- renderTable({
     
     data <- dat %>% filter(year >= min(input$Year) & year <= max(input$Year)) ## filter data for seleted year range
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

