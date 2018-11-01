#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bnwidget)
library(bnlearn)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Bayesian network visual"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     shiny::selectInput(
       inputId = "net",
       h5("Bayesian Network:"),
       c("Sample Discrete Network" = 1,
         "Asia" = 2,
         "Alarm Network" = 3,
         "Insurance Network" = 4,
         "Hailfinder Network" = 5
       )
     ),
      
      # Show the Bayesian network
      mainPanel(
        shinydashboard::box(
          title = "Bayesian Network",
          status = "success",
          collapsible = TRUE,
          width = NULL,
          
          # d3 force directed network
          bnwidget::bnwidgetOutput("bnPlot")
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  dat <- shiny::reactive({
     if (input$net == 1) {
        dat <- learning.test
      } else if (input$net == 2) {
        dat <- asia
      } else if (input$net == 3) {
        dat <- alarm
      } else if (input$net == 4) {
        dat <- insurance
      } else if (input$net == 5) {
        dat <- hailfinder
      }
  })
  
  dag <- shiny::reactive({
      if (is.null(dat()))
        return(NULL)
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Learning network structure", value = 0)
      
      # Get the selected learning algorithm from the user and learn the network
      dag <- bnlearn::cextend(bnlearn::hc(dat()), strict = FALSE)
    })
  
   output$bnPlot <- bnwidget::renderBnwidget({
     if (is.null(dag()) || is.null(dat()))
       return(NULL)
     
     bnfit<-bnlearn::bn.fit(dag(), dat())
     probs <- lapply(bnfit, function(x) x$prob)
     cpds<- lapply(bnfit, function(x) x$prob)
     node_values <- lapply(bnfit, function(x) rownames(x$prob))
     nodes <- bnlearn::nodes(dag())
     links <- bnlearn::arcs(dag())
     
     bnwidget::bnwidget(
       nodes,
       links,
       cpds
     )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

