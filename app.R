#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(bnwidget)
library(gRain)
library(shiny)
library(bnlearn)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Bayesian network visualization"),
   verbatimTextOutput("selectedNode"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     shiny::selectInput(
       inputId = "net",
       h5("Choose dataset:"),
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
          width = 1000,
          height = 800,
          # d3 force directed network
          bnwidget::bnwidgetOutput("bnPlot")
        ),
        width = 12
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  rv <- reactiveValues()
  
  output$selectedNode <- shiny::renderPrint({
    input$selectedNode
  })
  #output$evidence <- shiny::renderPrint({
  #  input$newEvidence
  #})
  
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
  
  observeEvent(input$newEvidence, {
     if(is.null(input$newEvidence))
     {
       rv$jtree <- compile(as.grain(bn()))
      }else
      {
        newEvidence <- strsplit(input$newEvidence, "$", fixed=TRUE)[[1]]
        rv$jtree <- setFinding(rv$jtree, nodes = newEvidence[1], states = newEvidence[2])
      }
   },ignoreNULL = FALSE)
   
   
   bn <- shiny::reactive({
     bn <- bnlearn::bn.fit(dag(), dat())
   })
  
   output$bnPlot <- bnwidget::renderBnwidget({
     if (is.null(dag()) || is.null(dat()) || is.null(bn()) || is.null(rv$jtree))
       return(NULL)

     probs <- lapply(bn(), function(x) x$prob)
     cpds<- lapply(bn(), function(x) x$prob)
     node_values <- lapply(bn(), function(x) rownames(x$prob))
     nodes <- bnlearn::nodes(dag())
     links <- bnlearn::arcs(dag())
     marginal_probs = querygrain(rv$jtree)
     evidence <-getEvidence(rv$jtree)
     evidence_probs<-lapply(seq_along(evidence$nodes), 
                            function(i){ 
                              node <- evidence$nodes[i]
                              probs <- rep(0, length(node_values[[node]])); 
                              probs[match(evidence$hard.state[i], node_values[[node]])] <- 1; 
                              probs})
     evidence_probs <- setNames(evidence_probs, evidence$nodes)
     marginal_probs <- append(evidence_probs, marginal_probs)
     
     bnwidget::bnwidget(
       nodes,
       links,
       cpds,
       marginal_probs = marginal_probs
     )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

