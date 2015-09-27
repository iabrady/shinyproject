library(shiny)
library(googleVis)
source("process.R")
sts <- sort(unique(as.character(df$State)))
states <- c("ALL", sts)
print(states)
shinyServer(
  function(input, output) {
    #output$oid1 <- renderPrint({input$id1})
    #output$oid2 <- renderPrint({input$id2})
    #output$odate <- renderPrint({input$date})
    values <- reactiveValues()
    values$states <- states
    output$statesControl <- renderUI({
      radioButtons('state', 'US STATES with Earthquake Report(Mag >= 3.0):', states, selected = "ALL")
    })
    
    output$dTable <- renderDataTable({
      usTable()
    } 
    )
    
    summaryTable <- reactive({
      summaryByState(state_stat, input$state)
    })
    
    output$smTable <- renderDataTable({
      summaryTable()
    })
    
    output$occurrencesByStates <- renderGvis({
      gvisGeoChart(state_stat, locationvar = "State", colorvar = "Occurrences", options = list(width =600, height = 400, region="US", displayMode="regions", resolution="provinces"))
    })
    
    output$statsByClass <- renderGvis({
      gvisMotionChart(class_stat, "Class", "Date", options = list(width = 600, height = 400))
    })
    
  }
)