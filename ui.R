require(markdown)
shinyUI(
  navbarPage(
    "Earthquake Set Visualizer", 
    # multi-page user-interface that includes a navigation bar.
    tabPanel("Explore the Data",
    #headerPanel("Earthquake Visualizer (08-28-2015 ~ 09-27-2015)"),
      sidebarPanel(
        uiOutput("statesControl")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(p(icon("table"), "Dataset"),
                   dataTableOutput(outputId="dTable")
          ),
          tabPanel(p(icon("line-chart"), "Visualize the Data"), 
                   h3('Earthquake by State Summary'),
                   dataTableOutput(outputId="smTable"),
                   h3('Occurrences Overview'),
                   htmlOutput("occurrencesByStates"),
                   h3('Stats by Class'),
                   htmlOutput("statsByClass")
                   )
          )
      )
    ),
    tabPanel("About",
             mainPanel(
               includeMarkdown("about.md")
             )
    ) 
  )
)