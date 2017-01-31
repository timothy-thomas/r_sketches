

server <- function(input, output) {
  output$plot <- renderPlot({
    hist(rnorm(50), col = 'darkgray', border = 'white')
  })
  output$summary <- renderText("The summary page!")
  output$table <- renderTable(iris)
}

tabs <- list(plo=tabPanel("Plot", div(br(), plotOutput("plot"))),
             sum=tabPanel("Summary", div(br(), h4("My Summary"), verbatimTextOutput("summary"))),
             tab=tabPanel("Table", div(br(), h4("My Table"), tableOutput("table"))))

# print(tabs$plo)

ui <- fluidPage(
  mainPanel(
    br(),
    # tabsetPanel(tabs$plo, tabs$sum, tabs$tab)
    do.call(tabsetPanel, unname(tabs))
  )
)

shinyApp(ui = ui, server = server)