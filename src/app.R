library(shiny)

ui <- navbarPage(
    "CMSC 150 - Project",
    navbarMenu(
        "Generic Solvers",
        tabPanel("Quadratic Spline Interpolation"),
        tabPanel("Polynomial Regression")
    ),
    tabPanel("Simplex Method"),
    inverse = TRUE
)

server <- function(input, output){
}

shinyApp(ui = ui, server = server)