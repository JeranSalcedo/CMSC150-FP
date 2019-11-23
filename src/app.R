library(shiny)

ui <- navbarPage(
    "CMSC 150 - Project",
    navbarMenu(
        "Generic Solvers",
        tabPanel(
            "Quadratic Spline Interpolation",
            sidebarLayout(
                sidebarPanel(
                    fileInput(
                        "qsiFileInput",
                        label = h3("Upload CSV"),
                        accept = c("text/csv")
                    ),
                    numericInput("qsiNumInput", label = h3("x"), value = 1)
                ),
                mainPanel(
                    tableOutput("qsiTable")
                )
            )
        ),
        tabPanel(
            "Polynomial Regression",
            sidebarLayout(
                sidebarPanel(
                    fileInput(
                        "prFileInput",
                        label = h3("Upload CSV"),
                        accept = c("text/csv")
                    ),
                    numericInput("prNumInput", label = h3("x"), value = 1)
                ),
                mainPanel(
                    tableOutput("prTable"),
                    tableOutput("prSolution")
                )
            )
        )
    ),
    tabPanel(
        "Simplex Method",
        sidebarLayout(
            sidebarPanel("side3"),
            mainPanel("main3")
        )
    ),
    inverse = TRUE
)

server <- function(input, output){
    qsiData <- reactive({
        csvFile <- input$qsiFileInput
        
        if(is.null(csvFile)){
            return(NULL)
        }
        read.table(file = csvFile$datapath, sep = ',', col.names = c("x", "y"))
    })
    
    output$qsiTable <- renderTable({
        if(is.null(qsiData())){
            return(NULL)
        }
        
        qsiData()
    })
    
    prData <- reactive({
        csvFile <- input$prFileInput
        
        if(is.null(csvFile)){
            return(NULL)
        }
        read.table(file = csvFile$datapath, sep = ',', col.names = c("x", "y"))
    })
    
    output$prTable <- renderTable({
        if(is.null(prData())){
            return(NULL)
        }
        
        prData()
    })
    
    output$prSolution <- renderTable({
        if(is.null(prData())){
            return(NULL)
        }
        
        dataMatrix = matrix(1, nrow(prData()) - 1, nrow(prData()) - 1)
        
        for(y in 1:nrow(prData())){
            for(x in 1:nrow(prData())){
                
            }
        }
    }, colnames = FALSE)
}

shinyApp(ui = ui, server = server)