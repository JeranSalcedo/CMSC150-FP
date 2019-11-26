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
                    numericInput("prDegree", label = h3("Degree"), value = 0),
                    numericInput("prNumInput", label = h3("x"), value = 1)
                ),
                mainPanel(
                    tableOutput("prTable"),
                    textOutput("prFunction"),
                    textOutput("prSolution")
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
    
    prFunction <- reactive({
        if(is.null(prData())){
            return(NULL)
        }
        
        l = input$prDegree + 1
        dataMatrix = matrix(0, l, l + 1)
        outputString = "function(x) "
        
        for(y in 1:l){
            for(x in 1:l){
                for(i in 1:(nrow(prData()))){
                    dataMatrix[l + 1 - y, x] = dataMatrix[l + 1 - y, x] + (prData()[i, "x"]) ^ (x + y - 2)
                }
            }
            for(i in 1:(nrow(prData()))){
                dataMatrix[l + 1 - y, l + 1] = dataMatrix[l + 1 - y, l + 1] + (prData()[i, "y"]) * (prData()[i, "x"]) ^ (y - 1)
            }
        }
        
        for(i in 1:l){
            pivotElement = dataMatrix[i, i]
            for(x in i:(l + 1)){
                dataMatrix[i, x] = dataMatrix[i, x] / pivotElement
            }
            for(y in 1:l){
                if(y == i){
                    next
                }
                mul = dataMatrix[y, i]
                for(x in i:(l + 1)){
                    dataMatrix[y, x] = dataMatrix[y, x] - mul * dataMatrix[i, x]
                }
            }
        }
        
        for(i in l:1){
            outputString = paste(outputString, dataMatrix[i, l + 1])
            if(i == 2){
                outputString = paste(outputString, " * x + ", sep = "")
            } else if(i > 1){
                outputString = paste(outputString, " * x ^ ", i - 1, sep = "")
                outputString = paste(outputString, "+")
            }
        }
        
        outputString
    })
    
    output$prFunction <- renderPrint({
        prFunction()
    })
    
    output$prSolution <- renderPrint({
        if(is.null(prFunction())){
            return("Upload a csv file")
        }
        prF = eval(parse(text = prFunction()))
        prF(input$prNumInput)
    })
}

shinyApp(ui = ui, server = server)