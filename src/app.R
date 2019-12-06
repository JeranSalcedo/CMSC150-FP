library(shiny)
library(rhandsontable)

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
        tags$style("##smP1, #smP2, #smP3, #smW1, #smW2, #smW3, #smW4, #smW5, #smS1, #smS2, #smS3 { width: 200px; display: inline-block; }"),
        sidebarLayout(
            sidebarPanel(
                tags$h4("Shipping Cost"),
                rHandsontableOutput("smTable1"),
                tags$h4("Demands"),
                rHandsontableOutput("smTable2"),
                tags$h4("Supply"),
                rHandsontableOutput("smTable3")
            ),
            mainPanel(
                textOutput("smObjF"),
                verbatimTextOutput("smConstraints"),
                tableOutput("smOutputTable")
            )
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
    
    
    
    
    
    tableData1 = reactiveVal({
        table1 = data.frame(w1 = c(5, 6, 3), w2 = c(6, 7, 5), w3 = c(7, 8, 7), w4 = c(8, 9, 11), w5 = c(9, 10, 13))
        rownames(table1) <- c("p1", "p2", "p3")
        
        table1
    })
    tableData2 = reactiveVal({
        table2 = data.frame(w1 = c(100), w2 = c(100), w3 = c(100), w4 = c(100), w5 = c(100))
        rownames(table2) <- c("")
        
        table2
    })
    tableData3 = reactiveVal({
        table3 = data.frame(p1 = c(200), p2 = c(200), p3 = c(200))
        rownames(table3) <- c("")
        
        table3
    })
    
    # referenced from https://stackoverflow.com/questions/22272571/data-input-via-shinytable-in-r-shiny-application
    observe({
        if(!is.null(input$smTable1)){
            tableData1(hot_to_r(input$smTable1))
        }
        if(!is.null(input$smTable2)){
            tableData2(hot_to_r(input$smTable2))
        }
        if(!is.null(input$smTable3)){
            tableData3(hot_to_r(input$smTable3))
        }
    })
    
    output$smTable1 <- renderRHandsontable({
        if(!is.null(tableData1())){
            rhandsontable(tableData1(), stretchH = "all")
        }
    })
    
    output$smTable2 <- renderRHandsontable({
        if(!is.null(tableData2())){
            rhandsontable(tableData2(), stretchH = "all")
        }
    })
    
    output$smTable3 <- renderRHandsontable({
        if(!is.null(tableData3())){
            rhandsontable(tableData3(), stretchH = "all")
        }
    })
    
    output$smObjF <- renderText({
        objF = "Min Z = "
        
        for(x in 1:ncol(tableData1())){
            for(y in 1:nrow(tableData1())){
                objF = paste(objF, tableData1()[y, x], " * m(", rownames(tableData1())[y], ", ", colnames(tableData1())[x], ")", sep = "")
                if(x < ncol(tableData1()) || y < nrow(tableData1())){
                    objF = paste(objF, "+ ")
                }
            }
        }
        
        objF
    })
    
    output$smConstraints <- renderPrint({
        constraints = c()
        
        for(y in 1:nrow(tableData1())){
            constraint = ""
            for(x in 1:ncol(tableData1())){
                constraint = paste(constraint, "m(", rownames(tableData1())[y], ", ", colnames(tableData1())[x], ")", sep = "")
                if(x < ncol(tableData1())){
                    constraint = paste(constraint, "+ ")
                }
            }
            
            constraint = paste(constraint, "<=", tableData3()[1, y])
            constraints[y] = constraint
        }
        
        for(x in 1:ncol(tableData1())){
            constraint = ""
            for(y in 1:nrow(tableData1())){
                constraint = paste(constraint, "m(", rownames(tableData1())[y], ", ", colnames(tableData1())[x], ")", sep = "")
                if(y < nrow(tableData1())){
                    constraint = paste(constraint, "+ ")
                }
            }
            
            constraint = paste(constraint, ">=", tableData2()[1, x])
            constraints[x + 3] = constraint
        }
        
        for(y in 1:nrow(tableData1())){
            for(x in 1:ncol(tableData1())){
                constraints[(y - 1) * ncol(tableData1()) + x + 8] = paste("m(", rownames(tableData1())[y], ", ", colnames(tableData1())[x], ") >= 0", sep = "")
            }
        }
        
        constraints
    })
    
    output$smOutputTable <- renderTable({
        colLabels = c()
        objR = c()
        outputTable = matrix(c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 200, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 200, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 200, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 100, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 100, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, -1, 100), 8, 24, TRUE);
        
        
        for(x in 1:ncol(tableData1())){
            for(y in 1:nrow(tableData1())){
                colLabels[(x - 1) * nrow(tableData1()) + y] = paste("m(", rownames(tableData1())[y], ", ", colnames(tableData1())[x], ")", sep = "")
                objR[(x - 1) * nrow(tableData1()) + y] = tableData1()[y, x]
            }
        }
        
        for(i in 1:8){
            if(i < 4){
                outputTable[i, 24] = tableData3()[1, i]
            } else {
                outputTable[i, 24] = tableData2()[1, i - 3]
            }
            objR[i + 15] = 0
            colLabels[i + 15] = paste("s", i, sep = "")
        }
        colLabels[24] = "b"
        objR[24] = 0
        
        outputTable = rbind(outputTable, objR)
        colnames(outputTable) = colLabels
        
        outputTable
    })
}

shinyApp(ui = ui, server = server)