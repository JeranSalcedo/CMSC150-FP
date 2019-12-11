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
                    tableOutput("qsiTable"),
                    verbatimTextOutput("qsiEquations")
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
        tags$style("#smP1, #smP2, #smP3, #smW1, #smW2, #smW3, #smW4, #smW5, #smS1, #smS2, #smS3, #smPB, #smNB { width: 200px; display: inline-block; }"),
        sidebarLayout(
            sidebarPanel(
                tags$h4("Shipping Cost"),
                rHandsontableOutput("smTable1"),
                tags$h4("Demands"),
                rHandsontableOutput("smTable2"),
                tags$h4("Supply"),
                rHandsontableOutput("smTable3"),
                checkboxInput("smShow", "Show Iterations"),
                conditionalPanel(
                    condition = "input.smShow == 1",
                    tags$h4("Iterations"),
                    actionButton("smPB", "Previous Page"),
                    actionButton("smNB", "Next Page")
                )
            ),
            mainPanel(
                textOutput("smObjF"),
                verbatimTextOutput("smConstraints"),
                tags$h4("Optimal Solution"),
                tableOutput("smOS"),
                conditionalPanel(
                    condition = "input.smShow == 1",
                    textOutput("smIterations"),
                    tableOutput("smOutputTable"),
                    tags$h4("Basic Solution"),
                    tableOutput("smBS")
                )
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
    
    output$qsiEquations <- renderPrint({
        if(is.null(qsiData())){
            return(NULL)
        }
        dataMatrix = matrix(0, 3 * (nrow(qsiData()) - 1) - 1, 3 * (nrow(qsiData()) - 1))
        
        count = 1
        final = c()
        
        for(i in 3:nrow(qsiData())){
            final[count] = paste(
                qsiData()[i - 1, "x"] ^ 2, " * a", i - 2, " + ",
                qsiData()[i - 1, "x"], " * b", i - 2, " + ",
                "c", i - 2, " = ", qsiData()[i - 1, "y"],
                sep = ""
            )
            dataMatrix[count, 3 * (i - 3)] = qsiData()[i - 1, "x"] ^ 2
            dataMatrix[count, 3 * (i - 3) + 1] = qsiData()[i - 1, "x"]
            dataMatrix[count, 3 * (i - 3) + 2] = 1
            dataMatrix[count, 3 * (nrow(qsiData()) - 1)] = qsiData()[i - 1, "y"]
            final[count + 1] = paste(
                qsiData()[i - 1, "x"] ^ 2, " * a", i - 1, " + ",
                qsiData()[i - 1, "x"], " * b", i - 1, " + ",
                "c", i - 1, " = ", qsiData()[i - 1, "y"],
                sep = ""
            )
            dataMatrix[count + 1, 3 * (i - 2)] = qsiData()[i - 1, "x"] ^ 2
            dataMatrix[count + 1, 3 * (i - 2) + 1] = qsiData()[i - 1, "x"]
            dataMatrix[count + 1, 3 * (i - 2) + 2] = 1
            dataMatrix[count + 1, 3 * (nrow(qsiData()) - 1)] = qsiData()[i - 1, "y"]
            final[count + 2] = paste(
                qsiData()[i - 1, "x"] * 2, " * a", i - 2, " + ",
                "b", i - 2, " = ",
                qsiData()[i - 1, "x"] * 2, " * a", i - 1, " + ",
                "b", i - 1,
                sep = ""
            )
            dataMatrix[count + 2, 3 * (i - 3)] = qsiData()[i - 1, "x"] * 2
            dataMatrix[count + 2, 3 * (i - 3) + 1] = 1
            dataMatrix[count + 2, 3 * (i - 2)] = -qsiData()[i - 1, "x"] * 2
            dataMatrix[count + 2, 3 * (i - 2) + 1] = -1
            count = count + 3
        }
        
        final[count] = paste(
            qsiData()[1, "x"] ^ 2, " * a1 + ",
            qsiData()[1, "x"], " * b1 + ",
            "c1 = ", qsiData()[1, "y"],
            sep = ""
        )
        dataMatrix[count, 0] = qsiData()[1, "x"] ^ 2
        dataMatrix[count, 1] = qsiData()[1, "x"]
        dataMatrix[count, 2] = 1
        dataMatrix[count, 3 * (nrow(qsiData()) - 1)] = qsiData()[1, "y"]
        final[count + 1] = paste(
            qsiData()[nrow(qsiData()), "x"] ^ 2, " * a", nrow(qsiData()) - 1, " + ",
            qsiData()[nrow(qsiData()), "x"], " * b", nrow(qsiData()) - 1, " + ",
            "c", nrow(qsiData()) - 1, " = ", qsiData()[nrow(qsiData()), "y"],
            sep = ""
        )
        dataMatrix[count + 1, 3 * (nrow(qsiData()) - 1) - 3] = qsiData()[nrow(qsiData()), "x"] ^ 2
        dataMatrix[count + 1, 3 * (nrow(qsiData()) - 1) - 2] = qsiData()[nrow(qsiData()), "x"]
        dataMatrix[count + 1, 3 * (nrow(qsiData()) - 1) - 1] = 1
        dataMatrix[count + 1, 3 * (nrow(qsiData()) - 1)] = qsiData()[nrow(qsiData()), "y"]
        count = count + 2
        
        final[count] = "a1 = 0"
        
        for(i in 1:(ncol(dataMatrix) - 1)){
            if(i < ncol(dataMatrix) - 2){
                highestInd = 1
                highestVal = abs(dataMatrix[i, i])
                
                for(y in 1:nrow(dataMatrix[i:nrow(dataMatrix), ])){
                    if(abs(dataMatrix[i:nrow(dataMatrix), ][y, i]) > highestVal){
                        highestInd = y
                        highestVal = abs(dataMatrix[i:nrow(dataMatrix), ][y, i])
                    }
                }
                
                dataMatrix[i:nrow(dataMatrix), ] = dataMatrix[i:nrow(dataMatrix), ][c(highestInd, (1:(nrow(dataMatrix) - i + 1))[-highestInd]), ]
            }
            
            pivotElement = dataMatrix[i, i]
            for(x in i:ncol(dataMatrix)){
                dataMatrix[i, x] = dataMatrix[i, x] / pivotElement
            }
            for(y in 1:nrow(dataMatrix)){
                if(y == i){
                    next
                }
                mul = dataMatrix[y, i]
                for(x in i:ncol(dataMatrix)){
                    dataMatrix[y, x] = dataMatrix[y, x] - mul * dataMatrix[i, x]
                }
            }
        }
        
        solutionVec = c(0, dataMatrix[, ncol(dataMatrix)])
        for(i in 1:(length(solutionVec) / 3)){
            print(paste("For", qsiData()[i, "x"], "< x <", qsiData()[i + 1, "x"]))
            print(paste("f(x) = ", solutionVec[(i - 1) * 3 + 1], " * x ^ 2 + ", solutionVec[(i - 1) * 3 + 2], " * x + ", solutionVec[(i - 1) * 3 + 3], sep = ""))
            
            if(input$qsiNumInput >= qsiData()[i, "x"] && input$qsiNumInput <= qsiData()[i + 1, "x"]){
                print(paste("f(", input$qsiNumInput, ") = ", solutionVec[(i - 1) * 3 + 1] * input$qsiNumInput ^ 2 + solutionVec[(i - 1) * 3 + 2] * input$qsiNumInput + solutionVec[(i - 1) * 3 + 3], sep = ""))
            }
        }
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
    
    pageNumber = reactiveVal({
        0
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
    
    solutions <- reactive({
        colLabels = c()
        objR = c()
        basicSolutionTable = matrix(0, 1, 23)
        initialTableau = matrix(c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 200, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 200, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 200, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 100, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 100, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, -1, 0, 100), 8, 25, TRUE)
        for(x in 1:ncol(tableData1())){
            for(y in 1:nrow(tableData1())){
                colLabels[(x - 1) * nrow(tableData1()) + y] = paste("m(", rownames(tableData1())[y], ", ", colnames(tableData1())[x], ")", sep = "")
                objR[(x - 1) * nrow(tableData1()) + y] = tableData1()[y, x]
            }
        }
        
        for(i in 1:8){
            initialTableau[i, 24] = 0
            if(i < 4){
                initialTableau[i, 25] = tableData3()[1, i]
            } else {
                initialTableau[i, 25] = tableData2()[1, i - 3]
            }
            objR[i + 15] = 0
            colLabels[i + 15] = paste("s", i, sep = "")
        }
        colnames(basicSolutionTable) = colLabels
        colLabels[24] = "Z"
        colLabels[25] = "Answer"
        objR[24] = 1
        objR[25] = 0
        
        initialTableau = rbind(initialTableau, objR)
        colnames(initialTableau) = colLabels
        
        final = list()
        final[[1]] = initialTableau
        
        stop = FALSE
        tableau = initialTableau
        iteration = 1
        while(!stop){
            pivotRow = 0
            pivotElement = 0
            tRatio = NA
            feasible = TRUE
            set = FALSE
            
            for(x in 1:(ncol(tableau) - 2)){
                counter = 0
                for(y in 1:nrow(tableau)){
                    if(counter > 1){
                        if(set){
                            set = FALSE
                            feasible = TRUE
                            pivotRow = 0
                        }
                        basicSolutionTable[1, x] = 0
                        break
                    }
                    if(tableau[y, x] != 0){
                        basicSolutionTable[1, x] = tableau[y, x] * tableau[y, ncol(tableau)]
                        if(!set && basicSolutionTable[1, x] < 0){
                            feasible = FALSE
                            set = TRUE
                            pivotRow = y
                        }
                        
                        counter = counter + 1
                    }
                }
            }
            final[[(iteration - 1) * 2 + 2]] = basicSolutionTable
            
            if(feasible){
                stop = TRUE
                leastVal = 0
                for(i in 1:(ncol(tableau) - 1)){
                    if(tableau[nrow(tableau), i] < 0){
                        stop = FALSE
                        if(tableau[nrow(tableau), i] < leastVal){
                            leastVal = tableau[nrow(tableau), i]
                            j = i
                        }
                    }
                }
                if(stop){
                    break
                }
            } else {
                for(i in 1:(ncol(tableau) - 1)){
                    if(tableau[pivotRow, i] > 0){
                        j = i
                        break
                    }
                }
            }
            
            for(y in 1:(nrow(tableau) - 1)){
                if(tableau[y, j] <= 0){
                    next
                }
                if(is.na(tRatio) || tableau[y, ncol(tableau)] / tableau[y, j] <= tRatio){
                    tRatio = tableau[y, ncol(tableau)] / tableau[y, j]
                    pivotRow = y
                    pivotElement = tableau[y, j]
                }
            }

            for(x in 1:ncol(tableau)){
                tableau[pivotRow, x] = tableau[pivotRow, x] / pivotElement
            }
            for(y in 1:nrow(tableau)){
                if(y == pivotRow){
                    next
                }
                pivotElement = tableau[y, j]
                for(x in 1:ncol(tableau)){
                    tableau[y, x] = tableau[y, x] - pivotElement * tableau[pivotRow, x]
                }
            }
            
            final[[iteration * 2 + 1]] = tableau

            iteration = iteration + 1
        }
        
        final
    })
    
    output$smOS <- renderTable({
        table = solutions()[[length(solutions())]][1, 1:15]
        table = c(-1 * solutions()[[length(solutions()) - 1]][nrow(solutions()[[length(solutions()) - 1]]), ncol(solutions()[[length(solutions()) - 1]])], table)
        names(table)[1] = "Z"
        table = t(table)
    })
    
    output$smOutputTable <- renderTable({
        solutions()[[pageNumber() * 2 + 1]]
    })
    
    output$smBS <- renderTable({
        solutions()[[pageNumber() * 2 + 2]]
    })
    
    observeEvent(input$smPB, {
        prevVal = pageNumber()
        if(prevVal > 0){
            pageNumber(prevVal - 1)
        }
    })
    
    observeEvent(input$smNB, {
        prevVal = pageNumber()
        if(prevVal < length(solutions()) / 2 - 1){
            pageNumber(prevVal + 1)
        }
    })
    
    output$smIterations <- renderText({
        if(pageNumber() == 0){
            "Initial Tableau"
        } else {
            paste("Iteration", pageNumber())
        }
    })
}

shinyApp(ui = ui, server = server)