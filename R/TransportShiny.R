TransportShiny <- function () {

  library(shiny)
  library(lpSolve)
  library(rhandsontable)

  #Raktárak és boltok száma, valamint azok elnevezései
  row <- c()
  col <- c()
  columnNames <- c()
  rowNames <- c()

  #Kiszámolja, hol lehetne így többet szállítani, hogy az összköltségem csökkenjen.
  negativeCost <- function(matrix, costs){

    #Column dataframe létrehozása, ami olyan raktár-bolt párokat tartalmaz, ahol történik szállítás.
    options(stringsAsFactors = FALSE)
    Columns <- data.frame()
    RightSide <- c(0)
    for(i in 1:nrow(matrix)) {
      for(j in 1:ncol(matrix)) {
        if(matrix[i,j]>0) {
          Columns <- rbind(Columns,c(rownames(matrix)[i], colnames(matrix)[j]))
          RightSide <- c(RightSide,costs[i,j])
        }
      }
    }

    #Egyenletrendszerhez szükséges mátrix létrehozása
    EquationsSystemMatrix <- matrix(rep(0,(nrow(matrix)+ncol(matrix))^2),
                                    ncol = (nrow(matrix)+ncol(matrix)))
    colnames(EquationsSystemMatrix) <- c(rownames(matrix),colnames(matrix))
    EquationsSystemMatrix[1,1] <- 1
    for(i in 1:nrow(Columns)){
      EquationsSystemMatrix[i+1,Columns[i,1]] <- 1
      EquationsSystemMatrix[i+1,Columns[i,2]] <- 1
    }

    print(det(EquationsSystemMatrix))
    #Egyenletrendszer megoldása és az árnyékárakat tartalmazó mátrix létrehozása
    if (det(EquationsSystemMatrix) != 0){
      x <- solve(EquationsSystemMatrix,RightSide)
      solution <- c()
      for(i in 1:length(rownames(matrix))){
        for(j in (length(rownames(matrix))+1):(length(rownames(matrix)) + length(colnames(matrix)))){
          solution <- c(solution, ifelse(x[i]+x[j] >= 0, "-", x[i] + x[j]))
        }
      }
      names(solution) <- NULL
      solution <- matrix(solution, nrow=nrow(matrix), byrow=TRUE)
      rownames(solution) <- rownames(matrix)
      colnames(solution) <- colnames(matrix)

      return(solution)
    } else {
      solution <- matrix(rep("-", nrow(matrix) * ncol(matrix)), nrow=nrow(matrix), byrow=TRUE)
      rownames(solution) <- rownames(matrix)
      colnames(solution) <- colnames(matrix)
      return(solution)
    }
  }

  #Megvizsgáljuk, hogy a bemeneti táblákban csak számok találhatóak-e.
  validateInputs <- function(costs, capacities, needs){
    notValidInput <- FALSE
    for(i in 1:nrow(costs)){
      for(j in 1:ncol(costs)){
        if(is.na(costs[i,j])){
          notValidInput <- TRUE
        }
      }
    }
    for (i in 1:length(capacities)) {
      if(is.na(capacities[i])){
        notValidInput <- TRUE
      }
    }
    for (i in 1:length(needs)) {
      if(is.na(needs[i])){
        notValidInput <- TRUE
      }
    }
    return(notValidInput)
  }

  #Az alkalmazás felületi elemeit meghatározó részek
  ui <- fluidPage(
    pageWithSidebar(

      headerPanel("Transport shiny."),
      sidebarPanel( id = "sidebar",
        div(id="parameters",
            textOutput("numberofstorage"),
            textInput("storage", label = "", width = "60px"),
            textOutput("numberofstore"),
            textInput("store", label = "", width = "60px")),
        actionButton("ok","OK"),
        div(id = "errorMessage", style="margin-top: 5px; color: red", textOutput("errorMessage"))
      ),
      mainPanel(
        div(style="display: inline-block; margin-right: 20px",
          textOutput("costTableText"),
          rHandsontableOutput("costTable")),
        div(style="display: inline-block; margin-right: 20px",
          textOutput("capacityTableText"),
          rHandsontableOutput("capacityTable")),
        div(style="display: inline-block; margin-right: 20px",
          textOutput("needTableText"),
          rHandsontableOutput("needTable")
          ),
        div(id="solvebutton", style="margin-top: 5px; margin-bottom: 20px"),
        textOutput("solutionText"),
        div(style="margin-top: 5px; margin-bottom: 5px", rHandsontableOutput("result")),
        textOutput("solutionValue"),
        div(style="margin-top: 5px; margin-bottom: 5px", rHandsontableOutput("costresult")),
        textOutput("interpreterResult"),
        width = 10
      )
    )
  )

  server <- function(input, output, session) {

    #A boltok és raktárak számának meghatározása
    observeEvent(input$ok,{
      if(!is.na(as.numeric(input$storage)) && !is.na(as.numeric(input$store))
         && as.numeric(input$storage) > 0 && as.numeric(input$store) > 0 &&
         as.numeric(input$storage) %% 1 == 0 && as.numeric(input$store) %% 1 == 0){
        row <<- as.numeric(input$storage)
        col <<- as.numeric(input$store)

        #Oszlopnevek meghatározása
        for(i in 1:col)
          columnNames <<- c(columnNames, paste0("B",i))
        #Sornevek meghatározása
        for(i in 1:row)
          rowNames <<- c(rowNames, paste0("R",i))

        x <- matrix(rep(0,row*col),nrow=row, ncol=col)
        colnames(x) <- columnNames
        rownames(x) <- rowNames
        costs <- data.frame(x)
        x <- matrix(rep(0,row), nrow=1, ncol=row)
        colnames(x) <- rowNames
        capacities <- data.frame(x)
        x <- matrix(rep(0,col), nrow=1, ncol=col)
        colnames(x) <- columnNames
        needs <- data.frame(x)

        output$costTable <- renderRHandsontable({
          rhandsontable(costs, readOnly = FALSE)
        })
        output$capacityTable <- renderRHandsontable({
          rhandsontable(capacities, readOnly = FALSE, rowHeaders = NULL)
        })
        output$needTable <- renderRHandsontable({
          rhandsontable(needs, readOnly = FALSE, rowHeaders = NULL)
        })

        insertUI("#solvebutton", ui = actionButton("solve","Solve"))
        removeUI(".col-sm-4")
        output$costTableText <- renderText({"Table of costs:"})
        output$capacityTableText <- renderText({"Capacities of storages:"})
        output$needTableText <- renderText({"Needs of stores:"})
      } else {
        #Hibaüzenetek kiírása
        output$errorMessage <- renderText({
          if(is.na(as.numeric(input$storage)) || as.numeric(input$storage) %% 1 == 0){
            "Please give a positive integer to number of storage."
          } else if(is.na(as.numeric(input$store)) || as.numeric(input$store) %% 1 == 0){
            "Please give a positive integer to number of store."
          } else if(as.numeric(input$storage) <= 0 || as.numeric(input$store) <= 0){
            "The number of storage and store must be greater than zero!"
          } else {
            ""
          }
        })
      }
    })

    #Megoldjuk a bemenet alapján a szállítási feladatot
    observeEvent(input$solve,{
      #A input táblák átalakítása R objektumokká (itt mátrixokká visszalakítja)
      costMatrix <- hot_to_r(input$costTable)
      costMatrix <- data.matrix(costMatrix)
      capacityMatrix <- hot_to_r(input$capacityTable)
      capacityMatrix <- data.matrix(capacityMatrix)
      capacityVector <- capacityMatrix[1,]
      needMatrix <- hot_to_r(input$needTable)
      needMatrix <- data.matrix(needMatrix)
      needVector <- needMatrix[1,]

      notValidInput <- validateInputs(costMatrix, capacityVector, needVector)
      result <- c()
      solutionValue <- c()
      #Ha az inputok validak, akkor megoldom  feladatot és kiszámolom az árnyékárakat.
      if(!notValidInput){
        capacity.signs <- rep ("<=", row)
        need.signs <- rep (">=", col)
        result <- lp.transport (costMatrix, "min", capacity.signs, capacityVector,
                                need.signs, needVector)
        matrix <- result$solution
        colnames(matrix) <- columnNames
        rownames(matrix) <- rowNames
        x <- data.frame(matrix)
        solutionValue <- result$objval
        result <- negativeCost(matrix, costMatrix)

      }
      output$result <- renderRHandsontable({
        validate(
          need(!notValidInput, "Please give only numeric values into tables!")
        )
        rhandsontable(x, readOnly = TRUE)
      })
      if(!is.null(solutionValue)){
        output$solutionText <- renderText({"Solution:"})
        output$solutionValue <- renderText({
          paste("Cost of the transport is",solutionValue)
        })
      }
      if(!is.null(result)){
        output$costresult <- renderRHandsontable({
          rhandsontable(result, readOnly = TRUE)
        })
        output$interpreterResult <- renderText({
          "In the table where negative values are, you can decrease the cost of transport.
          How? Increase the capacity and need on that routes."
        })
      } else {
        output$costresult <- renderRHandsontable({})
        output$interpreterResult <- renderText({
          ""
        })
      }
    })

    observe({
      output$numberofstorage <- renderText({"Number of storage: "})
      output$numberofstore <- renderText({"Number of store: "})
    })

  }

shinyApp(ui=ui,server=server)
}
