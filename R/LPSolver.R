#'Solver to Linear problem
#'
#'This function helps to solve a linear problem
#'@param
#'@return Start a shiny apps to add the problem and solve it.
#'@export

LPSolver <- function () {
  library(linprog)
  library(shiny)

  startUp <- function(column) {
  model.obj <<- c()
  model.con <<- matrix(nrow = 0, ncol = column)
  model.dir <<- c()
  model.rhs <<- c()
  lpSolution <<- c()

  options(stringsAsFactors = FALSE)
  solution.variables <<- data.frame()
  solution.constraints <<- data.frame()
  range.variables <<- data.frame()
  range.constraints <<- data.frame()
  }

  calculateSolutionReport <- function() {

  kezdoMatrix <- model.con
  nullvektor <- rep(0, nrow(model.con))
  for(i in 1:length(model.dir)) {
    if (model.dir[i] == "<=") {
      aktualis <- nullvektor
      aktualis[i] = 1
      kezdoMatrix <- cbind(kezdoMatrix, aktualis)
    } else if(model.dir[i] == ">="){
      aktualis <- nullvektor
      aktualis[i] = -1
      kezdoMatrix <- cbind(kezdoMatrix, aktualis)
    }
  }
  colnames(kezdoMatrix) <- c()

  tobbivaltozo <- bazisvaltozok <- reduced.costs <- c()
  bazisvaltozok.coefficients <- tobbivaltozo.coefficients <- valtozok.coefficients <- c()
  bazisvaltozok.value <- tobbivaltozo.value <- c()
  valtozok <- c(lpSolution$solution, lpSolution$con[which(model.dir != "="),4])
  #A slack változók közül csak azokat válogatom ki, amelyek olyan feltételekhez tartoznak,
  #hogy nincs bennük '=' jel
  for(i in 1:length(valtozok)){
    if(valtozok[i] != 0){
      bazisvaltozok <- c(bazisvaltozok, i)
      bazisvaltozok.coefficients <- c(bazisvaltozok.coefficients, ifelse(i <= length(lpSolution$solution), model.obj[i], 0))
      bazisvaltozok.value <- c(bazisvaltozok.value, valtozok[i])
    } else {
      tobbivaltozo <- c(tobbivaltozo, i)
      tobbivaltozo.coefficients <- c(tobbivaltozo.coefficients, ifelse(i <= length(lpSolution$solution), model.obj[i], 0))
      tobbivaltozo.value <- c(tobbivaltozo.value, valtozok[i])
    }
    valtozok.coefficients <- c(valtozok.coefficients, ifelse(i <= length(lpSolution$solution), model.obj[i], 0))
  }

  eredmenyMatrix <- matrixszamolashoz <- c()
  for(i in 1:length(bazisvaltozok)){
    matrixszamolashoz <- cbind(matrixszamolashoz, kezdoMatrix[,bazisvaltozok[i]])
  }
  for (i in 1:ncol(kezdoMatrix)) {
    x <- solve(matrixszamolashoz, kezdoMatrix[,i])
    eredmenyMatrix <- cbind(eredmenyMatrix, x)
    colnames(eredmenyMatrix)[i] <- paste0("x", i)
  }
  for(i in 1:length(valtozok)){
    if(valtozok[i] != 0){
      reduced.costs <- c(reduced.costs, 0)
    } else {
      reduced.costs <- c(reduced.costs, sum(bazisvaltozok.coefficients * eredmenyMatrix[,paste0("x",i)]) - valtozok.coefficients[i])
    }
  }
  for (i in 1:length(valtozok)) {
    if(i <= length(model.obj)) {
      if (valtozok[i] == 0) {
        range.variables <<- rbind(range.variables, c(paste0("x",i), valtozok.coefficients[i], reduced.costs[i], Inf))
      } else {
        for(j in 1:length(bazisvaltozok)) {
          if(bazisvaltozok[j] == i) {
            toDecrease <- toIncrease <- increase <- decrease <- c()
            for(k in tobbivaltozo) {
              if (eredmenyMatrix[j,k] > 0) {
                toDecrease <- c(toDecrease, - reduced.costs[k] / eredmenyMatrix[j,k])
              } else if (eredmenyMatrix[j,k] < 0) {
                toIncrease <- c(toIncrease, - reduced.costs[k] / eredmenyMatrix[j,k])
              }
            }
            if (length(toIncrease) != 0) {
              increase <- min(toIncrease)
            } else {
              increase <- Inf
            }
            if (length(toDecrease) != 0) {
              decrease <- max(toDecrease)
            } else {
              decrease <- Inf
            }
            range.variables <<- rbind(range.variables, c(paste0("x", i), valtozok.coefficients[i], increase, abs(decrease)))
          }
        }
      }
      solution.variables <<- rbind(solution.variables, c(paste0("x", i), valtozok[i], reduced.costs[i]))
    } else {

      toDecrease <- toIncrease <- increase <- decrease <- c()
      for(k in 1:length(eredmenyMatrix[,i])) {
        if (eredmenyMatrix[k,i] > 0) {
          toDecrease <- c(toDecrease, - bazisvaltozok.value[k] / eredmenyMatrix[k,i])
        } else if (eredmenyMatrix[k,i] < 0) {
          toIncrease <- c(toIncrease, - bazisvaltozok.value[k] / eredmenyMatrix[k,i])
        }
      }
      if (length(toIncrease) != 0) {
        increase <- min(toIncrease)
      } else {
        increase <- Inf
      }
      if (length(toDecrease) != 0) {
        decrease <- max(toDecrease)
      } else {
        decrease <- Inf
      }
      range.constraints <<- rbind(range.constraints, c(paste0("f", i-length(model.obj)), model.rhs[i-length(model.obj)], increase, abs(decrease)))

      solution.constraints <<- rbind(solution.constraints, c(paste0("f", i-length(model.obj)), valtozok[i], reduced.costs[i]))
    }
  }
  if (ncol(solution.variables) == 3) {
    colnames(solution.variables) <<- c("Variables","Value", "Reduced cost")
  }
  if (ncol(solution.constraints) == 3) {
    colnames(solution.constraints) <<- c("Constraints","Slack value", "Dual price")
  }
  if (ncol(range.variables) == 4) {
    colnames(range.variables) <<- c("Variables","Current coefficient", "Allowable increase", "Allowable decrease")
  }
  if (ncol(range.constraints) == 4) {
    colnames(range.constraints) <<- c("Constraints", "Current rhs", "Allowable increase", "Allowable decrease");
  }
  }

  numberOfConstraints <- 1
  numberOfVariables <- 0

  ui <- fluidPage(

  headerPanel(div(id = "title", div(id = "titleText", "Linear problem"))),

  mainPanel(

    div(id = "numberOfVariables",
        textOutput("variablesText", inline = TRUE),
        textInput("variablesInput",label="", width = "70px"),
        actionButton("variables","Add"),
        div(id = "errorMessage", style="margin-top: 5px; color: red", textOutput("errorMessage"))
    ),

    div(id = "solution",
      textOutput("solutionText"),
      tableOutput("solutionValueTable"),
      tableOutput("solutionConstraintTable"),
      textOutput("rangesVariablesText"),
      tableOutput("rangesVariablesTable"),
      textOutput("rangesConstraintText"),
      tableOutput("rangesConstraintTable"),
      div(id = "reset")
    ), width = 12
  )
  )

  server <- function(input, output, session) {

  inputsValid <- function() {
    isNotZeroObjFuncCoeff = FALSE
    for(i in 1:numberOfVariables) {
      if(is.na(as.numeric(input[[paste0("objFuncCoeff",i)]]))) {
        return (paste0("Empty or not a number value in the objective function's coefficients! (at x", i, ")"))
      }
      if(as.numeric(input[[paste0("objFuncCoeff",i)]]) != 0) {
        isNotZeroObjFuncCoeff = TRUE
      }
    }
    if(isNotZeroObjFuncCoeff == FALSE){
      return ("At least one objective function's coefficient must be not zero!")
    }

    for (i in 1:numberOfConstraints) {
      for (j in 1:numberOfVariables) {
        if (is.na(as.numeric(input[[paste0("x",j,"_value_",i)]]))) {
          return (paste0("Empty or not a number value in the ", i, ". constraint at x",j,"!"))
        }
      }
      if (is.na(as.numeric(input[[paste0("rs_value_",i)]]))) {
        return (paste0("Empty or not a number value in the ", i, ". constraint at right side value!"))
      }
    }
    return (NULL);
  }

  observeEvent(input$variables,{
    if(is.na(as.numeric(input$variablesInput)) || as.numeric(input$variablesInput) <= 0){
      output$errorMessage <- renderText({
        "Variables number must be a natural number (greater than ZERO)!"
      })
    } else {
      numberOfVariables <<- as.numeric(input$variablesInput)
      removeUI("#numberOfVariables")
      insertUI("#solution", where = "beforeBegin", ui =
                 div(id = "ObjectiveFunctionInputFields",
                 textOutput("objectfunctText")
                 )
              )
      for(i in 1:numberOfVariables){
        insertUI("#ObjectiveFunctionInputFields", where = "beforeEnd", ui =
          div(style="display: inline-block",textInput(paste0("objFuncCoeff",i),label="", width = "70px")))
        insertUI("#ObjectiveFunctionInputFields", where = "beforeEnd", ui =
          div(style="display: inline-block; margin: 2px",textOutput(paste0("objFuncx",i), inline = TRUE))
        )
      }
      insertUI("#ObjectiveFunctionInputFields", where = "beforeEnd", ui =
        div(style="display: inline-block",selectInput("objFuncType", "", choices = c("max","min"), selected = "max", width = "70px"))
      )

      insertUI("#ObjectiveFunctionInputFields", where = "afterEnd", ui=
                 div(id = "constraintsInputFields", style="display: block",
                     div(style="margin-top: 5px", textOutput("constraintsText"))
                 ))
      for(i in 1:numberOfVariables) {
        insertUI("#constraintsInputFields", where = "beforeEnd", ui =
                   div(style="display: inline-block",textInput(paste0("x",i,"_value_",numberOfConstraints), label = "", width = "70px"))
        )
        insertUI("#constraintsInputFields", where = "beforeEnd", ui =
                   div(style="display: inline-block; margin: 2px",textOutput(paste0("x", i), inline = TRUE))
        )
      }
      insertUI("#constraintsInputFields", where = "beforeEnd", ui =
                 div(style="display: inline-block; margin: 2px", selectInput(paste0("comparison_sign_", numberOfConstraints), label = "", choices = c("=","<=",">="), width ='65px'))
      )
      insertUI("#constraintsInputFields", where = "beforeEnd", ui =
                 div(style="display: inline-block; margin: 2px", textInput(paste0("rs_value_",numberOfConstraints), label = "", width = '70px'))
      )

      insertUI("#constraintsInputFields", where = "afterEnd", ui =
                 div(id = "errorMessage", style="margin-top: 5px; color: red", textOutput("errorMessage")))
      insertUI("#constraintsInputFields", where = "afterEnd", ui =
                 div(style="display: inline-block", actionButton("solve","Solve")))
      insertUI("#constraintsInputFields", where = "afterEnd", ui =
                 div(style="display: inline-block; margin-right: 5px", actionButton("addConst","Add new constraint")))

      lapply(1:numberOfVariables, function(i) {
        if(i != numberOfVariables) {
          output[[paste0("x", i)]] <- renderText({paste0(" * x", i, " + ")})
          output[[paste0("objFuncx", i)]] <- renderText({paste0(" * x", i, " + ")})
        } else {
          output[[paste0("x", i)]] <- renderText({paste0(" * x", i)})
          output[[paste0("objFuncx", i)]] <- renderText({paste0(" * x", i, " -> ")})
        }
      })
    }
  })

  #Új feltétel hozzáadását lehetővé tevő felületi elemek hozzáadása
  observeEvent(input$addConst, {
    numberOfConstraints <<- numberOfConstraints + 1;

    insertUI("#constraintsInputFields", ui = div())
    for(i in 1:numberOfVariables) {
      insertUI("#constraintsInputFields", ui =
        div(style="display: inline-block",textInput(paste0("x",i,"_value_",numberOfConstraints), label = "", width = "70px"))
      )
      insertUI("#constraintsInputFields", ui =
        div(style="display: inline-block; margin: 2px",textOutput(paste0("x", i, "_", numberOfConstraints), inline = TRUE))
      )
      if(i != numberOfVariables){
        output[[paste0("x", i, "_", numberOfConstraints)]] <- renderText({paste0(" * x", i, " + ")})
      } else {
        output[[paste0("x", i, "_", numberOfConstraints)]] <- renderText({paste0(" * x", i, " ")})
      }
    }
    insertUI("#constraintsInputFields", where = "beforeEnd", ui =
               div(style="display: inline-block; margin: 2px", selectInput(paste0("comparison_sign_", numberOfConstraints), label = "", choices = c("=","<=",">="), width ='65px'))
    )
    insertUI("#constraintsInputFields", where = "beforeEnd", ui =
               div(style="display: inline-block; margin: 2px", textInput(paste0("rs_value_",numberOfConstraints), label = "", width = '70px'))
    )

    lapply(1:numberOfVariables, function(i) {
      if(i != numberOfVariables) {
        output[[paste0("x", i, "_", numberOfConstraints)]] <- renderText({paste0(" * x", i, " + ")})
      } else {
        output[[paste0("x", i, "_", numberOfConstraints)]] <- renderText({paste0(" * x", i)})
      }
    })
  })


  #Megoldása a feladatnak
  observeEvent(input$solve, {
    valid <- inputsValid()
    if(is.null(valid)) {
      startUp(numberOfVariables)
      for (j in 1:numberOfVariables) {
        model.obj <<- c(model.obj, as.numeric(input[[paste0("objFuncCoeff",j)]]))
      }
      for (i in 1:numberOfConstraints) {
        rowToMatrix <- c()
        for(j in 1:numberOfVariables) {
          rowToMatrix <- c(rowToMatrix, as.numeric(input[[paste0("x",j,"_value_",i)]]))
        }
        model.con <<- rbind(model.con, rowToMatrix)
        model.dir <<- c(model.dir, input[[paste0("comparison_sign_", numberOfConstraints)]])
        model.rhs <<- c(model.rhs, as.numeric(input[[paste0("rs_value_",i)]]))
      }
      lpSolution <<- solveLP(model.obj, model.rhs, model.con, TRUE, model.dir, lpSolve = TRUE)
      if(lpSolution$status == 0) {
      calculateSolutionReport()

      removeUI("#ObjectiveFunctionInputFields")
      removeUI("#constraintsInputFields")
      removeUI("#addConst")
      removeUI("#solve")
      removeUI("#errorMessage")
      removeUI("#titleText")
      insertUI("#title", ui = div(id = "titleText", "Solution report"))
      insertUI("#reset", ui = actionButton("resetButton","Give a new problem"))

      output$solutionText <- renderText("Solution")
      output$solutionValueTable <- renderTable(solution.variables)
      output$solutionConstraintTable <- renderTable(solution.constraints)
      output$rangesVariablesText <- renderText("Objective coefficient ranges")
      output$rangesVariablesTable <- renderTable(range.variables)
      output$rangesConstraintText <-renderText("RHS ranges")
      output$rangesConstraintTable <- renderTable(range.constraints)
      } else {
        output$rangesVariablesText <- renderText("There is not solution!")
      }
    } else {
      output$errorMessage <- renderText({
        valid
      })
    }
  })

  observeEvent(input$resetButton, {
    output$solutionText <- renderText("")
    output$solutionValueTable <- renderTable(NULL)
    output$solutionConstraintTable <- renderTable(NULL)
    output$rangesVariablesText <- renderText("")
    output$rangesVariablesTable <- renderTable(NULL)
    output$rangesConstraintText <-renderText("")
    output$rangesConstraintTable <- renderTable(NULL)
    removeUI("#resetButton")
    removeUI("#titleText")
    insertUI("#title", ui = div(id = "titleText", "Linear problem"))

    numberOfConstraints <<- 1
    insertUI("#solution", where = "beforeBegin", ui =
         div(id = "numberOfVariables",
             textOutput("variablesText", inline = TRUE),
             textInput("variablesInput",label="", width = "70px"),
             actionButton("variables","Add")
         ))
  })

  output$objectfunctText <- renderText({"Objective function:"})
  output$constraintsText <- renderText({"Constraints:"})
  output$objFuncx1 <- renderText({" * x1 + "})
  output$objFuncx2 <- renderText({" * x2 -> "})
  output$x1 <- renderText({" * x1 + "})
  output$x2 <- renderText({" * x2"})
  output$variablesText <- renderText({"Number of variables in the problem:"})
  }
  shinyApp(ui=ui,server=server)
}
