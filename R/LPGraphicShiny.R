LPGraphicShiny <- function () {

  library(shiny)
  library(lpSolve)

  startUp <- function(){
    model.obj <<- c()
    model.type <<- c()
    model.con <<- matrix(nrow = 0, ncol = 2)
    model.dir <<- c()
    model.rhs <<- c()

    options(stringsAsFactors = FALSE)
    constrainList <<- data.frame()
    length_X_axes <<- 0
    length_Y_axes <<- 0
  }

  fv <- function(a,x,b){
    return (a*x+b)
  }

  startUp()

  ui <- fluidPage(
    pageWithSidebar(

      headerPanel("Graphic solution of a 2D linear problem."),

      sidebarPanel(

        #Célfüggvény hozzáadását szolgáló felületi elemek
        div( id="objectiveFunction",
             div( id="objfuncdiv",
        textOutput("objectfunctText"),
        div(style="display: inline-block",textInput("coeff1",label="", width = "70px")),
        div(style="display: inline-block; margin: 2px",textOutput("x1_OF", inline = TRUE)),
        div(style="display: inline-block",textInput("coeff2",label="", width = "70px")),
        div(style="display: inline-block; margin: 2px",textOutput("x2_OF", inline = TRUE)),
        div(style="display: inline-block",selectInput("objfunctype", "", choices = c("max","min"), selected = "max", width = "70px")),
        div(style="display: block; margin-botton: 5px", actionButton("addObjFuncCoeff","Add")))),

        #Új feltételek megadásához szükségek felületi elemek
        div( id= "constraints",
        div(style="margin-top: 5px", textOutput("constraintsText")),
        div(style="display: inline-block",textInput("x1_value", label = "", width = "50px")),
        div(style="display: inline-block; margin: 2px",textOutput("x1", inline = TRUE)),
        div(style="display: inline-block", textInput("x2_value", label = "", width = '50px')),
        div(style="display: inline-block; margin: 2px",textOutput("x2", inline = TRUE)),
        div(style="display: inline-block", selectInput("comparison_sign", label = "", choices = c("=","<=",">="), width ='65px')),
        div(style="display: inline-block", textInput("rs_value", label = "", width = '50px')),
        div(style="display: block"),
        div(style="display: inline-block", actionButton("addConst","Add")),
        div(style="display: inline-block", actionButton("reset", "Reset"))),

        div(style="margin-top: 5px; margin-bottom: 5px", textOutput(outputId = "objectiveFunction")),

        div(id="checkbox",
        checkboxGroupInput(inputId = "constrains", choices = constrainList$constrain, label="")),
        div(id = "errorMessage", style="margin-top: 5px; color: red", textOutput("errorMessage")),

        width = 5
      ),

      #Fő rész, ahol a 2d-os koordináta-rendszert és a feltétel módosítás megjelenik
      mainPanel(
        plotOutput("plot"),
        textOutput("solution"),
        div(id = "divToModifyRightSide", div(id = "rightSides")),
        width = 7
      )
    )
  )

  server <- function(input, output, session) {

    #Célfüggvény hozzáadása
    observeEvent(input$addObjFuncCoeff,{
      #Megfelelő-e a célfüggvény (nem megfelelő, ha bármelyik cfv együttható nem szám vagy 0)
      if(!is.na(as.numeric(input$coeff1)) & !is.na(as.numeric(input$coeff2))
                       & (as.numeric(input$coeff1) != 0 || as.numeric(input$coeff2) != 0)){
        model.obj <<- c(as.numeric(input$coeff1),as.numeric(input$coeff2))
        model.type <<- input$objfunctype
        #Törlöm a felületről a célfüggvény megadásához tartozó részt, és kiírom a célfüggvényt
        removeUI("#objfuncdiv")
        output$objectiveFunction <- renderText({
          paste("Objective Function: ", model.obj[1],"* x1 + ",
                model.obj[2], " * x2 -> ", model.type)})
        output$errorMessage <- renderText({""})
      } else {
        #Ha nem megfelelő valamelyik input, akkor itt íratjuk ki a hibaszöveget a felületre
        output$errorMessage <- renderText({
          if(is.na(as.numeric(input$coeff1))){
            "Please give a numeric value to the first coefficient!"
          } else if(is.na(as.numeric(input$coeff2))){
            "Please give a numeric value to the second coefficient!"
          } else if(as.numeric(input$coeff1) == 0 & as.numeric(input$coeff2) == 0){
            "Only one coefficients can't be zero!"
          } else {
            ""
          }
        })
      }
    })

    #Új feltétel hozzáadása
    observeEvent(input$addConst,{
      #Megfelelő-e a feltétel
      if(!is.na(as.numeric(input$x1_value)) & !is.na(as.numeric(input$x2_value)) &
         !is.na(as.numeric(input$rs_value)) & !is.null(model.obj)){
        #Hozzáadom a beolvasott adatokat a mártixhoz és vektorokhoz, amik a lpSolve bemenetéül szolgálnak
        model.con <<- rbind(model.con, c(as.numeric(input$x1_value), as.numeric(input$x2_value)))
        model.dir <<- c(model.dir, input$comparison_sign)
        model.rhs <<- c(model.rhs,as.numeric(input$rs_value))
        #A feltétel felületre való kiírásához összeállítom a feltétel karakteres változatát
        constrain <- c( paste(ifelse(as.numeric(input$x1_value != 0), paste(input$x1_value, "* x1"), ""),
                              ifelse(as.numeric(input$x1_value) & as.numeric(input$x2_value), " + ", ""),
                              ifelse(as.numeric(input$x2_value != 0), paste(input$x2_value, "* x2 "), ""),
                              input$comparison_sign, input$rs_value))
        #b az x-tengely, a az y-tengely metszéspontja az új feltétel esetén
        b <- as.numeric(input$rs_value)/as.numeric(input$x1_value)
        a <- as.numeric(input$rs_value)/as.numeric(input$x2_value)
        meredekseg <- -1*as.numeric(input$x1_value)/as.numeric(input$x2_value)
        sign <- input$comparison_sign
        rs <- input$rs_value
        constrainList <<- rbind(constrainList, c(constrain,a,b,meredekseg,sign,rs))
        colnames(constrainList) <<- c("constrain","a","b","meredekseg","sign","rs")
        #Megoldjuk a optimalizálást
        lp <- lp(model.type, model.obj, model.con, model.dir, model.rhs)
        x_axes_to_solution <<- (lp$objval)/model.obj[2]
        solutions.coordinate <<- lp$solution
        solution <<- c(lp$objval, lp$status)

        #Tengelyek hosszainak kiszámítása
        x<-0
        y<-0
        for (i in 1:nrow(constrainList)){
          if (model.con[i,2]==0) y[i]<-0 else y[i]<-model.rhs[i]/model.con[i,2]
          if (model.con[i,1]==0) x[i]<-0 else x[i]<-model.rhs[i]/model.con[i,1]
        }
        length_X_axes<<-abs(max(x))
        length_Y_axes<<-abs(max(y))
        if (length_Y_axes==0) {length_Y_axes <<- length_X_axes}
        if (length_X_axes==0) {length_X_axes <<- length_Y_axes}

        #Inputmezők frissítése
        updateCheckboxGroupInput(session, "constrains", label = "Constraints:",
                          choices = constrainList$constrain)
        updateTextInput(session, "x1_value",value = NA)
        updateTextInput(session, "x2_value",value = NA)
        updateTextInput(session, "rs_value",value = NA)

        #A hozzáadott feltétel módosításához szükséges felületi elemek hozzáadása
        insertUI("#rightSides", ui = div(style="display: inline-block; margin-right: 5px",
                       textOutput(paste0("rsText",nrow(constrainList)))))
        output[[paste0("rsText",i)]] <- renderText({
          paste0("Right side value of the ", i,". constraint:")
        })
        insertUI("#rightSides", ui = div(style="display: inline-block",
              numericInput(paste0("rs",nrow(constrainList)), label = "",
                           value = constrainList$rs[nrow(constrainList)],
                           min = -Inf, width = "100px")))
        insertUI("#rightSides", ui = div(style="display: block"))
        output$errorMessage <- renderText({})
      } else {
        #Hibaüzenet kiírása
        output$errorMessage <- renderText({
          if(is.na(as.numeric(input$x1_value))){
            "Please give a numeric value to x1"
          } else if(is.na(as.numeric(input$x2_value))){
            "Please give a numeric value to x2"
          } else if(is.na(as.numeric(input$rs_value))){
            "Please give a numeric value to right side value"
          } else if(is.null(model.obj)){
            "Please first give the objective function!"
          } else if(is.null(as.numeric(input$x1_value)) && is.null(as.numeric(input$x2_value))){
            "At least one coefficient must be none zero!"
          } else {
            ""
          }
        })
      }
    })

    #Visszaállítás, hogy egy új problémát tudjunk megadni
    observeEvent(input$reset,{
      #Felület manipulálása(Jobboldali értékek módosításáhz szükséges elemek levétele + kiírt cfv)
      removeUI("#objfuncdiv")
      removeUI("#rightSides")
      removeUI("#constrains")
      #Visszarakjuk a cfv megadását segítő mezőket, valamint a checkbox-ot is visszaállítjuk kiinduló állapotára
      insertUI("#divToModifyRightSide", ui = (div(id = "rightSides")))
      insertUI("#checkbox", ui=checkboxGroupInput(inputId = "constrains", choices = c(), label=""))
      insertUI("#objectiveFunction", ui = div(id="objfuncdiv",
                textOutput("objectfunctText"),
                div(style="display: inline-block",textInput("coeff1",label="", width = "50px")),
                div(style="display: inline-block; margin: 2px",textOutput("x1_OF", inline = TRUE)),
                div(style="display: inline-block",textInput("coeff2",label="", width = "50px")),
                div(style="display: inline-block; margin: 2px",textOutput("x2_OF", inline = TRUE)),
                div(style="display: inline-block",selectInput("objfunctype", "", choices = c("max","min"), selected = "max", width = "70px")),
                div(style="display: block; margin-botton: 5px", actionButton("addObjFuncCoeff","Add"))))
      #Tárolt értékek kinullázása
      startUp()
      output$objectiveFunction <- renderText({""})
      output$errorMessage <- renderText({})
    })

    #A checkbox és a jobboldali értékek inputmezőinek változását figyeli
    observe({

      output$plot <- renderPlot({

        #Jobb oldali értékek módosulása esetén fut. Azt vizsgáljuk, hogy a felületi RS mezők
        #közül módosult-e bármelyik is. Ha igen, akor a tárolt értéket felül kell írni és újra
        # meg kell oldani a feladatot.
        if(nrow(constrainList)>=1){
          for(i in 1:nrow(constrainList)){
            #Jobb oldali értékeket módosító inputokon végigmegyek és ha más az értéke ahhoz képest,
            #ami le van tárolva, akkor itt is módosítom.
            newRS <- as.numeric(input[[paste0("rs",i)]])
            if(!identical(newRS, numeric(0))){
              if(newRS != constrainList[i,]$rs){
                constrain <- c( paste(ifelse(model.con[i,1] != 0, paste(model.con[i,1], "* x1"), ""),
                                      ifelse(model.con[i,1] & model.con[i,2], " + ", ""),
                                      ifelse(model.con[i,2] != 0, paste(model.con[i,2], "* x2 "), ""),
                                      model.dir[i], newRS))
                b <- newRS/model.con[i,1]
                a <- newRS/model.con[i,2]
                earlyConstraint <- constrainList[i,]$constrain
                newConstraint <- constrain
                constrainList[i,]$constrain <<- constrain
                constrainList[i,]$a <<- a
                constrainList[i,]$b <<- b
                constrainList[i,]$rs <<- newRS
                model.rhs[i] <<- newRS

                #Újra megoldom az optimalizálási feladatot, viszont a módosított értékekkel
                lp <- lp(model.type, model.obj, model.con, model.dir, model.rhs)
                x_axes_to_solution <<- (lp$objval)/model.obj[2]
                solutions.coordinate <<- lp$solution
                solution <<- c(lp$objval, lp$status)

                #Újraszámolom az x-y tengelyhosszt
                x<-0
                y<-0
                for (i in 1:nrow(constrainList)){
                  if (model.con[i,2]==0) y[i]<-0 else y[i]<-model.rhs[i]/model.con[i,2]
                  if (model.con[i,1]==0) x[i]<-0 else x[i]<-model.rhs[i]/model.con[i,1]
                }
                length_X_axes<<-abs(max(x))
                length_Y_axes<<-abs(max(y))
                if (length_Y_axes==0) {length_Y_axes <<- length_X_axes}
                if (length_X_axes==0) {length_X_axes <<- length_Y_axes}

                #A checkbox állapotát meg akarom tartani, így megvizsgálom, mik vannak bepipálva. Ha a
                #módosított feltétel is bevolt pipálva, akkor az új is be lesz. Majd checkbox frissítés.
                selected <- c()
                for(i in input$constrains){
                  modified <- F
                  if(i == earlyConstraint){
                    modified <- T
                    selected <- c(selected, newConstraint)
                  }
                  if(!modified){
                    selected <- c(selected, i)
                  }
                }
                updateCheckboxGroupInput(session, "constrains", label = "Constraints:",
                                         choices = constrainList$constrain, selected = selected)
              }
            }
          }
        }

        #Az ábra kirajzolása a tengelyekkel
        plot(c(-2,length_X_axes+5), c(-2,length_Y_axes+5), type = "n", xlab="x", ylab="y")
        abline(h=0, v=0, col = "gray60")

        #Bepipált feltételekhez az egyenesek kirajzolása az ábrára
        for(i in input$constrains){
          row <- constrainList[which(constrainList$constrain == i),]
          if(nrow(row) != 0){
            if(as.numeric(row$meredekseg) == 0){
              abline(h = row$a, col = "green")
            } else if(is.infinite(abs(as.numeric(row$meredekseg)))){
              abline(v = row$b, col = "green")
            } else {
              abline(a = row$a, b = row$meredekseg, col = "green")
            }
          }
        }

        #Megoldások halmazának beszínezése.
        if (length(input$constrains) == nrow(constrainList) & nrow(constrainList)>=1){
          felsohatar <- alsohatar <- c()
          xalsohatar <- xfelsohatar <- c()
          #Azok a sorok, amelyeknek kirajzolt egyenese merőleges az x tengelyre
          if(nrow(constrainList[which(is.infinite(abs(as.numeric(constrainList$meredekseg)))),]) == 0){
            x <- seq(0, 1.1*(length_X_axes+5), length.out = 100)
            for (i in 1:nrow(constrainList)){
              if (!is.infinite(abs(as.numeric(constrainList[i,]$b))))
                x <- c(x,as.numeric(constrainList[i,]$b))
            }
            x <- sort(x)
          } else {
            rows <- as.data.frame(constrainList[which(is.infinite(as.numeric(constrainList$a))),])
            rowskisebb <- as.data.frame(rows[which(rows$sign == "<="),])
            rowskisebb <- rowskisebb$b
            rowsnagyobb <- as.data.frame(rows[which(rows$sign == ">="),])
            rowsnagyobb <- rowsnagyobb$b
            if(length(rowskisebb) > 0){
              min <- min(as.numeric(rowskisebb))
              xfelsohatar <- min
            } else {
              xfelsohatar <- 1.1*(length_X_axes+5)
            }
            if(length(rowsnagyobb) > 0){
              max <- max(as.numeric(rowsnagyobb))
              xalsohatar <- max
            } else {
              xalsohatar <- 0
            }
            x <- seq(xalsohatar, xfelsohatar, length.out = 100)
          }
          vonalelsokoord <- vonalutolsokoord <- c()
          if(nrow(constrainList[which(constrainList$sign == "="),]) < 2){
            for(j in x){
              a <- b <- c()

              #A <= feltételek esetén mindegyik ilyen feltételhez megnézzük az x aktuális értékénél
              #felvett függvényétéket és az összes közül kiválasztjuk a legkisebbet.
              rows <- as.data.frame(constrainList[which(constrainList$sign == "<="),])
              rows <- as.data.frame(rows[which(!is.infinite(as.numeric(rows$a))),])
              if(nrow(rows) > 0){
                for(i in 1:nrow(rows)){
                  a <- c(a, fv(as.numeric(rows$meredekseg[i]), j, as.numeric(rows$a[i])))
                }
                felsohatar <- ifelse(min(a) < 0, 0, min(a))
              } else {
                felsohatar <- 1.1*(length_Y_axes+5)
              }

              #A >= feltételeket vesszük és szintén minden x-hez megkeressük a fv-ek aktuális értékét
              #x-hez és itt a legnagyobb értéketválasztjuk.
              rows <- constrainList[which(constrainList$sign == ">="),]
              rows <- as.data.frame(rows[which(!is.infinite(as.numeric(rows$a))),])
              if(nrow(rows) > 0){
                for(i in 1:nrow(rows)){
                  b <- c(b, fv(as.numeric(rows$meredekseg[i]),j, as.numeric(rows$a[i])))
                }
                alsohatar <- ifelse(max(b) < 0, 0, max(b))
              } else {
                alsohatar <- 0
              }

              #Ha van egy egyenlőség típusú feltételem, akkor megnézem, hogy az egyenes az x aktuális
              #értékénél az alsó és a felső határ közé esik. Ha igen, akkor optimális megoldás lesz.
              #Itt csak az első és utolsó kordinátát tároljuk el!
              #Ha nincs =, akkor pedig a also és felső határ közötti terület között vonalat húzunk.
              if(nrow(constrainList[which(constrainList$sign == "="),]) == 1){
                row <- as.data.frame(constrainList[which(constrainList$sign == "="),])
                if (!is.infinite(abs(as.numeric(row$meredekseg)))){
                  fvertek <- fv(as.numeric(row$meredekseg),j, as.numeric(row$a))
                  if(fvertek > alsohatar && fvertek < felsohatar){
                    if(is.null(vonalelsokoord)){
                      vonalelsokoord <- c(j,fvertek)
                    } else {
                      vonalutolsokoord <- c(j,fvertek)
                    }
                  }
                } else {
                  if (j == row$b){
                    vonalelsokoord <- c(j,alsohatar)
                    vonalutolsokoord <- c(j,felsohatar)
                  } else {
                    alsohatar <- felsohatar - 1
                  }
                }
              } else {
              if(alsohatar < felsohatar){
                segments(x0=j, y0=alsohatar, x1=j, y1=felsohatar, col="cornflowerblue")
                }
              }
            }
            if(nrow(constrainList[which(constrainList$sign == "="),]) == 1){
              if(!is.null(vonalelsokoord)){
              segments(x0=vonalelsokoord[1], y0=vonalelsokoord[2], x1=vonalutolsokoord[1],
                       y1=vonalutolsokoord[2], col="cornflowerblue")
              }
            }
          }
        }

        #A célfüggvény kirajzolása, hogy az optimális megoldást érinti.
        if (length(input$constrains) == nrow(constrainList) && nrow(constrainList)>0) {
          if(solution[2] == 0){
            if (model.obj[2] != 0) {
              abline(a = x_axes_to_solution,
                   b = round(-1*model.obj[1]/model.obj[2],4), col = "red")
            } else {
              abline(v = solutions.coordinate[1], col = "red")
            }
            points(solutions.coordinate[1], solutions.coordinate[2], col = "blue", pch = 19)
          }
        }

      })

      #A optimális megoldás szöveges kiírásához szükséges megjelenítési fv. Akkor jelenik
      #meg, ha mindegyik feltétel be van pipálva.
      output$solution <- renderText({
        if (length(input$constrains) == nrow(constrainList) & nrow(constrainList)>0){
          if(solution[2] == 0){
          paste0("The Optimal Point is (", round(solutions.coordinate[1], 2),",",
                round(solutions.coordinate[2], 2),") and the Objective function's value is ",
                round(solution[1], 2),".")
          } else if(solution[2] == 2){
            "There is no solution!"
          } else {
            "Objective function has a lot of solutions."
          }
        }
      })

      #Egyéb felületi szövegelemeknek az értékadása.
      output$x1_OF <- renderText({" * x1 + "})
      output$x2_OF <- renderText({" * x2 -> "})
      output$x1 <- renderText({" * x1 + "})
      output$x2 <- renderText({" * x2"})
      output$objectfunctText <- renderText({"Give the coefficients of objective function:"})
      output$constraintsText <- renderText({"Give the constraints:"})
    })
  }

  shinyApp(ui=ui,server=server)
}
