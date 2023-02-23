server <- function(input, output) {

  output$is_local <- reactive(Sys.getenv('SHINY_PORT') == "")
  outputOptions(output, "is_local", suspendWhenHidden=FALSE)
  
  ##############################################################################
  # Main interactive table
  ##############################################################################

  output$tbl.summary <- DT::renderDT(tbl.summary,
    filter="top", escape=which(names(tbl.summary) != "DOI"), options=list(pageLength=6)
  )

  ##############################################################################
  # Graphics reporting numbers of records
  ##############################################################################

  output$tbl.numberOfRecords <- renderTable({
    crit <- input$numberOfRecordsTable
    if (crit == "Contaminant") {
      x <- tbl.recordsPerContaminant[,c("ContaminantGroup","Contaminant","Records")]
      x <- x[order(x[,"ContaminantGroup"],x[,"Records"]),]
    } else if (crit == "TechnologyGroup") {
      x <- tbl.recordsPerTechnologyGroup[,c("TechnologyGroup","Records")]
    } else {
      stop("undefined state") 
    }
    x
  }, striped=TRUE)
  
#  barPlt <- function(x, title) {
#    stopifnot(is.data.frame(x) && identical(names(x), c("count","label")))
#    x <- x[order(x[,"count"]),]
#    z <- barplot(x[,"count"], horiz=TRUE, col="#3973ac", border=NA, axes=FALSE)
#    axis(1, lwd=0)
#    #abline(v=axTicks(side=1), lty=3, col="lightgrey")
#    less <- x[,"count"] < mean(x[,"count"])
#    text(x=x[,"count"], y=z, pos=ifelse(less, 4, 2), x[,"label"], col=ifelse(less, "black", "white"))
#    mtext(side=3, title)
#  }

#  output$plt.recordsPerContaminant <- renderPlot({
#    barPlt(data.frame(
#      count= tbl.recordsPerContaminant[,"Records"],
#      label= tbl.recordsPerContaminant[,"Contaminant"]
#    ), "Number of Records per Contaminant")
#  })
  
#  output$plt.recordsPerTechnologyGroup <- renderPlot({
#    barPlt(data.frame(
#      count= tbl.recordsPerTechnologyGroup[,"Records"],
#      label= tbl.recordsPerTechnologyGroup[,"TechnologyGroup"]
#    ), "Number of Records per Technology Group")
#  })

  ##############################################################################
  # Graphics reporting ranges
  ##############################################################################
  rangePlt <- function(x, groupsField, title) {
    stopifnot(is.data.frame(x))
    stopifnot(all(c(groupsField,"Value","Unit") %in% names(x)))
    if (nrow(x) == 0) {
      plot(0:1, 0:1, type="n", bty="n", asp=1, axes=FALSE, ann=FALSE)
      text(x=0.5, y=0.5, "?", col="#E6E6C0", cex=5)
      mtext(side=1, "No matching records in database")
    } else {
      xlab <- unique(x[,"Unit"])
      mini <- aggregate(list(min=x[,"Value"]), by=x[,groupsField,drop=FALSE], aggr.fun, fun=min)
      maxi <- aggregate(list(max=x[,"Value"]), by=x[,groupsField,drop=FALSE], aggr.fun, fun=max)
      count <- aggregate(list(count=x[,"Value"]), by=x[,groupsField,drop=FALSE], aggr.fun, fun=length)
      x <- merge(mini, maxi, by=groupsField)
      x <- merge(x, count,  by=groupsField)
      rm(mini, maxi, count)
      x <- x[order(x[,"max"]),]
      xrng <- range(x[,c("min","max")])
      plot(xrng, c(0.5, nrow(x)+0.5), type="n", bty="n",
        axes=FALSE, xlab=xlab, ylab="")
      axis(1, col="darkgrey")
      dy <- 0.4
      for (i in 1:nrow(x)) {
        rect(xleft=x[i,"min"], xright=x[i,"max"], ybottom=i-dy, ytop=i+dy,
          border="#C5C59E", col="#E6E6C0")
        xpos <- mean(unlist(x[i, c("min", "max")]))
        lab <- paste0(x[i,groupsField]," (n=",x[i,"count"],")")
        text(x=xpos, y=i, lab, pos=ifelse(xpos < mean(xrng), 4, 2))
      }
      mtext(side=3, title)
    }
  }
  
  output$plt.rangePerContaminant <- renderPlot({
    sel <- (tbl.rangeGraphics[,"TechnologyGroup"] == input$rangeGraphics.technologyGroup) &
      (tbl.rangeGraphics[,"ContaminantGroup"] == input$rangeGraphics.contaminantGroup)
    rangePlt(tbl.rangeGraphics[sel,], "Contaminant",
      paste("Reported ranges for",input$rangeGraphics.technologyGroup))
  })

  output$plt.rangePerTechnologyGroup <- renderPlot({
    sel <- tbl.rangeGraphics[,"Contaminant"] == input$rangeGraphics.contaminant
    rangePlt(tbl.rangeGraphics[sel,], "TechnologyGroup",
      paste("Reported ranges for",input$rangeGraphics.contaminant))
  })
    
  
#  ##############################################################################
#  # Expert mode
#  ##############################################################################

  # Values returned from query
  expertModeOutputs <- reactiveValues(
      message=NULL,
      table=NULL
  )

  observe({
    expertModeOutputs$table <- NULL
    expertModeOutputs$message <- NULL
    if (!is.null(input$expertMode.submit) & (input$expertMode.submit > 0)) {
      tryCatch({
        isolate( # do not react instantly on query edits
          if (nchar(input$expertMode.query) > 0) {
            # check 'q' for possible bad inputs
            q <- as.character(input$expertMode.query)[1]
            blank <- " "
            special <- c("*",".",",","(",")","=","_")
            allowed <- c(letters, LETTERS, special, blank)
            qq <- unlist(strsplit(q, split=""))
            if (!all(qq %in% allowed)) {
              stop(paste0("Illegal characters in input. Must be characters,",
                " blanks, or one of ",paste(special, collapse="")))
            }
            if (!grepl(q, pattern="^select ", ignore.case=TRUE)) {
              stop("Legal queries start with 'select'.")
            }
            expertModeOutputs$table <- with(db, sqldf(x=q, dbname=":memory:"))
          }
        )
      }, warning = function(x) {
          expertModeOutputs$table <- NULL
          expertModeOutputs$message <- x
      }, error = function(x) {
          expertModeOutputs$table <- NULL
          expertModeOutputs$message <- x
      })
    }
  })

  output$expertMode.out.message <- renderUI({
    if (!is.null(expertModeOutputs$message))
      HTML(paste0("<br><p style='background-color:#EED4BE;border:2px;
        border-style:solid; border-color:#7F7F7F; padding:1em;'>",
        expertModeOutputs$message,"</p>"))
    else
      HTML("")
  })

  output$expertMode.out.table <- renderTable({
    expertModeOutputs$table
  }, striped=TRUE)

}
