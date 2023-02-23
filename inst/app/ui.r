
prepChoices <- function(x, major="ContaminantGroup", minor="Contaminant") {
  x <- tapply(x[,minor], x[,major], list)
  lapply(x, sort)
}

ui <- fluidPage(
  tags$head(
    tags$style(paste(readLines("www/html/styles.css"), collapse="\n"))
  ),
  titlePanel(appname),
  tabsetPanel(
    id="mainMenue",
    ############################################################################
    tabPanel("Start page",
      br(),
      HTML(start)
    ),
    ############################################################################
    tabPanel("High-level summaries",
      br(),
      tabsetPanel(
        id = "summariesTabsetPanel",
        tabPanel("Numbers of records",
          br(),
          selectInput("numberOfRecordsTable",
            label="Select criterion for building the summary",
            choices=c("Contaminant", "TechnologyGroup"),
            selected=c("TechnologyGroup")
          ),
          br(),
          htmlOutput("tbl.numberOfRecords")
        ),
        tabPanel("Ranges per contaminant",
          br(),
          selectInput("rangeGraphics.contaminantGroup",
            label="Select contaminant group",
            choices=unique(tbl.rangeGraphics[,"ContaminantGroup"]),
            selected=tbl.rangeGraphics[1,"ContaminantGroup"]
          ),
          selectInput("rangeGraphics.technologyGroup",
            label="Select technology group",
            choices=unique(tbl.rangeGraphics[,"TechnologyGroup"]),
            selected=tbl.rangeGraphics[1,"TechnologyGroup"]
          ),
          br(),
          plotOutput("plt.rangePerContaminant")
        ),
        tabPanel("Ranges per technology group",
          br(),
          selectInput("rangeGraphics.contaminant",
            label="Select contaminant",
            choices=prepChoices(unique(tbl.rangeGraphics[,c("ContaminantGroup","Contaminant")])),
            selected=tbl.rangeGraphics[1,"Contaminant"]
          ),
          br(),
          plotOutput("plt.rangePerTechnologyGroup")
        )
      )
    ),
    

    ############################################################################
    tabPanel("Tabular views",
      br(),
      tabsetPanel(
        id = "tabularViewsTabsetPanel",
        tabPanel("Results",
          br(),
          DTOutput("tbl.summary")
        ),
        tabPanel("Help",
          br(),
          HTML(help[["basicSummary"]])
        )
      )
    ),
    ############################################################################
    tabPanel("SQL mode",
      br(),
      conditionalPanel(
        condition = "output.is_local == true",
        tabsetPanel(
          id = "expertModeTabsetPanel",
          tabPanel("SQL query",
            br(),
            textAreaInput("expertMode.query", "Type or paste your SQL query here",
              cols=50, rows=12),
            actionButton("expertMode.submit", "Submit"),
            br(),
            uiOutput("expertMode.out.message"),
            br(),
            htmlOutput("expertMode.out.table")
          ),
          tabPanel("Help",
            br(),
            HTML(help[["expertMode"]])
          )
        )
      ),
      conditionalPanel(
        condition = "output.is_local == false",
          HTML(paste0("For security reasons, this feature is currently not",
            " supported when the app is run over the internet. A distribution",
            " package for offline use is currently under construction."))
      )
    ),
    ############################################################################
    tabPanel("Impressum",
      br(),
      HTML(impressum)
    ),
    tabPanel("Legal aspects",
      br(),
      HTML(legal)
    )
  )
)
