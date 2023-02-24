library("shiny")
library("sqldf")
library("DT")

if(!require("tabular")){
  library("devtools")
  devtools::install_github("dkneis/tabular")
}

appname <- "DSWAP Removal Efficiency Database"

# external HTML documents
start <- paste(readLines("www/html/start.html"), collapse="\n")
impressum <- paste(readLines("www/html/impressum.html"), collapse="\n")
legal <- paste(readLines("www/html/legal.html"), collapse="\n")
help <- list(
  basicSummary= paste(readLines("www/html/help_basicSummary.html"), collapse="\n"),
  expertMode= paste(readLines("www/html/help_expertMode.html"), collapse="\n")
)

# import DB spread over multiple tables
db <- tabular::db.read(dir="www/db/", ext="csv", sep=",", quote="\"")

# check DB integrity
validate <- function(db) {
  with(db, {

    # cases
    stopifnot(tabular::check.key(cases, c("id_case")))
    stopifnot(tabular::check.link(cases, "id_technology", technologies, "id_technology"))
    stopifnot(tabular::check.link(cases, "id_watersource", watersources, "id_watersource"))
    stopifnot(tabular::check.notnull(cases, names(cases)))

    # conditions
    stopifnot(tabular::check.key(conditions, c("id_case","id_parameter")))
    stopifnot(tabular::check.link(conditions, "id_case", cases, "id_case"))
    stopifnot(tabular::check.link(conditions, "id_parameter", parameters, "id_parameter"))
    stopifnot(tabular::check.notnull(conditions, names(conditions)))

    # parameters
    stopifnot(tabular::check.key(parameters, "id_parameter"))
    stopifnot(tabular::check.notnull(parameters, names(parameters)))

    # water sources
    stopifnot(tabular::check.key(watersources, "id_watersource"))
    stopifnot(tabular::check.notnull(watersources, names(watersources)))

    # technologies
    stopifnot(tabular::check.key(technologies, "id_technology"))
    stopifnot(tabular::check.link(technologies, "id_technologyGroup", technologyGroups, "id_technologyGroup"))
    stopifnot(tabular::check.notnull(technologies, names(technologies)))

    # technologyGroups
    stopifnot(tabular::check.key(technologyGroups, "id_technologyGroup"))
    stopifnot(tabular::check.notnull(technologyGroups, names(technologyGroups)))

    # efficiencies
    stopifnot(tabular::check.key(efficiencies, c("id_contaminant", "id_case", "id_statistics")))
    stopifnot(tabular::check.link(efficiencies, "id_contaminant", contaminants, "id_contaminant"))
    stopifnot(tabular::check.link(efficiencies, "id_statistics", statistics, "id_statistics"))
    stopifnot(tabular::check.notnull(efficiencies, names(efficiencies)))

    # contaminants
    stopifnot(tabular::check.key(contaminants, "id_contaminant"))
    stopifnot(tabular::check.link(contaminants, "id_contaminantGroup", contaminantGroups, "id_contaminantGroup"))
    stopifnot(tabular::check.notnull(contaminants, names(contaminants)))

    # contaminantGroups
    stopifnot(tabular::check.key(contaminantGroups, "id_contaminantGroup"))
    stopifnot(tabular::check.notnull(contaminantGroups, names(contaminantGroups)))

    # statistics
    stopifnot(tabular::check.key(statistics, "id_statistics"))
    stopifnot(tabular::check.notnull(statistics, names(statistics)))

  })
}

validate(db)

################################################################################
# Tables used in specific views
################################################################################

# Query returning data for the main interactive table view

tbl.summary <- with(db, {
  sqldf("
  SELECT
    cg.description AS ContaminantGroup,
    ct.description AS Contaminant,
    tg.description AS TechnologyGroup,
    ef.value AS Value,
    ca.DOI AS DOI
  FROM
    efficiencies AS ef,
    contaminantGroups AS cg,
    contaminants AS ct,
    cases AS ca,
    technologies AS te,
    technologyGroups AS tg
  WHERE
    ef.id_contaminant == ct.id_contaminant AND
    ef.id_case == ca.id_case AND
    ca.id_technology == te.id_technology AND
    te.id_technologyGroup == tg.id_technologyGroup AND
    ct.id_contaminantGroup == cg.id_contaminantGroup
  ORDER BY
    cg.description,
    ct.description,
    tg.description
 ")
})
tbl.summary[,"DOI"] <- paste("<a href='",tbl.summary[,"DOI"],"'>",gsub(tbl.summary[,"DOI"],
        pattern="https://doi.org/", fixed=TRUE, replacement=""),"</a>")
tbl.summary[,"Value"] <- sapply(tbl.summary[,"Value"], function(x) {
  z <- suppressWarnings(as.numeric(x))
  if (!is.finite(z)) x else signif(z, 2)
})
tbl.summary[,"Value"] <- sapply(tbl.summary[,"Value"], function(x) {gsub(x, pattern="_", replacement="...", fixed=TRUE)})
# string to factor conversion enables selectize in table filtering (text search otherwise)
tbl.summary[,"Contaminant"] <- as.factor(tbl.summary[,"Contaminant"])
tbl.summary[,"ContaminantGroup"] <- as.factor(tbl.summary[,"ContaminantGroup"])
tbl.summary[,"TechnologyGroup"] <- as.factor(tbl.summary[,"TechnologyGroup"])

# Queries for graphics of observed ranges
tbl.rangeGraphics <- with(db, {
  sqldf("
  SELECT
    cg.description AS ContaminantGroup,
    cg.unit AS Unit,
    ct.description AS Contaminant,
    tg.description AS TechnologyGroup,
    te.description AS Technology,
    ef.value AS Value
  FROM
    efficiencies AS ef
      LEFT JOIN cases AS ca
        ON ef.id_case = ca.id_case
      LEFT JOIN technologies AS te
        ON te.id_technology = ca.id_technology
      LEFT JOIN technologyGroups AS tg
        ON te.id_technologyGroup = tg.id_technologyGroup
      LEFT JOIN contaminants AS ct
        ON ct.id_contaminant = ef.id_contaminant
      LEFT JOIN contaminantGroups AS cg
        ON ct.id_contaminantGroup = cg.id_contaminantGroup
 ")
})


# Queries returning the number of records

tbl.recordsPerContaminant <- with(db, {
  sqldf("
  SELECT
    cg.description AS ContaminantGroup,
    co.description AS Contaminant,
    COUNT (*) AS Records
  FROM
    efficiencies AS ef
      LEFT JOIN contaminants AS co
        ON ef.id_contaminant = co.id_contaminant
      LEFT JOIN contaminantGroups AS cg
        ON cg.id_contaminantGroup = co.id_contaminantGroup
  GROUP BY co.description
  ")
})

tbl.recordsPerTechnologyGroup <- with(db, {
  sqldf("
  SELECT
    tg.description AS TechnologyGroup,
    COUNT (*) AS Records
  FROM
    efficiencies AS e
      LEFT JOIN cases AS c
        ON e.id_case = c.id_case
      LEFT JOIN technologies AS t
        ON t.id_technology = c.id_technology
      LEFT JOIN technologyGroups AS tg
        ON t.id_technologyGroup = tg.id_technologyGroup
  GROUP BY tg.description
  ")
})



################################################################################
# Functions
################################################################################

non_numeric <- function(x) {
  any(grepl(x, pattern="[a-zA-Z]+")) 
}

# functions for aggregation; deals with single numbers and range info;
# range limits are converted to two individual numbers before calling "fun"
# compute ranges
aggr.fun <- function(x, fun) {
  if (length(x) == 0) stop("empty input")
  # get rid of spaces
  x <- gsub(x, pattern="[ ]+", replacement="")
  # detect invalid items
  i <- which(grepl(x, pattern="[^-1234567890._]"))
  if (length(i) > 0) {
    # print(x[i[1]])
    stop("format of value(s) not supported")
  }
  # replace underscore in range info by comma (-> separate values)
  x <- gsub(x, pattern="_", replacement=",", fixed=TRUE)
  # merge all by comma
  x <- paste(x, collapse=",")
  # split
  x <- unlist(strsplit(x, split=",", fixed=TRUE))
  x <- as.numeric(x)
  if (any(!is.finite(x))) stop("conversion of value(s) failed")
  fun(x)
}

