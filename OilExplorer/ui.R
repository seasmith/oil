
# HEADER ------------------------------------------------------------------

# Header
header <- dashboardHeader(
 title = "Oil Explorer"
)


# SIDEBAR -----------------------------------------------------------------

# Define unit inputs
defaultUnit <- "Rig Count"
unitTypes <- c(
 "Rig Count" = "Rig Count",
 "Change From Start Period" = "Change From Start Period",
 "Percent Change From Start Period" = "Percent Change From Start Period"
)

# Define boundary inputs
defaultBoundary <- "County"
boundaryTypes <- c(
 "County"   = "County",
 "Basin"    = "Basin"
)

# Define week inputs
allWeekValues <- rc_master %>%
 distinct(PublishDate) %>%
 arrange(desc(PublishDate)) %>%
 # pull(PublishDate)
 mutate(display = paste0(PublishDate, " ", "(", (row_number() - 1), ")"))

allWeekValues <- allWeekValues %>%
 {
  x <- .
  y <- x$PublishDate
  names(y) <- x$display
  # y <- x$display
  # names(y) <- x$PublishDate
  y
 } %>%
 as.character()

defaultEndWeek   <- allWeekValues[[1L]]
defaultStartWeek <- allWeekValues[[4L]]

# Set variables for use through out the ui

mainItem <- "Rig Count Map"
mainItemTabName <- "rigCountMap"

# Sidebar
sidebar <- dashboardSidebar(
 sidebarMenu(
  menuItem(mainItem, tabName = mainItemTabName),
  # selectInput("unit", "Unit", unitTypes, defaultUnit),
  selectInput("boundary", "Boundary Type", boundaryTypes, defaultBoundary),
  selectInput("endWeek", "End Week", allWeekValues, defaultEndWeek),
  selectInput("startWeek", "Start Week", allWeekValues, defaultStartWeek),
  actionButton("resetSelections", "Reset All Selections")
 )
)

# BODY --------------------------------------------------------------------

# Body
body <- dashboardBody(
 tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}"),
 tabItems(
  tabItem(
   tabName = mainItemTabName,
   title = "Map",
   fluidRow(
    height = "400px",
    box(
     width = 6,
     plotOutput("mapLevel", height = "400px")
    ),
    box(
     width = 6,
     plotOutput("mapChange", height = "400px")
    )
   ),
   fluidRow(
    height  = "400px",
    box(
     width = 12,
     plotOutput("chartLevel")
    )
   )
  )
 )
)


# UI ----------------------------------------------------------------------

# ui
ui <- dashboardPage(header, sidebar, body, skin = "purple")
