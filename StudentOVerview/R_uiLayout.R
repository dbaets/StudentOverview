#ui layout
page_selection <- dashboardPage(
  title = "Leerling selectie",
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    fluidRow(
      column(6, selectInput("vak", label = h3("Selecteer vak"), 
                            choices = list("Fysica", 
                                           "Chemie", 
                                           "Biologie",
                                           "Natuurwetenschappen"), 
                            selected = "Biologie")),
      column(6,uiOutput("klas"))
    ),
    
    hr(),
    
    DT::dataTableOutput("loadedTable")
    
  )
)

page_summary <- dashboardPage(
  title = "Samenvatting",
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    # fluidRow(
    #   column(6,uiOutput("yaxis")),
    #   column(6,uiOutput("xaxis"))
    # ),
    fluidRow(
      column(6,plotOutput('vis1')),
      column(6,plotOutput('vis5'))
    ),
    DT::dataTableOutput("table2"),
    fluidRow(
      column(12,verbatimTextOutput("summary"))
      ),
    plotOutput('vis6')
  )
)

page_overzicht <- dashboardPage(
  title = "Overzicht",
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    # fluidRow(
    #   column(6,uiOutput("yaxis")),
    #   column(6,uiOutput("xaxis"))
    # ),
    plotOutput('vis2'),
    plotOutput('vis3'),
    plotOutput('vis4')
    
  )
)
