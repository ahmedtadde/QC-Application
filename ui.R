source('functions.R')
libraries()
shinyUI(
  navbarPage("Data QC UI",
             theme = "cosmos.css",
             
             tabPanel("ASCII",
                      fluidRow(
                        column(6, align = 'center', fileInput('ascii', 'Upload ASCII',
                                                              multiple = F,
                                                              accept = c('.xls',".xlsX")
                        )
                        ),
                        column(6, align = 'center', htmlOutput("ascii.status"))
                      ),
                      fluidRow(
                        column(6, align = 'center', uiOutput("record.type"))
                      ),
                      fluidRow(
                        column(12, align = 'center', DT::dataTableOutput("ascii.table"))
                      )
             ),
             
             tabPanel("SPREADSHEET QC",
                      fluidRow(
                        column(3, align = 'left', textInput("path", label="Path to Directory")), 
                        column(6, uiOutput("fileSelection")),
                        column(3, align = 'left', actionButton("fileSelectionGO", "Load File(s)"))
                      ),
                      tags$br(),
                      fluidRow(
                        
                        column(3,  align = 'left', uiOutput("recordtype.QC")),
                        column(3,  align = 'left', uiOutput("Apply.ASCII")),
                        column(6,  align = 'center', tableOutput("apply.ascii.message"))
                      ),
                      fluidRow(
                        column(4, align = 'left', actionButton("cutpointTables", "Select Cut Tables")),
                        column(4)
                      ),
                      tags$br(),tags$br(),
                      fluidRow( 
                        column(12, align = 'center', uiOutput("displayCutpointTables"))
                      ),
                      tags$br(),tags$br(),tags$br(),
                      fluidRow(
                        column(12, align = 'center',DT::dataTableOutput("table")))
             ),
             
             tabPanel("SPREADSHEETS COMPARISON",
                      fluidRow(
                        column(3, align = 'left', actionButton("file1.path", "Select Spreadsheet 1")),
                        column(3, align = 'left', htmlOutput("file1.status")),
                        column(3, align = 'left', actionButton("file2.path", "Select Spreadsheet 2")),
                        column(3, align = 'left', htmlOutput("file2.status"))
                      ),
                      tags$br(),
                      fluidRow(
                        
                        column(3, align = 'left', uiOutput("getKey1")),
                        column(3),
                        column(3, align = 'left', uiOutput("getKey2")),
                        column(3)
                      ),
                      tags$br(),
                      fluidRow(
                        column(3, align = 'left', uiOutput("relevantcols1")),
                        column(3),
                        column(3, align = 'left', uiOutput("relevantcols2")),
                        column(3)
                      ),
                      tags$br(),
                      fluidRow(
                        column(3, align = 'left', uiOutput("whichFilterColumns1")),
                        column(3),
                        column(3, align = 'left', uiOutput("whichFilterColumns2")),
                        column(3)
                      ),
                      tags$br(),
                      fluidRow(
                        column(3, align = 'left', uiOutput("whichFilterValues1")),
                        column(3),
                        column(3, align = 'left', uiOutput("whichFilterValues2")),
                        column(3)
                      ),
                      tags$br(),
                      fluidRow(
                        column(2, align = 'left', uiOutput("apply.filters")),
                        column(1, algin = 'left', actionButton("compareButton", "GO"))
                      ),
                      tags$br(),tags$br(),
                      fluidRow(
                        
                        uiOutput("displayComparisonReport")
                      )
             )
             
  )
)
