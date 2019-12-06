#   ____________________________________________________________________________
#   UI                                                                      ####




### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Colors                                                                  ####

#C10250 purple
#03BCC0 green
#D2D945 yellow/green
#FCB040 orange
#FF5850 red
#436983 hipster blue

shinyUI(navbarPage(title ='Data Wizards',
                   theme = "style/style.css",
                   fluid = TRUE, 
                   collapsible = TRUE,
                   
                   # ----------------------------------
                   # tab panel 1 - Home
                   tabPanel("Home",
                            includeHTML("home.html"),
                            tags$script(src = "plugins/scripts.js"),
                            tags$head(
                              tags$link(rel = "stylesheet", 
                                        type = "text/css", 
                                        href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                              tags$link(rel = "icon", 
                                        type = "image/png", 
                                        href = "images/logo_icon.png")
                            )
                            
                   ),
                   
                   # ----------------------------------
                   # tab panel 2 - Dashboard
                   tabPanel("Dashboard",
                            useShinydashboard(),
                             # include dependencies
                            
                            
      fluidRow(
      infoBox("Total Applicants", "305,405", icon = icon("users"), color = "orange",fill = TRUE),
      infoBox("DEFAULT Rate", "8.1%", icon = icon("line-chart"), color = "green",fill = TRUE),
      infoBox("TOTAL Loan Amount", '184,207,084,196 LC',  icon = icon("money"), color = "blue",fill = TRUE)),
                              
      fluidRow(splitLayout(cellWidths = c("20%","20%","20%","20%","20%"), 
                           billboarderOutput("first"), billboarderOutput("second"),
                           billboarderOutput("third"), billboarderOutput("fourth"),billboarderOutput("fifth"))),
                     
                              
      fluidRow( 
        
         tabBox(
           height = "400px",   title = tagList(shiny::icon("bar-chart-o"), "Distribution"),  
          selected = "Age", 
          tabPanel("Age", "Percentage of Clients among different Age Group",plotOutput('bar1')),
          tabPanel("Income", "Distribution of Income", plotOutput('bar2')),
          tabPanel("Credit", "Distribution of Amount Credit", plotOutput('bar3'))
        ),
     
      #tags$style(type = 'text/css', 
                 #'.nav-tabs { background-color:  blue;}',
                 #'.nav-tabs-default .nav-tabs-brand{color: white;}',
                 #'.tab-panel{ background-color:  blue; color: white}',
                 #'.nav-tabs-nav li.active:hover a, .nav-tabs-nav li.active a {
                 #background-color: green ;
                 #border-color: green;
                 #}'
           
     # ),
      tabBox(
        side = "right", height = "250px", title = tagList(shiny::icon("gear"), "Data Map"),
        tabPanel("Sample", DT::dataTableOutput("table"))
        
      ) )
      
      #fluidRow( div(DT::dataTableOutput("table"), style = "font-size: 75%; width: 50%"))
      
      
                            
      
                            
                                      
                     
                            
                   ),
                   
                   # ----------------------------------
                   # tab panel 3 - Model
                   tabPanel("Model", #useShinydashboard(),
                            
                            tabItem("setup",
                                    box(width =4,title = 'Input Dataset',solidHeader = T,status = 'primary',
                                         fileInput('target_upload',label = 'Upload CSV File or press link to get sample', accept = c("text/csv",
                                                                                              "text/comma-separated-values,text/plain",
                                                                                              ".csv")),
                                        #submitButton( "Confirm Submission"  ),
                                        downloadLink("downloadmodeldata", "Download simulated data for trial"),
                                        hr(),
                                        actionButton('btn_viewData',label = 'View Raw Data',icon=icon('table'),
                                                     style="color: #fff; background-color: #33c706; border-color: #2e6da4"),
                                        actionButton('btn_viewData2',label = 'View Exploratory Data',icon=icon('table'),
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        hr()
                                        
                                        ),
                                    
                                    bsModal('data',title = 'Dataset',trigger = 'btn_viewData',size = 'large',
                                            DT::dataTableOutput('rawdata')) ,
                                    
                                    
                                    bsModal('data2',title = 'Exploratory Dataset Analysis',trigger = 'btn_viewData2',size = 'large',
      
                                            uiOutput("summaryTable")),
                                    
                                    box(width = 8,title = 'Explore Distributions',solidHeader = T,status = 'primary',
                                        selectInput('xvar',label=label.help('Choose variable:','lbl_xvar'), character(0), multiple=FALSE),
                                        bsTooltip(id = "lbl_xvar", title = "Try and see distribution plot of selected variable", 
                                                  placement = "right", trigger = "hover"),
                                        hr(),
                                        plotOutput('Yplot',height=300))   
                                    
                                    ),
                    tabItem("modelpara",   
                            box(
                              title = "Model Attributes", width=3, status = 'primary', solidHeader = TRUE,
                              p('Please be patient while model is being trained, it will take awhile once run button below pressed.'),
                              p( 'Also keep in mind as it is work in progress demo,  Confusion matrix, ROC curve and Variable importance 
                                 plot only work if leader model has those parameters.'),
                              br(),
                              selectizeInput('ytarget',label='Select class variable', character(0), multiple=FALSE),
                              selectizeInput('xfeatures',label='Select independant variables',character(0), multiple = T),
                              hr(),
                              
                              sliderInput('sld_testsplit',label = label.help('Set Train fraction','lbl_testsplit'),min = 0.5,max = 0.9, value = 0.7),
                              bsTooltip(id = "lbl_testsplit", title = "Select fraction of data to set aside for train data", 
                                        placement = "right", trigger = "hover"),
                              actionButton( 'train', label = 'Click to Run AutoML',icon = icon('cogs'), class='btn-danger fa-lg'
                                          ),
                              br(),
                              helpText('Once you are satisfied with model output you can save it as PDF file'),
                              br(),
                              downloadButton("downloadprediction", "Download-UNAVAILABLE yet")
                              
                              
                            ),
                           
                            box(title = "Model Outputs", width=9, status = 'primary', solidHeader = TRUE,
                                h4('The table below is the Outcome of H2O AutoML') ,
                                withSpinner(DT::dataTableOutput('my')),
                                hr(),
                              h4('The table below is the Outcome of H2O Leader CONFUSION MATRIX table'),
                                DT::dataTableOutput('confusion_matrix'),
                                hr(),
                              h4('The plot below is the Outcome of H2O Leader ROC CURVE'),
                                plotOutput('rocplot',height=350),
                                hr(),
                              h4('The plot below is the Outcome of H2O Leader Variable Importance '),
                                plotOutput('import',height=350)
                                
                                ))
                            
                           
                            
                            
                            
                            
                            
                            
                            
                       
                       
                       
                       
                     
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            )
                   
                   # ----------------------------------
                   
))



