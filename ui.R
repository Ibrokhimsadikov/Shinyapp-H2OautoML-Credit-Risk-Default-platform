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

shinyUI(navbarPage(id='AC', title ='Data Wizards', 
                   theme = "style/style.css",
                   fluid = TRUE, 
                   collapsible = TRUE,
                   
 
                   # ----------------------------------
                   # tab panel 1 - Home
                   tabPanel("Home", icon = icon("list-alt"),
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
                   tabPanel("Dashboard", "Dashboard", icon = icon("users"),
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
     
      
     tabBox(
        side = "right", height = "250px", title = tagList(shiny::icon("gear"), "Data Map"),
        tabPanel("Sample", DT::dataTableOutput("table"))
        
      ) )
      
     # fluidRow( div(DT::dataTableOutput("table"), style = "font-size: 75%; width: 50%"))
      
      
                            
      
                            
                                      
                     
                            
                   ),
                   
                   # ----------------------------------
                   # tab panel 3 - Model
                   tabPanel("Model", #useShinydashboard(), 
                            icon = icon('cogs'),
                            
                            
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
                              p( 'Also keep in mind as it is Credit Risk Modelling platform,  Confusion matrix, ROC curve and Variable importance 
                                 plot only work if leader model has those parameters. In other words our modelling mainly designed for classification tasks'),
                              br(),
                              selectizeInput('ytarget',label='Select class variable', character(0), multiple=FALSE),
                              selectizeInput('xfeatures',label='Select independant variables',character(0), multiple = T),
                              hr(),
                              
                              sliderInput('sld_testsplit',label = label.help('Set Train fraction','lbl_testsplit'),min = 0.5,max = 0.9, value = 0.7),
                              bsTooltip(id = "lbl_testsplit", title = "Select fraction of data to set aside for train data", 
                                        placement = "right", trigger = "hover"),
                              actionButton( 'train', label = 'Click to Run AutoML',icon = icon('cogs'), class='btn-danger fa-lg'
                                          ),
                              hr(),
                              helpText('Once you are satisfied with model output you can generate predictions on Test data'),
                             actionButton('btn_viewPred',label = 'View AutoMl Predictions',icon=icon('table'),class='btn-success fa-lg')
                              
                              
                            ),
                            bsModal('prediction',title = 'H2O Leader Prediction',trigger = 'btn_viewPred',size = 'large',
                            selectizeInput('clientID',label='Select first clientID column and then Actual Target Column', character(0), multiple=T),
                            h4('The table below is the Predictions of AutoML Leader Model on Test Data') ,
                            p('So here you can compare model predictions with Actual Target and CustomerID it belongs too'), 
                            div(style='max-height:300px; overflow-y: scroll; position: relative',withSpinner(tableOutput('h2oprediction')))),
                        
                            
                            
                    
                            box(title = "Model Outputs", width=9, status = 'primary', solidHeader = TRUE,
                            h4('The table below is the Outcome of H2O AutoML') ,
                            withSpinner(tableOutput('my')),
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
                            
                           
                      
                            
                            ),
    
     
                   # ----------------------------------
    tabPanel('Scorecard', icon = icon("th"),
              tabItem("setup",
                     box(width =3,title = 'Set up Prediction attributes',solidHeader = T,status = 'primary',
                         selectInput("dept","SELECT CLIENT ID:", character(0), multiple = FALSE),
                         hr(),
                         h4("Set probability credit score Thresholds for gauge chart", align = "center"),
                         hr(),
                         sliderInput('poor',label ='Select POOR Score threshold from 0 to:',min = 20,max = 50, value = 40),
                         sliderInput('fair',label ='Select FAIR Score threshold from:',min = 50,max = 80, value = 65), 
                         sliderInput('good',label ='Select GOOD Score threshold from:',min =50,max = 90, value = 85),
                         actionButton( 'set', label = 'Apply selected',icon = icon('bar-chart'), class='btn-primary fa-lg'
                         )
                         ),
                     
                          
                          
                          
                   box(width = 9,title = 'Explore and Compare Predictions in Individual Level',solidHeader = T,status = 'primary',
                       div(style = "font-size: 10px; padding: 0px 0px; margin-top:-2em",
                       h4("Probability of Not-Default", align = "center")),
                       div(style = "font-size: 10px; padding: 0px 0px; margin-top:-10em",
                        echarts4rOutput("plot")),
                       #h4("Adjustable Probability Creditsocring gauge", align = "center"),
                       div(style = "font-size: 10px; padding: 0px 0px; margin-top:-8em", 
                       fluidRow(infoBoxOutput("sale"),
                                valueBoxOutput("o"),
                                valueBoxOutput("product"))),
                       div(style = "font-size: 10px; padding: 0px 0px; margin-top:-1em",
                       fluidRow(
                         column(9, align="right",
                                plotOutput("plot2", width='500px')
                         )
                       )
                       
                       ))
              ),
              
    
   
            box(width =3,title = 'Auto Creditscore Generator',solidHeader = T,status = 'primary',
                selectInput("score_id","SELECT CLIENT ID:", character(0), multiple = FALSE),
                selectInput("score_target","Select Target Class:", character(0), multiple = FALSE),
                sliderInput('score',label = 'Set Train fraction', min = 0.5,max = 0.9, value = 0.6),
                actionButton( 'set_bin', label = 'Apply selected',icon = icon('bar-chart'), class='btn-primary fa-lg'
                )
               
                
                
                
            ),
            
            
            
            
            box(width = 9,title = 'Scorecard:Good & Bad WOE Binning',solidHeader = T,status = 'primary',
               
                
                selectInput("select_bin","Select variable to plot binning:", character(0), multiple = FALSE),
                withSpinner(plotOutput('plot_bin'))
                
                
                
               ),
            
            fluidRow(
            box(width = 6,title = 'Scorecrad: Traditional GLM Model Output',solidHeader = T,status = 'primary', side = "right",
                
                withSpinner(plotOutput('plot_model'))
                
            ),
                
             
            box(width = 6,title = 'Overall Score Distribution',solidHeader = T,status = 'primary', side = "right",
                
                withSpinner(plotOutput('plot_score')),
                hr(),
                actionButton('btn_scorecard',label = 'Generate Creditscore',icon=icon('table'),class='btn-primary fa-lg'),
                column(8, align="center", bsModal('CARD',title = 'ScoreCARD',trigger = 'btn_scorecard',size = 'large',
                div(style='max-height:300px; overflow-y: scroll; position: relative',withSpinner(tableOutput('score_table')))))
                       
                
            ))        
            
              
              
    ),    
             
    bsModal('C',title = 'ScoreCARD',trigger = "Model",size = 'large'
            )                  
     
))



