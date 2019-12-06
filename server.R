

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
########################################################### 
  
  output$first <- renderBillboarder({
    
    plot1<-df1%>%
    group_by(TARGET)%>%
    summarize(count=n())
  billboarder() %>% 
    bb_donutchart(data = plot1)%>% bb_legend(show = FALSE)%>% bb_title(text = "DEFAULT/NON-DEFAULT", position = "center")
 
  })
 
  output$second <- renderBillboarder({
    plot2<-df1%>%
      filter(TARGET==1)%>%
      group_by(NAME_INCOME_TYPE)%>%
      summarize(count=n())
    billboarder() %>% 
      bb_donutchart(data = plot2)%>% bb_legend(show = FALSE)%>% bb_title(text ="DEFAULT by INCOME TYPE",  position = "center")
  
  })
  
  output$third <- renderBillboarder({
    plot3<-df1%>%
      filter(TARGET==1)%>%
      group_by(FLAG_OWN_CAR)%>%
      summarize(count=n())
    billboarder() %>% 
      bb_donutchart(data = plot3)%>% bb_legend(show = FALSE)%>% bb_title(text = "DEFAULT by Owning Car", position = "center")
    
  })
  
  
  output$fourth <- renderBillboarder({
    plot4<-df1%>%
      filter(TARGET==1)%>%
      group_by(NAME_EDUCATION_TYPE)%>%
      summarize(count=n())
    billboarder() %>% 
      bb_donutchart(data = plot4)%>% bb_legend(show = FALSE)%>% bb_title(text = "DEFAULT by EDUCATION TYPE", position = "center")
    
  })
  
  output$fifth <- renderBillboarder({
    plot5<-df1%>%
      filter(TARGET==1)%>%
      group_by(NAME_CONTRACT_TYPE)%>%
      summarize(count=n())
    billboarder() %>% 
      bb_donutchart(data = plot5)%>% bb_legend(show = FALSE)%>% bb_title(text ="DEFAULT by Contract Type",  position = "center")
    
  })
  
##############################################################################  
  
output$bar1 <- renderPlot({  
ggplot() +
geom_bar(data=age_df_interval, aes(x=factor(age_group), y=count_percentage, fill=factor(target)),stat="identity", position="dodge")+
geom_text(data=age_df_interval,aes(x=age_group,y=count_percentage+0.5,label = paste0("",round(count_percentage,0)," %",sep=""),group=target), position=position_dodge(width=1),vjust=0,size=4,fontface = 'bold')+
labs(x="Age Group",y="Count",fill='Target')+  
theme(legend.position="bottom")
}) 
    
output$bar2 <- renderPlot({    
income_df %>%
filter(income<10e5)%>%
ggplot(aes(x = income)) +
geom_histogram(bins = 30,color="darkblue", fill="blue") +
labs(x= 'Income',y = 'Count')+
theme_bw()
})

output$bar3 <- renderPlot({
ggplot(data=credit_amt,aes(x = AMT_CREDIT)) +
  geom_histogram(bins = 30,fill="orange")+
  labs(x= 'Amount Credit',y = 'Count') +theme_bw()
})  
  
  
output$table = DT::renderDataTable({ 
  as.datatable(df_format,
               extensions=c("Buttons",'Scroller'),
               options = list(dom = 'Bfrtip',
                              scrollY = 500,
                              scroller = TRUE,
                              scrollX=TRUE,
                              pageLength = 10, lengthChange = TRUE,rownames= FALSE
               ))
  
})  

########################################################################################################  
  
df_products_upload <- reactive({
  inFile <- input$target_upload
  if (is.null(inFile))
    return(NULL)
  df <- read.csv(inFile$datapath, header = TRUE,sep = ",")
  return(df)
})

output$rawdata<- DT::renderDataTable({
  #df <- df_products_upload()
  DT::datatable( head(df_products_upload(), 10),options = list(pageLength = 10,scrollX=TRUE,searching = FALSE) )
})


#output$table <- renderUI({
  #print(dfSummary(df_products_upload(), graph.magnif = 0.8), 
        #method = 'render',
       # headings = FALSE,
       # bootstrap.css = FALSE)


#})


output$summaryTable <- renderUI({

  Uploaded_data<-df_products_upload()
  out <- print(dfSummary(Uploaded_data, 
                         graph.magnif = 0.7),
               method = 'render',
               omit.headings = TRUE,
               bootstrap.css = TRUE,
               graph.col = st_options("dfSummary.graph.col"),
               graph.magnif = st_options("dfSummary.graph.magnif"))
  
  return(out)
  
  
})
  
  
observe({
  nms <- names(df_products_upload())
  updateSelectInput(session,'xvar', choices=nms)
  updateSelectizeInput(session,'ytarget', choices=nms,selected=nms[1])
  updateSelectizeInput(session,'xfeatures', choices=nms, selected=nms)
})  

output$Yplot <- renderPlot({
  
  validate(
    need(df_products_upload() != "", "Please do not panic. You are getting this error becasue you did not upload data!")
  )
  
  #x<-input$xvar
  ggplot(df_products_upload(), aes_string(x=input$xvar)) +
    geom_histogram(bins = 50,colour="blue", fill="white")
})

############################################
# Download simulated data.
output$downloadmodeldata <- downloadHandler(
  filename = "sample.csv",
  content = function(con) {
    write_csv(df_model, con)
  }
)

######################################MODEL################################################

###Data prepare
htoo<-reactive({
  
  df3<-df_products_upload()
  string_2_factor_names <- df3 %>%
    select_if(is.character) %>%
    names()
  
  unique_numeric_values_tbl <-df3  %>%
    select_if(is.numeric) %>%
    map_df(~ unique(.) %>% length()) %>%
    gather() %>%
    arrange(value) %>%
    mutate(key = as_factor(key))
  
  factor_limit <- 7
  
  num_2_factor_names <- unique_numeric_values_tbl %>%
    filter(value < factor_limit) %>%
    arrange(desc(value)) %>%
    pull(key) %>%
    as.character()
  
  rec_obj <- recipe(~ ., data = df3) %>%
    #step_string2factor(string_2_factor_names) %>%
    step_num2factor(num_2_factor_names) %>%
    prep(stringsAsFactors = FALSE)
  train_new <- bake(rec_obj, df3)
  data_h2o <- as.h2o(train_new)
  
  return(data_h2o)
  
})

#observe({
  #nms2 <- names(htoo())
  #updateSelectizeInput(session,'ytarget', choices=nms2,selected=nms2[1])
  #updateSelectizeInput(session,'xfeatures', choices=nms2, selected=nms2)
#})  

split<-eventReactive(input$train,{
  splits_h2o <- h2o.splitFrame(htoo(), ratios = input$sld_testsplit, seed = 1358)
  return(splits_h2o)
})

aml <- eventReactive(input$train,{
  #train_h2o <- splits_h2o[[1]]
  #test_h2o <- splits_h2o[[2]]
  
  y <- input$ytarget
  x <- setdiff(input$xfeatures, y)
  
  aml <- h2o.automl(x = x, y = y,
                    training_frame = split()[[1]],
                    max_models =2,
                    seed = 1,
                    max_runtime_secs  = 999999)
  return(aml)
})  

#observeEvent(input$button,{


output$my<- DT::renderDataTable({
  
  lb <- aml()@leaderboard
  
  models=datatable(as.data.frame(lb),
                   extensions=c("Buttons",'Scroller'),
                   options = list(dom = 'Bfrtip',
                                  scrollY = 500,
                                  scroller = TRUE,
                                  scrollX=TRUE,
                                  pageLength = 5, lengthChange = TRUE,rownames= FALSE
                   ))
  
  return(models)
  
})

output$confusion_matrix<- DT::renderDataTable({
  automl_leader <-aml()@leader
  performance_h2o <- h2o.performance(automl_leader, newdata = split()[[2]])
  
  confusion=performance_h2o %>%
    h2o.confusionMatrix()
  
  conmatrix=datatable(as.data.frame(confusion))
  return(conmatrix)
})

output$rocplot <- renderPlot({
  automl_leader <-aml()@leader
  performance_h2o <- h2o.performance(automl_leader, newdata = split()[[2]])
  AUC=plot(performance_h2o, type='roc')
  return(AUC)
})


output$import <- renderPlot({

  automl_leader2 <-aml()@leader
importance=h2o.varimp_plot(automl_leader2, num_of_features = 10 )
return(importance)
})  

#}) #closing observe event for modelling

  })
