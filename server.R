

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
########################################################### 
  
  observeEvent(input$AC, {
    
    if (input$AC == "Dashboard")  {
      showModal(modalDialog(
        title = "Quick Guildline on how to use app",
        div(id = "aa", style = "width: 1000px; height: 315px;", 
            HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/Ao3GKJRh9H4" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
        easyClose = TRUE
      ))
    }else{}
  })
  
  
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
  updateSelectizeInput(session,'clientID', choices=nms) #, selected=nms[1]
})  

output$Yplot <- renderPlot({
  
  validate(
    need(df_products_upload() != "", "Please do not panic. You are getting this error because you did not upload data!")
  )
  
 
                                         
  
  if (class(df_products_upload()[[input$xvar]]) == "integer" ||
      class(df_products_upload()[[input$xvar]]) == "numeric") {
    
     explore_distribution_numeric(df_products_upload()[[input$xvar]],vname = input$xvar)
                                          
   
  } else {
   distobj <- explore_distribution_categorical(df_products_upload()[[input$xvar]],vname = input$xvar)
   distobj$distplot
   
  
  }
  
  #x<-input$xvar
  #ggplot(df_products_upload(), aes_string(x=input$xvar)) +
    #geom_histogram(bins = 50,colour="blue", fill="white")
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


output$my<- renderTable({
  
  lb <- aml()@leaderboard
  
  models=as.data.frame(lb)
                   
  
  return(models)
  
})

output$confusion_matrix<- renderDataTable({
  automl_leader <-aml()@leader
  performance_h2o <- h2o.performance(automl_leader, newdata = split()[[2]])
  
  confusion=performance_h2o %>%
    h2o.confusionMatrix()
  
  conmatrix=DT::datatable(as.data.frame(confusion))
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
#########################Predictions##############################
test<-reactive({
validate(
  need(input$clientID != "", "Please do not panic. You are getting this error becasue you did not select ClientID  and Actual Target columns yet. Please make sure to select Client Identification number first followed by Actual Target column !")
)

prediction_h2o <- h2o.predict(aml()@leader, newdata = split()[[2]]) 
pred=as.data.frame(prediction_h2o)


ID=as.data.frame(split()[[2]][, input$clientID])

d=bind_cols(ID,pred )
return(d)
})


output$h2oprediction<- renderTable({
  test()
})

####################################################################

observeEvent(test(),  {
  nms2 <- test()[, 1]
  updateSelectInput(session,'dept', choices=nms2)
  
}) 

z<-eventReactive(input$set,{
  z=test()%>%filter(test()[1]==input$dept)%>%select(p0) 
  return(z)
})

output$plot <- renderEcharts4r({
  
  #z=test()%>%filter(test()[1]==input$dept)%>%select(p0)
  liquid <- data.frame(p0 = c( 0.70, 0.6))
  k=bind_rows(z(), liquid) 
  k %>% 
    e_charts() %>% 
    e_liquid(p0)#%>% 
    #e_title("Probability of Not-Default", align = 'center') 
})

z1<-eventReactive(input$set,{
  z1=test()%>%filter(test()[1]==input$dept)%>%select(TARGET)
})
output$sale <- renderInfoBox({
  
  infoBox("ACTUAL CLASS", if(z1() == 0){
    "Not Defaulted"
  } else {
    "Defaulted"
  },  
  icon = icon("credit-card"),
  color = "orange", fill=TRUE
  )
})

z2<-eventReactive(input$set,{
z2=test()%>%filter(test()[1]==input$dept)%>%select(predict)
})
output$o <- renderInfoBox({
infoBox("PREDICTED CLASS", if(z2() == 0){
    "Not Defaulted"
  } else {
    "Defaulted"
  },  
  icon = icon("credit-card"),
  color = "blue", fill=TRUE
  )
})

z3<-eventReactive(input$set,{
  z3=test()%>%filter(test()[1]==input$dept)%>%select(p1)
})
output$product <- renderInfoBox({
infoBox("PROBABILITY OF DEFAULT", z3(),  
  icon = icon("info"),
  color = "red", fill=TRUE
  )
})

z4<-eventReactive(input$set,{
  z4=test()%>%filter(test()[1]==input$dept)%>%select(p0)
  lc = as.list(z4)
  gg.gauge(as.numeric(lc)*100,breaks=c(0,input$poor,input$fair,input$good,100))  #as.numeric(input$dept)
})
output$plot2<-renderPlot({
   z4()
})

#######################################Scorecard#####################

observe({
  nms_score <- names(df_products_upload())
  updateSelectInput(session,'score_id', choices=nms_score)
  updateSelectInput(session,'score_target', choices=nms_score)
  
}) 


dt_filter<-eventReactive(input$set_bin,{
  df_bin=df_products_upload()
  x=input$score_id
  df_bin=df_bin%>%select(-x)
  dt_f = var_filter(df_bin, y=input$score_target) 
  return(dt_f)
}) 


bins<-reactive({
  bins=woebin(dt_filter(), y=input$score_target)
  return(bins)
  
}) 



observeEvent(bins(),{
 nms_bins <- names(bins())
  updateSelectInput(session,'select_bin', choices=nms_bins,selected=nms_bins[1])
}) 

output$plot_bin <- renderPlot({
woebin_plot(bins(), x=input$select_bin)
})


dt_list<-reactive({
  dt_list = split_df(dt_filter(), y=input$score_target, ratio =input$score, seed = 30)
  return(dt_list)
})

label_list<-reactive({
  label_list = lapply(dt_list(), function(x) x[[input$score_target]])
  return(label_list)
})

dt_woe_list<-reactive({
  dt_woe_list = lapply(dt_list(), function(x) woebin_ply(x, bins()))
  return(dt_woe_list)
})



m2<-reactive({

  form <- sprintf("%s~.",input$score_target)
  m1 <-glm(as.formula(form),family=binomial(),data=dt_woe_list()$train)
  m_step = stats::step(m1, direction="both", trace = FALSE)
  m2 = eval(m_step$call)
  return(m2)
})
output$plot_model <- renderPlot({

pred_list = lapply(dt_woe_list(), function(x) predict(m2(), x, type='response'))
perf = perf_eva(pred = pred_list, label = label_list())
return(perf)
})

score_list<-reactive({

   # score ------
   ## scorecard
    card = scorecard(bins(), m2())
  #  ## credit score
    score_list = lapply(dt_list(), function(x) scorecard_ply(x, card))
  return(score_list)  
})

output$plot_score <- renderPlot({


  perf_psi(score = score_list(), label = label_list())


})


output$score_table <- renderTable({
  

  test=as.data.frame(score_list()$test)
  train=as.data.frame(score_list()$train)
  train_test=force_bind(train, test)
  id=as.data.frame(df_products_upload()[[input$score_id]])
  result=bind_cols(id, train_test)
  names(result)[1] <- input$score_id
  return(result)
})





  })




