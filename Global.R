library(shiny)
library(shinyjs)
library(shinyBS)
library(data.table)
library(echarts4r)
library(billboarder)
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(formattable)
library(DT)
library(summarytools)
library(recipes)
library(h2o)
library(waiter)
library(shinycssloaders)
h2o.no_progress()  # turn off progress bars
h2o.init()

df1<- fread("./app_shiny.csv")
df_model<- fread("./MyData.csv")
#name=names(df1)
##############################################################################
age_df<-data.frame(target=df1$TARGET,age=as.integer(df1$DAYS_BIRTH/-365))
age_df$age_group<-cut(age_df$age,breaks=seq(20,70,by=5),right=TRUE)  
age_df_interval<-age_df%>%group_by(age_group,target)%>%summarise(count=n(),count_percentage=n()/nrow(df1)*100)
age_df_interval<-age_df_interval[-c(21),] 

income_df<-data.frame(target=df1$TARGET,income=df1$AMT_INCOME_TOTAL)

credit_amt<-df1 %>%filter(AMT_CREDIT < 2e6)

##################################################################################
df2<-df1[1:200]
df2<-df2%>%select(SK_ID_CURR,TARGET,CODE_GENDER,FLAG_OWN_CAR,FLAG_OWN_REALTY,AMT_INCOME_TOTAL,AMT_CREDIT,DAYS_BIRTH,DAYS_EMPLOYED)
df2$FLAG_OWN_CAR[df2$FLAG_OWN_CAR=="N"] <- "FALSE"
df2$FLAG_OWN_CAR[df2$FLAG_OWN_CAR=="Y"] <- "TRUE"
df2$FLAG_OWN_REALTY[df2$FLAG_OWN_REALTY=="N"] <- "FALSE"
df2$FLAG_OWN_REALTY[df2$FLAG_OWN_REALTY=="Y"] <- "TRUE"
df2$CODE_GENDER[df2$CODE_GENDER=="M"] <- "TRUE"
df2$CODE_GENDER[df2$CODE_GENDER=="F"] <- "FALSE"



df_format<-formattable(df2, list(
  DAYS_BIRTH = color_tile("white", "orange"),
  
  DAYS_EMPLOYED = color_tile("  light green ", "red"),
  
  TARGET = formatter("span",
                     style = x ~ formattable::style(color = ifelse(x  <1,  "blue", "red"), font.weight = "bold")),
  
  CODE_GENDER = formatter("span",
                          style = x ~ formattable::style(color = ifelse(x, "blue", "red"), font.weight = "bold"),
                          x~ icontext(ifelse(x, "star-empty", "star"), ifelse(x, "Male", "Female"))),
  
  
  area(col = c(AMT_INCOME_TOTAL, AMT_CREDIT)) ~ normalize_bar("pink", 0.4),
  
  FLAG_OWN_CAR = formatter("span",
                           style = x ~ formattable::style(color = ifelse(x, "green", "red")),
                           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
  CNT_FAM_MEMBERS = color_bar("lightblue"),
  FLAG_OWN_REALTY = formatter("span",
                              style = x ~ formattable::style(color = ifelse(x, "green", "red")),
                              x~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
))

##########################################################################################################


label.help <- function(label,id){
  HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}

#######################################################################################################
#df3<-df_model[1:4000]
#string_2_factor_names <- df3 %>%
 # select_if(is.character) %>%
  #names()

#unique_numeric_values_tbl <-df3  %>%
 # select_if(is.numeric) %>%
  #map_df(~ unique(.) %>% length()) %>%
  #gather() %>%
  #arrange(value) %>%
  #mutate(key = as_factor(key))

#factor_limit <- 7

#num_2_factor_names <- unique_numeric_values_tbl %>%
 # filter(value < factor_limit) %>%
  #arrange(desc(value)) %>%
  #pull(key) %>%
  #as.character()

#rec_obj <- recipe(~ ., data = df3) %>%
  #step_string2factor(string_2_factor_names) %>%
 # step_num2factor(num_2_factor_names) %>%
  #prep(stringsAsFactors = FALSE)
#train_new <- bake(rec_obj, df3)
#name=names(train_new)
#data_h2o <- as.h2o(train_new)
#####################################################################################################



