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
library(scorecard)
h2o.no_progress()  # turn off progress bars
h2o.init()

df1<- fread("./app_shiny.csv")
df_model<- fread("./final.csv")
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
explore_distribution_numeric <- function(x, vname = "numeric") {
  x <- x[!is.na(x)]
  nuniq <- length(unique(x))
  if (nuniq < 30) {
    nbins <- 10
  } else if (nuniq >= 30 && nuniq < 300) {
    nbins <- 20
  } else {
    nbins <- 30
  }
  d <- data.frame(var = x)
  g <- ggplot(d, aes(x)) +
    theme_bw() +
    geom_histogram(aes(y = ..density..), bins = nbins, colour = "black", fill = "#56B4E9") +
    geom_density(size = 1) +
    ggtitle(paste0("Distribution - ", vname)) +
    xlab(NULL) +
    ylab(NULL) +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5, vjust = 0.5),
      axis.text = element_text(size = 15),
      panel.border = element_blank()
    )
  g
}

explore_countperc_categorical <- function(x, max_values = 30) {
  df <- data.frame(Value = x, stringsAsFactors = FALSE)
  df <- df %>%
    group_by(Value) %>%
    summarise(Frequency = n()) %>%
    mutate(Proportion = Frequency / sum(Frequency)) %>%
    ungroup() %>%
    mutate(Value = ifelse(is.na(Value), "MISSING", Value)) %>%
    arrange(desc(Frequency))
  if (nrow(df) > max_values) {
    frequency_o <- sum(df[max_values:nrow(df), 2])
    proportion_o <- sum(df[max_values:nrow(df), 3])
    df <- bind_rows(df[1:(max_values - 1), ],
                    data.frame(Value = "OTHERS", Frequency = frequency_o,
                               Proportion = proportion_o,
                               stringsAsFactors = FALSE))
  }
  df
}


explore_distribution_categorical <- function(x, vname = "categorical") {
  d <- explore_countperc_categorical(x)
  g <- ggplot(d, aes(factor(Value), Proportion)) +
    theme_bw() +
    geom_col(width = 0.3, colour = "black", fill = "#56B4E9") +
    geom_text(aes(y = Proportion + .02,
                  label = sprintf("%.2f%%", Proportion * 100)), size = 3) +
    scale_y_continuous(
      limits = c(0, 1.1),
      breaks = seq(0, 1.1, by = 0.1),
      labels = scales::percent
    ) +
    ggtitle(paste0("Distribution - ", vname)) +
    xlab(NULL) +
    ylab(NULL) +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5, vjust = 0.5),
      axis.text = element_text(size = 15),
      panel.border = element_blank()
    )
  list( distplot = g)
}



#####################################################################################################


gg.gauge <- function(pos,breaks=c(0,30,50,70,100)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="limegreen")+
    geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="forestgreen")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
    annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
}

######################Bind dataframe###########

force_bind = function(df1, df2) {
  colnames(df2) = colnames(df1)
  bind_rows(df1, df2)
}