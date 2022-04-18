library(shiny)
library(DT)
library(utils)
library(tidyverse)
library(openair)

rm(list = ls(all.names = TRUE))

# function to get data from server
## function to load Pollutant data
LoadToEnvironment <- function(){
  rm(list=ls())
  load('hn_data_2017_2022.RData')
  data <- get(ls()[3])
}

## function to load back trajactory
LoadTraj <- function(){
  rm(list=ls())
  load('hn_data_2017_2022.RData')
  traj <- get(ls()[8])
}

# ui.R

# prepare a list of station used for analyzing
station <- list('Hoan Kiem' = 'HoanKiem','Thanh Cong' = 'ThanhCong','Tan Mai' = 'TanMai','Kim Lien' = 'KimLien','Pham Van Dong' = 'PhamVanDong','Tay Mo' = 'TayMo','My Dinh' = 'MyDinh','Hang Dau' = 'HangDau','Chi Cuc BVMT' = 'ChiCucBVMT','Minh Khai' = 'MinhKhai','DSQPhap' = 'DSQPhap','Dam Trau' = 'DamTrau','Doi Binh' = 'DoiBinh','Quang Cau' = 'QuangPhuCau','Van Dinh' = 'VanDinh','Le Truc' = 'LeTruc','Tu Lien' = 'TuLien','Khuong Trung' = 'KhuongTrung','Dao Tu' = 'DaoDuyTu','Dong Thuc' = 'DongKinhNghiaThuc','Ly To' = 'LyThaiTo','Cau Dien' = 'CauDien','KDT.Tay Tay' = 'KDT.TayHoTay','KDT.Phap Van' = 'KDT.PhapVan','Van Quan' = 'VanQuan','An Khanh' = 'AnKhanh','Van Ha' = 'VanHa','Vong La' = 'VongLa','Kim Bai' = 'KimBai','Sai Son' = 'SaiSon','Lien Quan' = 'LienQuan','Chuc Son' = 'ChucSon','Xuan Mai' = 'XuanMai','Thanh Son' = 'ThanhXuanSocSon','Soc Son' = 'SocSon')

ui <- fluidPage(
    title = 'Analyze Hanoi Air Pollutant with Meteorology Variables',
    h1 ('Analyze Hanoi Air Pollutant with Meteorology Variables'),
    tabsetPanel(
      tabPanel('Analyze', #plot factorial by station
               dateRangeInput(inputId ="dateRange2" ,
                              label = "Choose a date" ,
                              start = as.Date('2017-01-01'), 
                              end =as.Date('2021-12-31')),
               selectInput(inputId = 'Station',
                           label = "Choose a station (You could select multiple)",
                           station,
                           multiple = TRUE),
               selectInput(inputId = 'Parameter',
                           label = 'Parameter:',
                           c('PM2.5','PM10','CO','NO2','SO2','O3'),
                           multiple = FALSE),
               fluidRow(column(width=12,
                        h2('1. Observe changes by time series'),
                        actionButton(inputId = 'Run_time','Process'),
                        plotOutput('time_series'))),
               h2('2. Diurnal variation'),
               actionButton(inputId = 'Run_diurnal','Process'),
               fluidRow(column(width=8,
                               plotOutput('day_diurnal_plot')),
                        column(width=4,
                               plotOutput('hour_diurnal_plot'))),
               h2('3. Air mass trajectory'),
               actionButton(inputId = 'Run_traj','Process'),
               fluidRow(column(width=5,
                               plotOutput('traj_plot')),
                        column(width=5,
                               plotOutput('cwt_plot'))),
               fluidRow(column(width=12,
                        h2('4. Observe changes by wind direction'),
                        actionButton(inputId = 'Run_wd','Process'),
                        plotOutput('wd_plot')))
      ),
      tabPanel('Data',
               dateRangeInput(inputId ="dateRange" ,label = "Choose a date" ,start = as.Date('2017-01-01') , end =as.Date('2021-12-31')),
               dataTableOutput('hn_data'))
      )
    )
#app.R

server <- function(input,output){
  
  #Load Data
  re <- reactive({LoadToEnvironment()})
  output$hn_data = renderDataTable({re()}  %>% filter(date >= as.POSIXct(input$dateRange[1],format='%Y-%m-%d %H:%M') & 
                                                   date <= as.POSIXct(input$dateRange[2],format='%Y-%m-%d %H:%M')),
                                    options = list(pageLength = 20,
                                    initComplete = I('function(setting, json) { alert("done"); }')
  ))
  
  #Create a Function to select multiple stations
  data_input <- reactive({
    input_1 <- {LoadToEnvironment()} %>% filter(station %in% input$Station &
                                                date >= as.POSIXct(input$dateRange2[1],format='%Y-%m-%d %H:%M') & 
                                                date <= as.POSIXct(input$dateRange2[2],format='%Y-%m-%d %H:%M')) ;
    input_2 <- aggregate(input_1[,2:16], by = list(date = input_1$date),FUN='median', na.action = na.omit);
    return(input_2)
  })
  
  #1. Plot time series 
  action_1 <- eventReactive(input$Run_time,{timePlot({data_input()},
                                                     pollutant = input$Parameter,
                                                     avg.time = 'day')})
  output$time_series <- renderPlot({action_1()})
  
  #2. Diurnal variation
  action_3 <- eventReactive(input$Run_diurnal,
                            plot({{timeVariation({data_input()},
                                                 pollutant = input$Parameter,
                                                 cols = 'jet')}}, subset = 'day.hour'))
  
  output$day_diurnal_plot <- renderPlot({action_3()})
  
  action_4 <- eventReactive(input$Run_diurnal,
                            plot({{timeVariation({data_input()},
                                                 pollutant = input$Parameter,
                                                 cols = 'jet')}}, subset = 'hour'))
  
  output$hour_diurnal_plot <- renderPlot({action_4()})
  
  #3. Back Trajactory
  action_5 <- eventReactive(input$Run_traj,{trajCluster({LoadTraj()}%>% filter (date >= as.POSIXct(input$dateRange2[1],format='%Y-%m-%d %H:%M') & 
                                                                                date <= as.POSIXct(input$dateRange2[2],format='%Y-%m-%d %H:%M')) ,
                                                           method = "Angle", n.cluster = 4
                                                         , cols = c("blue","purple","green","red")
                                                         ,projection = "conic"
                                                         ,orientation = c(80,100,0),parameters = c(45)
                                                         ,xlim=c(95,123), ylim=c(30,5)
                                                         , grid.col = "transparent")})
 output$traj_plot <- renderPlot({action_5()})
 
 ## plot cwt
 ### consol data traj and pollutant
 data_input_2 <- reactive(
  consol_data <- merge({LoadTraj()} %>% select(date,lat,lon,height,hour.inc),
                       {data_input()}, by ='date'))
  
 ### create event to plot cwt
  action_6 <- eventReactive(input$Run_traj, {trajLevel({data_input_2()} %>% filter (date >= as.POSIXct(input$dateRange2[1],format='%Y-%m-%d %H:%M') & 
                                                                                 date <= as.POSIXct(input$dateRange2[2],format='%Y-%m-%d %H:%M')),
                                                       statistic="cwt",
                                                       pollutant= input$Parameter ,
                                                       col="jet",
                                                       smooth = TRUE,
                                                       projection = "conic",
                                                       orientation = c(80,100,0),
                                                       parameters = c(45),
                                                       xlim=c(95,123), ylim=c(30,5),
                                                       grid.col = "transparent")})
  output$cwt_plot <- renderPlot({action_6()})
  #4. Plot wind direction
  action_2 <- eventReactive(input$Run_wd,{polarPlot({data_input()},
                                                    pollutant = input$Parameter, type = 'year',
                                                    cols = 'jet')})
  
  output$wd_plot <- renderPlot({action_2()})
  
  
}

shinyApp(ui = ui, server = server)

