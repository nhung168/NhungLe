# ui.R

# prepare a list of station used for analyzing
station <- list('Hoan Kiem' = 'HoanKiem','Thanh Cong' = 'ThanhCong','Tan Mai' = 'TanMai','Kim Lien' = 'KimLien','Pham Dong' = 'PhamVanDong','Tay Mo' = 'TayMo','My Dinh' = 'MyDinh','Hang Dau' = 'HangDau','Chi Cuc BVMT' = 'ChiCucBVMT','Minh Khai' = 'MinhKhai','DSQPhap' = 'DSQPhap','Dam Trau' = 'DamTrau','Doi Binh' = 'DoiBinh','Quang Cau' = 'QuangPhuCau','Van Dinh' = 'VanDinh','Le Truc' = 'LeTruc','Tu Lien' = 'TuLien','Khuong Trung' = 'KhuongTrung','Dao Tu' = 'DaoDuyTu','Dong Thuc' = 'DongKinhNghiaThuc','Ly To' = 'LyThaiTo','Cau Dien' = 'CauDien','KDT.Tay Tay' = 'KDT.TayHoTay','KDT.Phap Van' = 'KDT.PhapVan','Van Quan' = 'VanQuan','An Khanh' = 'AnKhanh','Van Ha' = 'VanHa','Vong La' = 'VongLa','Kim Bai' = 'KimBai','Sai Son' = 'SaiSon','Lien Quan' = 'LienQuan','Chuc Son' = 'ChucSon','Xuan Mai' = 'XuanMai','Thanh Son' = 'ThanhXuanSocSon','Soc Son' = 'SocSon')

ui <- fluidPage(
  title = 'Analyze Hanoi Air Pollutant with Meteorology Variables',
  h1 ('Analyze Hanoi Air Pollutant with Meteorology Variables'),
  tabsetPanel(
    tabPanel('Analyze', #plot factorial by station
             selectInput(inputId = 'Station',
                         label = "Choose a station",
                         station,
                         multiple = FALSE),
             selectInput(inputId = 'Parameter',
                         label = 'Parameter:',
                         c('PM2.5','PM10','CO','NO2','SO2','O3'),
                         multiple = FALSE),
             fluidRow(column(width=12),plotOutput('time_series')),
             fluidRow(column(width=12),plotOutput('wd_plot'))
    ),
    tabPanel('Data',
             dateRangeInput(inputId ="dateRange" ,label = "Choose a date" ,start = as.Date('2017-01-01') , end =as.Date('2021-12-31')),
             dataTableOutput('hn_data'))
  )
)
