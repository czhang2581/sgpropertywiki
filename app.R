library(shiny)
library(psych)
library(stringr)
library(purrr)
library(readr)
library(dplyr)
library(tidyr)
library(reshape)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(devtools)
library(ggthemes)
library(scales)
library(DT)
library(sjlabelled)
library(xlsx)
library(patchwork)
library(rlist)
library(kableExtra)
library(ggrepel)
library(shinydashboard)



options(scipen = 999)

devv <- c("The Woodleigh Residences", "Amber Park", "Midwood", "JadeScape", "Sengkang Grand Residences", "Dairy Farm Residences", "The Reef at King's Dock",
          "JadeScape", "Normanton Park","Pasir Ris 8", "The Watergardens at Canberra","Irwell Hill Residences", "Parc Clematis", "One Bernam", "Hyll on Holland",
          "Leedon Green", "The Landmark","Clavon", "Verdale")%>%sort()

devvre <- c("The Panorama", "Sturdee Residences","Waterbank at Dakota", "Bartley Ridge","City Gate")%>%sort()

ui <- dashboardPage(
    
    dashboardHeader(title = "Development Info"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Details", tabName = "devsd", icon = icon("info")),
        menuItem("Floor Plan", tabName = "fl", icon = icon ("align-justify", lib = "glyphicon")),
        menuItem("Comparison", tabName = "compar", icon = icon ("object-align-bottom", lib = "glyphicon")),
        menuItem("Resale", tabName = "resa", icon = icon ("transfer", lib = "glyphicon"))
      )
    ),
    
    
    dashboardBody(

      
      tabItems(
        tabItem(tabName = "devsd",
                fluidRow(
                  box( width = 12, title = "Select a Development",
                    height = "150px",solidHeader = T,
                      selectInput("dev", "", devv)
                  )
                  
                ),
                
                fluidRow(
                  infoBoxOutput("pricsum"),
                  infoBoxOutput("psfsum"),
                  infoBoxOutput("transsum")
                  
                  
                ),
                
                fluidRow(
                  box( width = 12, title = "Summary Table", solidHeader = T, 

                      tableOutput("plot3")
                    
                  )
                ),
                
                fluidRow(height = "700px",
                         
                  box( width = 3, title = "Select Bedroom Types", solidHeader = T,height = 714,
                       checkboxInput ('all',"Select All/None", value = T),
                       checkboxGroupInput("types", "Select Bedroom Types",selected = '',
                                        tags$style(type="text/css", HTML("#test>*{float: left; margin-right: 15px; height: 20px;} #test {height: 20px;}")))
                       
                  ),
                  
                  tabBox( width = 9,
                    title = "Time Series", 
                   
                      tabPanel("Average PSF", plotOutput("price",height = 650)),
                      tabPanel("Average Price", plotOutput("pricetotal",height = 650)),
                      tabPanel("Transaction Volume", plotOutput("transac",height = 650))
                  )
                  
                  
                
                
                  
                  
              )
        ),
        
        tabItem(tabName = "fl",
                fluidRow(
                  box( title= "Select a Development", width = 12, height = "150px", solidHeader = T,
                    selectInput("dev6", "", devv)
                  )
                ),
                fluidRow(
                  box(width = 12, solidHeader = T,
                    uiOutput("pdfview")
                  )
                  
                )
        ),
        
        tabItem(tabName = "compar",
                fluidRow(
                  box(title = "Select Developments to Compare", width = 12, height = "150px", solidHeader = T,
                      selectInput("dev2", "Select Developments to Compare", devv, multiple = TRUE))
                ),
                
                fluidRow( height = "800px",
                  tabBox(width = 12,
                    
                    tabPanel("Average PSF", plotOutput("comppric2", height = "700px")),
                    tabPanel("Average Price", plotOutput("comppric", height = "700px")),
                    tabPanel("Transaction Volume", plotOutput("comppric3", height = "700px"))
              
                  )
                )
          ),
        
        tabItem(tabName = "resa",
                fluidRow(
                  box(title = "Select a Development", width = 6, height = "150px", solidHeader = T,
                      selectInput("dev3", "Select a Development", devvre)
                      ),
                  box(title = "Select Type of Sale", width = 6, height = "150px", solidHeader = T,
                      checkboxGroupInput ("check1", "Select Type of Sale",c("New Sale","Sub Sale","Resale"), selected = "Resale")
                    
                  )
                ),
                
                fluidRow(
                  box( width = 12, solidHeader = T,
                    "Summary Table", tableOutput("plot4")
                    )
                ),
                
                fluidRow( 
                  column( width = 12, align = "center",
                    tabBox( width = 12,
                          
                            tabPanel("Average PSF", plotOutput("price2", height = "650px")),
                            tabPanel("Average Price", plotOutput("pricetotal2", height = "650px")),
                            tabPanel("Transaction Volume", plotOutput("transac2", height = "650px"))
                  )
                          
                  )
                )
                
              )
                
            
                
      )
        
      
    )
    
)


server <- function(input, output, session) {
    
    district <- reactive({

        district <- list.load("AllD.RData") 
        return(district)
    })
    
    compd <- reactive({
        
        dist <- district()
        develop <- input$dev2
        
        la <- list()
        for (i in develop){
            l1 <- dist[[toupper(toString(i))]]
            for (j  in (1:length(l1$transaction))){
                l1$transaction[[j]]$Project = i
            }
            la$transaction <- c(la$transaction, l1$transaction)
        }
        
        x = la$transaction
        x=lapply(x, function(x) list_modify(x,"nettPrice"=NULL))
        
        myd<-data.frame(matrix(unlist(x), nrow=length(x), byrow=TRUE),stringsAsFactors=FALSE)
        
        myd$psf <- as.numeric(myd$X6) / (as.numeric(myd$X1)*10.76391041671)
        
        names(myd) <- c("Area", "FloorLevel","No. of Units","Date.of.Sale","Type of Sale","Price....","Property Type","District","Area Type", "Tenure","Development" ,"Unit.Price...psf.")
        
        myd$`Area..Sqft.` <-(as.numeric( myd$Area)*10.76391041671) %>% round(digits=0)
        
        myd$`Date.of.Sale`<-as.yearqtr(format(myd$`Date.of.Sale`), format = "%m%y")
        
        myd<-myd %>% mutate (`Size`=
                                 case_when(
                                     Area..Sqft. < 600 ~ "1",   
                                     Area..Sqft. >= 600 & Area..Sqft. <=800 ~ "2",  
                                     Area..Sqft. > 800 & Area..Sqft. <= 1000 ~ "3",  
                                     Area..Sqft. > 1000 ~ "4",  
                                     
                                 ))
        
        myd$Price.... <- as.numeric(myd$Price....)
        
        
        return(myd)
    })
    

    
    
    disframe2 <- reactive({
        
        validate(
            need(input$dev3 != "", "Please select the type of sale")
        )
        
        district <- district()
        develop <- input$dev3
        
        district <- district[[toupper(toString(develop))]]
        
        x = district$transaction
        
        x=lapply(x, function(x) list_modify(x,"nettPrice"=NULL))
        
        myd<-data.frame(matrix(unlist(x), nrow=length(x), byrow=TRUE),stringsAsFactors=FALSE)
        
        myd$psf <- as.numeric(myd$X6) / (as.numeric(myd$X1)*10.76391041671)
        
        names(myd) <- c("Area", "FloorLevel","No. of Units","Date.of.Sale","Type.of.Sale","Price....","Property Type","District","Area Type", "Tenure", "Unit.Price...psf.")
        
        myd$`Area..Sqft.` <-(as.numeric( myd$Area)*10.76391041671) %>% round(digits=0)
        
        myd$`Date.of.Sale`<-as.yearqtr(format(myd$`Date.of.Sale`), format = "%m%y")
        
        myd["Type.of.Sale"][myd["Type.of.Sale"] == 1] <- "New Sale"
        myd["Type.of.Sale"][myd["Type.of.Sale"] == 2] <- "Sub Sale"
        myd["Type.of.Sale"][myd["Type.of.Sale"] == 3] <- "Resale"
        
        myd <- myd %>% subset(myd$Type.of.Sale %in% input$check1) 
        
        return(myd)
    })
    
    disframe <- reactive({
        
        district <- district()
        develop <- input$dev
        
        district <- district[[toupper(toString(develop))]]
        
        x = district$transaction
        
        x=lapply(x, function(x) list_modify(x,"nettPrice"=NULL))
        
        myd<-data.frame(matrix(unlist(x), nrow=length(x), byrow=TRUE), stringsAsFactors=FALSE)
        
        myd$psf <- as.numeric(myd$X6) / (as.numeric(myd$X1)*10.76391041671)
        
        names(myd) <- c("Area", "FloorLevel","No. of Units","Date.of.Sale","Type of Sale","Price....","Property Type","District","Area Type", "Tenure", "Unit.Price...psf.")
        
        myd$`Area..Sqft.` <-(as.numeric( myd$Area)*10.76391041671) %>% round(digits=0)
        
        myd$`Date.of.Sale`<-as.yearqtr(format(myd$`Date.of.Sale`), format = "%m%y")
        
        
        return(myd)
    })
    
    
    prop <- reactive({
        
        prop <- disframe()
        
        
        if (input$dev == "Amber Park"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. %in% c(538,463) ~ "1+Study",   
                                            Area..Sqft. %in% c(463,484,549) ~ "1+Ensuite",  
                                            Area..Sqft. %in% c(700,829) ~ "2-Bedroom",  
                                            Area..Sqft. %in% c(678,743,753,872,883) ~ "2+Study",  
                                            Area..Sqft. %in% c(947,1109,1270) ~ "3-Bedroom",  
                                            Area..Sqft. %in% c(1572) ~ "4-Bedroom",  
                                            Area..Sqft. %in% c(1582,1798) ~ "4-Bedroom Premium",
                                            Area..Sqft. %in% c(1302) ~ "4+Study",  Area..Sqft. %in% c(2045,2142) ~ "5-Bedroom",  
                                            Area..Sqft. %in% c(2325,2336) ~ "5+Study",  Area..Sqft. >= 3000 ~ "Penthouse"
                                        ))
        }
        
        else if (input$dev == "The Woodleigh Residences"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. >= 570 & Area..Sqft. <= 592 ~ "2-Bedroom",
                                            Area..Sqft. >= 646 & Area..Sqft. <= 700 ~ "2-Bedroom Deluxe",
                                            Area..Sqft. >= 721 & Area..Sqft. <= 743 ~ "2-Flexi",
                                            Area..Sqft. >= 850 & Area..Sqft. <= 958 ~ "3-Bedroom",
                                            Area..Sqft. >= 1076 & Area..Sqft. <= 1119  ~ "3-Bedroom Deluxe",
                                            Area..Sqft. >= 1270 & Area..Sqft. <= 1281 ~ "4-Bedroom",
                                            Area..Sqft. >= 1475 & Area..Sqft. <= 1475 ~ "4-Bedroom Deluxe",
                                        ))
        }
        
        else if (input$dev == "JadeScape"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. == 527 ~ "1+Bedroom",
                                            Area..Sqft. >=646 & Area..Sqft. <=764 ~ "2-Bedroom",
                                            Area..Sqft. == 775 ~ "2-Bedroom Premium",
                                            Area..Sqft. >=904 & Area..Sqft. <=1055 ~ "3-Bedroom",
                                            Area..Sqft. >=1141 & Area..Sqft. <=1152 ~ "3-Bedroom Premium",
                                            Area..Sqft. == 1259 ~ "4-Bedroom",
                                            Area..Sqft. == 1421 ~ "4-Bedroom Deluxe",
                                            Area..Sqft. == 1647 ~ "4-Bedroom Suite",
                                            Area..Sqft. ==2099 ~ "5-Bedroom",
                                            Area..Sqft. == 4230 ~ "Penthouse"
                                        ))
        }
        
        else if (input$dev == "Midwood"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. >= 484 & Area..Sqft. <= 495 ~ "1-Bedroom",
                                            Area..Sqft. >= 549 & Area..Sqft. <= 549~ "1+Study",
                                            Area..Sqft. >= 635 & Area..Sqft. <= 700 ~ "2-Bedroom",
                                            Area..Sqft. >= 775 & Area..Sqft. <= 786 ~ "2+Study",
                                            Area..Sqft. >= 893 & Area..Sqft. <= 904  ~ "3-Bedroom",
                                            Area..Sqft. >= 990 & Area..Sqft. <=990 ~ "3+Yard",
                                            Area..Sqft. >= 1249 & Area..Sqft. <= 1259 ~ "4-Bedroom",
                                            
                                        ))
        }
        
        else if (input$dev == "Sengkang Grand Residences"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. >= 474 & Area..Sqft. <= 506 ~ "1+Study",
                                            Area..Sqft. >= 624 & Area..Sqft. <= 678 ~ "2-Bedroom",
                                            Area..Sqft. >= 732 & Area..Sqft. <= 764 ~ "2-Bedroom + Study",
                                            Area..Sqft. >= 936 & Area..Sqft. <= 947 ~ "3-Bedroom",
                                            Area..Sqft. >= 1023 & Area..Sqft. <= 1055 & Area..Sqft.!= 1012  ~ "3-Bedroom Premium",
                                            Area..Sqft. >= 1012 & Area..Sqft. <= 1012 ~ "3+Flexi",
                                            Area..Sqft. >= 1313 & Area..Sqft. <= 1324 ~ "4+Flexi",
                                            
                                            
                                        ))
        }
        
        else if (input$dev == "Dairy Farm Residences"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. >= 624 & Area..Sqft. <= 732 ~ "2-Bedroom",
                                            Area..Sqft. >= 764 & Area..Sqft. <= 775 ~ "2+Study",
                                            Area..Sqft. >=  915 & Area..Sqft. <= 1313 ~ "3-Bedroom",
                                            Area..Sqft. >=  1324 & Area..Sqft. <= 1475 ~ "4-Bedroom",
                                            
                                        ))
        }
        
        else if (input$dev == "The Reef at King's Dock"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. >= 431 & Area..Sqft. <= 657 ~ "1-Bedroom",
                                            Area..Sqft. >= 678 & Area..Sqft. <= 764 ~ "2-Bedroom",
                                            Area..Sqft. %in% c(893,883) ~ "2+Study/2-Premium",
                                            Area..Sqft. %in% c(980,1163) ~ "2-Bedroom Villa",
                                            Area..Sqft. %in% c(1076,1281) ~ "3+Study",
                                            Area..Sqft. %in% c(1216,1464,1249) ~ "3-Premium",
                                            Area..Sqft. %in% c(1345, 1572) ~ "3-Villa",
                                        ))
        }
        
        else if (input$dev == "Normanton Park"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. %in% c(517,527,484,581,495,603) ~  "1-Bedroom",
                                            Area..Sqft. %in% c(560,678,570,689,581,700) ~ "1+Study",
                                            Area..Sqft. %in% c(667,797,657,786,646, 753, 635)~ "2-Bedroom",
                                            Area..Sqft. %in% c(689,818,764,883,721, 861, 775, 893,732,872)~ "2-Bedroom Premium",
                                            Area..Sqft. %in% c(829, 947,850,980) ~ "2+Study",
                                            Area..Sqft. %in% c(936,1055,969,1109,958,1087,915,1023,904,1023,947,1076,915,1033) ~ "3-Bedroom",
                                            Area..Sqft. %in% c(1087,1227,1098,1238,1249,1238,1066,1238,1076) ~ "3-Bedroom Premium",
                                            Area..Sqft. %in% c(1195,1346) ~ "4-Bedroom+Study",
                                            Area..Sqft. %in% c(1313,1453,1475,1335,1496) ~ "4-Bedroom Premium",
                                            Area..Sqft. ==1615 |  Area..Sqft. == 1798 ~ "5-Bedroom",
                                            Area..Sqft. == 2110 ~ "Terrace House"
                                            
                                        ))
        }
        
        else if (input$dev == "Pasir Ris 8"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. >=517 & Area..Sqft. <=517 ~  "1-Bedroom Flexi",
                                            Area..Sqft. >=538 & Area..Sqft. <=538 ~ "1+Study",
                                            Area..Sqft. >=710 & Area..Sqft. <= 721 ~ "2-Bedroom",
                                            Area..Sqft. >=775 & Area..Sqft. <= 775 ~ "2-Bedroom Premium",
                                            Area..Sqft. ==829  ~ "2+Study",
                                            Area..Sqft. >=1023 & Area..Sqft. <=1066 ~ "3-Bedroom",
                                            Area..Sqft. >=1259 &  Area..Sqft. <= 1302 ~ "3+Guest",
                                            Area..Sqft. == 1464 ~ "4+Flexi",
                                            Area..Sqft. == 1539 |  Area..Sqft. == 1550 ~ "4+Guest",
                                            
                                        ))
        }
        
        else if (input$dev == "The Watergardens at Canberra"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. >=646 & Area..Sqft. <=678 ~  "2-Bedroom",
                                            Area..Sqft. %in% c(753,926,872,721,893,904,936) ~ "2-Bedroom Premium",
                                            Area..Sqft. ==797 | Area..Sqft. == 969 ~ "2+Study",
                                            Area..Sqft. %in% c(958,1163,1152,904,1076) ~ "3-Bedroom",
                                            Area..Sqft. ==1012 | Area..Sqft. ==1206  ~ "3+Study",
                                            Area..Sqft. ==1109 | Area..Sqft. ==1313 | Area..Sqft. == 1324~ "3-Premium+Study",
                                            Area..Sqft. %in% c(1302,1528) ~ "4-Bedroom",
                                        ))
        }
        
        else if (input$dev == "Irwell Hill Residences"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. %in% c(398) ~  " Studio",
                                            Area..Sqft. %in% c(452,495,646,506,667) ~ "1+Study",
                                            Area..Sqft. %in% c(614,764,624,603,721,732) ~ "2-Bedroom",
                                            Area..Sqft. %in% c(657,667,818,678,829) ~ "2-Premium",
                                            Area..Sqft. == 861  ~ "3-Bedroom",
                                            Area..Sqft. == 1270~ "3-Premium",
                                            Area..Sqft. %in% c(2185,2228) ~ "4-Bedroom Penthouse",
                                            Area..Sqft. %in% c(1539,1582) ~ "4-Bedroom",
                                            Area..Sqft. %in% c(2605) ~ "5-Bedroom Penthouse",
                                        ))
        }
        
        else if (input$dev == "Parc Clematis"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. %in% c(452,624) ~  "1-Bedroom",
                                            Area..Sqft. %in% c(517,506) ~ "1+Study",
                                            Area..Sqft. %in% c(710,721,689) ~ "2-Bedroom",
                                            Area..Sqft. %in% c(743) ~ "2+Study",
                                            Area..Sqft. %in% c(700,850,732)  ~ "2-Bedroom Dual-Key",
                                            Area..Sqft. %in% c(915,904,893,1076,883,829,1033,861,1055)~ "3-Bedroom",
                                            Area..Sqft. %in% c(990,1216,969,1184) ~ "3-Bedroom Dual Key",
                                            Area..Sqft. %in% c(1249,1044,1259) ~ "3-Bedroom Premium",
                                            Area..Sqft. %in% c(1238,1475,1238) ~ "4-Bedroom",
                                            Area..Sqft. %in% c(1496,1765,1464,1733,1292,1755) ~ "4-Bedroom Premium",
                                            Area..Sqft. %in% c(1636, 1927, 1668, 1970) ~ "5-Bedroom",
                                            Area..Sqft. %in% c(1711,1981) ~ "5-Bedroom Premium",
                                            Area..Sqft. == 3832 ~ "Bungalow",
                                            Area..Sqft. %in% c(2659,3466) ~ "Terrace",
                                            Area..Sqft. %in% c(2605,2164,2669,2217,1991) ~ "Pentouse"
                                            
                                        ))
            prop <- prop %>% filter(row_number() %% 2 != 0)
        }
        
        else if (input$dev == "One Bernam"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. %in% c(441,452,463) ~  "1-Bedroom",
                                            Area..Sqft. %in% c(732,700) ~ "2-Bedroom",
                                            Area..Sqft. %in% c(872,807,829) ~ "2+Study",
                                            Area..Sqft. %in% c(1421) ~ "3-Bedroom",
                                            Area..Sqft. %in% c(1744,4306,1948)  ~ "Penthouse",
                                        ))
        }
        
        else if (input$dev == "Hyll on Holland"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. %in% c(570) ~  "2-Bedroom Suite",
                                            Area..Sqft. %in% c(603,614) ~ "2-Bedroom Select",
                                            Area..Sqft. %in% c(657) ~ "2-Bedroom Classic",
                                            Area..Sqft. %in% c(700,721,710) ~ "2-Bedroom Deluxe",
                                            Area..Sqft. %in% c(1055)  ~ "3+Study",
                                            Area..Sqft. %in% c(936)  ~ "3-Bedroom Deluxe",
                                        ))
        }
        
        
        else if (input$dev == "Leedon Green"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. %in% c(474,603) ~  "1-Bedroom",
                                            Area..Sqft. %in% c(538,678,581,689) ~ "1+Study",
                                            Area..Sqft. %in% c(614,710,753,700,775,840,807,797,807,840,667,786) ~ "2-Bedroom",
                                            Area..Sqft. %in% c(818,872,926) ~ "2+Study",
                                            Area..Sqft. %in% c(958,990)  ~ "3-Bedroom",
                                            Area..Sqft. %in% c(1356,1485,1604)  ~ "3-Bedroom Exclusive",
                                            Area..Sqft. %in% c(1044,1076,1163)  ~ "3-Bedroom Premium",
                                            Area..Sqft. %in% c(1496,1615,1744)  ~ "4-Bedroom Exclusive",
                                            Area..Sqft. %in% c(2680,2594,2400,2411,2433)  ~ "Garden Villa"
                                        ))
        }
        
        else if (input$dev == "The Landmark"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. %in% c(495,517) ~  "1-Bedroom",
                                            Area..Sqft. %in% c(764,678,753) ~ "2-Bedroom",
                                            Area..Sqft. %in% c(1076,1141) ~ "3-Bedroom"
                                        ))
        }
        
        else if (input$dev == "Clavon"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. %in% c(527) ~  "1+Study",
                                            Area..Sqft. %in% c(678) ~ "2-Bedroom",
                                            Area..Sqft. %in% c(764) ~ "2-Bedroom Premium",
                                            Area..Sqft. %in% c(958) ~ "3-Bedroom",
                                            Area..Sqft. %in% c(1130) ~ "3-Bedroom Premium",
                                            Area..Sqft. %in% c(1281,1356) ~ "4-Bedroom",
                                            Area..Sqft. %in% c(1582) ~ "4-Bedroom Premium",
                                            Area..Sqft. %in% c(1690) ~ "5-Bedroom",
                                        ))
        }
        
        else if (input$dev == "Verdale"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. %in% c(463,474) ~  "1-Bedroom",
                                            Area..Sqft. %in% c(560,570) ~ "1+Study",
                                            Area..Sqft. %in% c(614) ~ "2-Bedroom",
                                            Area..Sqft. %in% c(700,743,710,732,753) ~ "2-Bedroom Deluxe",
                                            Area..Sqft. %in% c(947) ~ "3-Bedroom",
                                            Area..Sqft. %in% c(1001,1012,1033) ~ "3-Bedroom Deluxe",
                                            Area..Sqft. %in% c(1410) ~ "4-Bedroom",
                                            Area..Sqft. %in% c(1518,1528) ~ "4-Bedroom + Family",
                                            Area..Sqft. %in% c(1873) ~ "5+Study"
                                        ))
        }
        
        else if (input$dev == "Bartley Vue"){
          prop = prop %>% mutate (`Unit Type`=
                                    case_when(
                                      Area..Sqft. %in% c(657,797) ~  "2-Bedroom",
                                      Area..Sqft. %in% c(732,850,861) ~ "2-Bedroom Premium",
                                      Area..Sqft. %in% c(947,1087) ~ "3-Bedroom",
                                      Area..Sqft. %in% c(1066,1044,1259,1270) ~ "3-Bedroom Premium",
                                      Area..Sqft. %in% c(1356, 1539) ~ "4+Study"
                                    ))
        }
        
        
        
        
        
        
        
        
        
        
        return(prop)
        
    })
    
    devp <- reactive({
        input$dev6
    })
    
    
    output$pdfview<-renderUI({
        tags$iframe(style="height:1050px; width:100%", src=paste0(devp(),".pdf"))
    })
   
    
    prop2 <- reactive({

        
        prop <- disframe2()
        
        
        if (input$dev3 == "The Panorama"){
            prop = prop %>% mutate (`Unit Type`=
                                        case_when(
                                            Area..Sqft. >=431 & Area..Sqft. <= 474 ~  "1-Bedroom",
                                            Area..Sqft. >=678 & Area..Sqft. <= 700 ~ "2-Bedroom",
                                            Area..Sqft. >=775 & Area..Sqft. <= 797 ~ "2+Study",
                                            Area..Sqft. >=990 & Area..Sqft. <= 1066 ~ "3-Bedrom",
                                            Area..Sqft. >=1109 & Area..Sqft. <= 1163 ~ "3+Study",
                                            Area..Sqft. >=1313 & Area..Sqft. <= 1335 ~ "4-Bedroom",
                                            Area..Sqft. >=1561 &  Area..Sqft. <= 1561 ~ "5-Bedroom",
                                            Area..Sqft. >=1841 & Area..Sqft. <= 1916 ~ "4-Bedroom Penthouse",
                                            Area..Sqft. >=2239 &  Area..Sqft. <= 2411 ~ "5-Bedroom Penthouse",
                                        ))
        }
        
        else if (input$dev3 == "Sturdee Residences"){
            prop = prop %>% mutate (`Unit Type`= 
                                        case_when(
                                            Area..Sqft. ==420  ~  "1-Bedroom",
                                            Area..Sqft. == 570 ~ "2-Bedroom",
                                            Area..Sqft. ==657 ~ "2-Bedroom Premium",
                                            Area..Sqft. ==721 ~ "2+Study",
                                            Area..Sqft. ==829 ~ "3-Bedroom",
                                            Area..Sqft. %in% c(947,1033,1044) ~ "3-Bedroom Premium",
                                            Area..Sqft. == 1302 ~ "4-Bedroom",
                                            Area..Sqft. >=1399 & Area..Sqft. <= 1830 ~ "Penthouse",
                                        
                                        ))
        }
        
        else if (input$dev3 == "Waterbank at Dakota"){
            prop = prop %>% mutate (`Unit Type`= 
                                        case_when(
                                            Area..Sqft. %in% c(484,581)  ~  "1-Bedroom",
                                            Area..Sqft. %in% c(624,753,635,786) ~ "1+Study",
                                            Area..Sqft. %in% c(883,1087,1421) ~ "2-Bedroom",
                                            Area..Sqft. %in% c(1141,1389,1152,1528,1184,1432,1173,1464,1130,1259,1615,1528) ~ "3-Bedroom",
                                            Area..Sqft. %in% c(1281) ~ "3-Bedroom Dual Key",
                                            Area..Sqft. %in% c(1572,1981) ~ "4-Bedroom",
                                            Area..Sqft. %in% c(2390,2820)~ "Penthouse",
                                            
                                        ))
        }
        
        else if (input$dev3 == "Bartley Ridge"){
            prop = prop %>% mutate (`Unit Type`= 
                                        case_when(
                                            Area..Sqft. %in% c(463,549,441,517,495,635,560,592)  ~  "1-Bedroom",
                                            Area..Sqft. %in% c(743,840,721,883,732,893,764) ~ "1+Study",
                                            Area..Sqft. %in% c(829,990,1345,850,969,1378,936,1389,947,1399,861,1410,1313) ~ "2-Bedroom",
                                            Area..Sqft. %in% c(969,1109,1119,1561,980,1130,1582) ~ "3-Bedroom",
                                            Area..Sqft. %in% c(1033,1625,1044,1227,1593,1615) ~ "3-Bedroom Dual Key",
                                            Area..Sqft. %in% c(1173,1249,1787,1841,1163,1259,1755) ~ "4-Bedroom",
                                            Area..Sqft. %in% c(1302,1464,1862,1927)~ "Penthouse",
                                            Area..Sqft. %in% c(1550,1658,2120)~ "Penthouse",
                                            
                                        ))
        }
        
        else if (input$dev3 == "City Gate"){
            prop = prop %>% mutate (`Unit Type`= 
                                        case_when(
                                            Area..Sqft. %in% c(452,484,431)  ~  "1-Bedroom",
                                            Area..Sqft. %in% c(560,570) ~ "2-Bedroom",
                                            Area..Sqft. %in% c(753,775,689,710,807,678,700) ~ "2-Bedroom Dual Key",
                                            Area..Sqft. %in% c(904,915) ~ "3-Bedroom Dual Key",
                                            Area..Sqft. %in% c(1216) ~ "3-Bedroom Penthouse",
                                            Area..Sqft. %in% c(1604) ~ "4-Bedroom Penthouse"
                                            
                                        ))
        }
        
    })
    
   observe({
       updateCheckboxGroupInput(
          session,
         "types",
          choices  = unique(prop()$`Unit Type`) %>% sort(),
         selected = if (input$all) unique(prop()$`Unit Type`) %>% sort()
       )
        
    })
    
    output$plot3 <- function(){
        
        
        pric<-prop() %>%
            group_by(`Unit Type`) %>%
            summarize( `Area (sq ft)` = ifelse(min(Area..Sqft.) == max(Area..Sqft.), toString(min(Area..Sqft.)), 
                                               str_c(min(Area..Sqft.),' - ', max(Area..Sqft.)))
                ,`No. of Sales`=n(),
                      `Transacted Min` = str_c("$",format(min(as.numeric(Price....)), big.mark = ',', 
                                                          small.interval = 3),
                                               " ($",
                                               format(Unit.Price...psf.[Price....==min(Price....)][1], big.mark = ",", digits = 4),
                                               "psf)"), 
                      `Transacted Max` = str_c("$",format(max(as.numeric(Price....)), big.mark = ',', 
                                                          small.interval = 3),
                                               " ($",
                                               format(Unit.Price...psf.[Price....==max(Price....)][1], big.mark = ",", digits = 4),
                                               "psf)"),
                      
                      `Transacted Average` = 
                          str_c('$',
                                format(mean(as.numeric(Price....)), big.mark = ',', 
                                       small.interval = 3,digits = 4),' ($',
                                format(mean(as.numeric(Unit.Price...psf.)), big.mark = ',', 
                                       small.interval = 3, digits = 4), "psf)"))
        tot <- c( `Unit Type`='Total',`Area (sq ft)` = '', `No. of Sales` = sum(pric$`No. of Sales`), `Transacted Min`= '', `Transacted Max`= '', 
                  `Transacted Average` = str_c('$',
                                               format(mean(as.numeric(prop()$Price....)), big.mark = ',', 
                                                      small.interval = 3,digits = 4),' ($',
                                               format(mean(as.numeric(prop()$Unit.Price...psf.)), big.mark = ',', 
                                                      small.interval = 3, digits = 4), "psf)"))                                                                                                     
        pric <- rbind(pric, tot)
        
        pric %>% kbl(format = "html", align="llccc", escape=F) %>% kable_material(protect_latex = T)%>% row_spec(0, bold=T, color = 'white', background = "#bf9000") %>% 
            kable_styling(full_width = T, font_size = 16) %>% row_spec(nrow(pric), bold = T, italic = T)%>% row_spec(1:nrow(pric), color = 'black', background = "white")
        
    }
    
    output$pricsum <- renderInfoBox({
      infoBox( "Average Price",
        str_c('$',
            format(mean(as.numeric(prop()$Price....)), big.mark = ',', 
             small.interval = 3,digits = 4)), 
        icon = icon("list"),color = "green"
      )
    })
    
    output$psfsum <- renderInfoBox({
      infoBox("Average PSF",
        str_c('$',
            format(mean(as.numeric(prop()$Unit.Price...psf.)), big.mark = ',', 
                   small.interval = 3,digits = 4)),
        icon = icon("thumbs-up"),color = "blue"
      )
    })
    
    output$transsum <- renderInfoBox({
      infoBox("Transaction Volumn",
        prop()%>%nrow(),
        icon = icon("credit-card"),color = "yellow"
      )
    })
    
    
    
    output$plot4 <- function(){
        validate(
            need(input$check1 != "", "Please Select the Type of Sale")
        )
        
        validate(
            need(is.na(prop2()$Price....) ==F , "No Transaction Found")
        )
        

        
        pric<-prop2() %>%
            group_by(`Unit Type`) %>%
            summarize( `Area (sq ft)` = ifelse(min(Area..Sqft.) == max(Area..Sqft.), toString(min(Area..Sqft.)), 
                                               str_c(min(Area..Sqft.),' - ', max(Area..Sqft.)))
                       ,`No. of Sales`=n(),
                       `Transacted Min` = str_c("$",format(min(as.numeric(Price....)), big.mark = ',', 
                                                           small.interval = 3),
                                                " ($",
                                                format(Unit.Price...psf.[Price....==min(Price....)][1], big.mark = ",", digits = 4),
                                                "psf)"), 
                       `Transacted Max` = str_c("$",format(max(as.numeric(Price....)), big.mark = ',', 
                                                           small.interval = 3),
                                                " ($",
                                                format(Unit.Price...psf.[Price....==max(Price....)][1], big.mark = ",", digits = 4),
                                                "psf)"),
                       
                       `Transacted Average` = 
                           str_c('$',
                                 format(mean(as.numeric(Price....)), big.mark = ',', 
                                        small.interval = 3,digits = 4),' ($',
                                 format(mean(as.numeric(Unit.Price...psf.)), big.mark = ',', 
                                        small.interval = 3, digits = 4), "psf)"))
        tot <- c( `Unit Type`='Total',`Area (sq ft)` = '', `No. of Sales` = sum(pric$`No. of Sales`), `Transacted Min`= '', `Transacted Max`= '', 
                  `Transacted Average` = str_c('$',
                                               format(mean(as.numeric(prop2()$Price....)), big.mark = ',', 
                                                      small.interval = 3,digits = 4),' ($',
                                               format(mean(as.numeric(prop2()$Unit.Price...psf.)), big.mark = ',', 
                                                      small.interval = 3, digits = 4), "psf)"))                                                                                                     
        pric <- rbind(pric, tot)
        
        pric %>% kbl(format = "html", align="llccc", escape=F) %>% kable_material(protect_latex = T)%>% row_spec(0, bold=T, color = 'white', background = "#bf9000") %>% 
            kable_styling(full_width = T, font_size = 16) %>% row_spec(nrow(pric), bold = T, italic = T)
    }
    
    
    unitprice<- reactive({ 
        
        
        unitprice<-prop() %>% subset(`Unit Type` %in% input$types) %>% group_by(Date.of.Sale) %>% dplyr::summarize ('Avg' = mean(Unit.Price...psf.), 'Sold' = n(), 
                                                                                                                    'Total' = mean(as.numeric(Price....)) )
        return (unitprice)
    })
    
    unitpricedet <- reactive({
        
        validate(
            need(input$types != "", "")
        )
        
        unitpricedet <- prop() %>% subset(`Unit Type` %in% input$types) %>% group_by(Date.of.Sale, `Unit Type`) %>% dplyr::summarize ('Avg' = mean(Unit.Price...psf.), 'Sold' = n())
        unitpricedet$Sold2 <- ifelse(
            as.numeric(unitpricedet$Sold) >3, unitpricedet$Sold, NA
        )
        
        unitpricedet$Date.of.Sale <- as.yearqtr(unitpricedet$Date.of.Sale)
        return (unitpricedet)
    })
    
    unitprice2<- reactive({ 
        
        validate(
            need(input$check1 != "", "")
        )
        
        validate(
            need(is.na(prop2()$Price....) ==F , "")
        )
        
        unitprice<-prop2() %>% group_by(Date.of.Sale) %>% dplyr::summarize ('Avg' = mean(Unit.Price...psf.), 'Sold' = n(), 
                                                                                                                    'Total' = mean(as.numeric(Price....)) )
        return (unitprice)
    })
    
    unitpricedet2 <- reactive({
        
        validate(
            need(input$check1 != "", "")
        )
        
        validate(
            need(is.na(prop2()$Price....) ==F , "")
        )
        
        unitpricedet <- prop2()  %>% group_by(Date.of.Sale, `Unit Type`) %>% dplyr::summarize ('Avg' = mean(Unit.Price...psf.), 'Sold' = n())
        unitpricedet$Sold2 <- ifelse(
            as.numeric(unitpricedet$Sold) >3, unitpricedet$Sold, NA
        )
        
        unitpricedet$Date.of.Sale <- as.yearqtr(unitpricedet$Date.of.Sale)
        return (unitpricedet)
    })
    
    output$price <- renderPlot({
        
        validate(
            need(input$types != "", "Please select a Bedroom Type")
        )
        
        selbed <- unitprice()
        
        g<-ggplot(selbed) +
            geom_point(aes(x=as.yearqtr(Date.of.Sale),y=as.numeric(Avg)))+
            geom_line(aes(x=as.yearqtr(Date.of.Sale),y=as.numeric(Avg)))+ggtitle("Price")+
            geom_label(data=selbed,aes(x=Date.of.Sale,y=Avg,label = str_c('$',round(Avg,0))), size = 4.5) + 
            scale_x_yearqtr(breaks = seq(from = min(selbed$Date.of.Sale), 
                                         to = max(selbed$Date.of.Sale), by = 0.25),format = "%YQ%q") + 
            theme(panel.grid.minor.x =element_blank())+
            geom_hline(yintercept = sum(selbed$Avg*selbed$Sold)/
                           sum(selbed$Sold), color="red", linetype = "dashed") + 
            xlab("Quarter") + ylab("Average Psf ($)")+
            geom_label(aes(x = tail(Date.of.Sale, n=1), 
                           y = sum(Avg*Sold)/sum(Sold), 
                           label = str_c('$',round(sum(Avg*Sold)/
                                                   sum(Sold), 0)),  
                           family = "serif"), colour="red3", hjust=-1.5, size = 4)+
            coord_cartesian(xlim= c(min(selbed$Date.of.Sale), max(selbed$Date.of.Sale)), clip = 'off')+ theme(plot.margin = unit(c(1,3,1,1), "lines"),
                                                                                                              axis.title = element_text(size=14), axis.text = element_text(size=12))
            
        g
    })
    
    output$transac <- renderPlot({
        selbed <- unitpricedet()
        selbed$Date.of.Sale <- as.yearqtr(selbed$Date.of.Sale)
        
        
        g<-ggplot(data=selbed, aes(x=Date.of.Sale, y = Sold, fill = `Unit Type`)) +
            geom_bar(position= "stack",stat="identity")+
            geom_text(aes(label=Sold2), size= 3, position = position_stack(vjust=0.5),
                      colour= 'white') + scale_y_continuous()+ 
            geom_text(aes(label = stat(y),group = Date.of.Sale), colour = 'red3', size=4, stat = "summary", fun=sum, vjust=-0.4)+
            scale_x_yearqtr(breaks = seq(from = min(selbed$Date.of.Sale), 
                                         to = max(as.yearqtr(selbed$Date.of.Sale)), by = 0.25),format = "%YQ%q")+ 
            theme(panel.grid.minor.y=element_blank())+ theme(plot.margin = unit(c(1,3,1,1), "lines"))+ 
          xlab("Quarter")
        
        g
        
    })
    
    output$pricetotal <- renderPlot({
        validate(
            need(input$types != "", "")
        )
        
        selbed <- unitprice()
        
        g<-ggplot(selbed) +
            geom_point(aes(x=as.yearqtr(Date.of.Sale),y=as.numeric(Total)))+
            geom_line(aes(x=as.yearqtr(Date.of.Sale),y=as.numeric(Total)))+ggtitle("Price")+
            geom_label(data=selbed,aes(x=Date.of.Sale,y=Total,label = str_c('$',format(round(Total,0), small.interval = 3, big.mark = ','))), size=4.5) + 
            scale_x_yearqtr(breaks = seq(from = min(selbed$Date.of.Sale), 
                                         to = max(selbed$Date.of.Sale)+1, by = 0.25),format = "%YQ%q") + 
            theme(panel.grid.minor.x =element_blank())+
            geom_hline(yintercept = sum(selbed$Total*selbed$Sold)/
                           sum(selbed$Sold), color="red", linetype = "dashed") + 
            xlab("Quarter") + ylab("Average Price ($)")+
            geom_label(aes(x = tail(Date.of.Sale, n=1), 
                          y = sum(Total*Sold)/sum(Sold), 
                          label = str_c('$',format(round(sum(Total*Sold)/
                                                      sum(Sold), 1), small.interval = 3, big.mark = ',' )),  
                          family = "serif"), colour="red3", hjust=-0.8)+
            coord_cartesian(xlim= c(min(selbed$Date.of.Sale), max(selbed$Date.of.Sale)), clip = 'off')+ theme(plot.margin = unit(c(1,4,1,1), "lines"))
        
        g
    })
    
    
    output$price2 <- renderPlot({
        selbed <- unitprice2()
        
        g<-ggplot(selbed) +
            geom_point(aes(x=as.yearqtr(Date.of.Sale),y=as.numeric(Avg)))+
            geom_line(aes(x=as.yearqtr(Date.of.Sale),y=as.numeric(Avg)))+ggtitle("Price")+
            geom_label(data=selbed,aes(x=Date.of.Sale,y=Avg,label = str_c('$',round(Avg,0))), size = 4.5) + 
            scale_x_yearqtr(breaks = seq(from = min(selbed$Date.of.Sale), 
                                         to = max(selbed$Date.of.Sale), by = 0.25),format = "%YQ%q") + 
            theme(panel.grid.minor.x =element_blank())+
            geom_hline(yintercept = sum(selbed$Avg*selbed$Sold)/
                           sum(selbed$Sold), color="red", linetype = "dashed") + 
            xlab("Quarter") + ylab("Average Psf ($)")+
            geom_label(aes(x = tail(Date.of.Sale, n=1), 
                          y = sum(Avg*Sold)/sum(Sold), 
                          label = str_c('$',round(sum(Avg*Sold)/
                                                      sum(Sold), 0)),  
                          family = "serif"), colour="red3", hjust=-1.5)+
            coord_cartesian(xlim= c(min(selbed$Date.of.Sale), max(selbed$Date.of.Sale)), clip = 'off')+ theme(plot.margin = unit(c(1,3,1,1), "lines"))
        
        g
    })
    
    output$transac2 <- renderPlot({
        selbed <- unitpricedet2()
        selbed$Date.of.Sale <- as.yearqtr(selbed$Date.of.Sale)
        
        
        g<-ggplot(data=selbed, aes(x=Date.of.Sale, y = Sold, fill = `Unit Type`)) +
            geom_bar(position= "stack",stat="identity")+
            geom_text(aes(label=Sold2), size= 3, position = position_stack(vjust=0.5),
                      colour= 'white') + scale_y_continuous()+ 
            geom_text(aes(label = stat(y),group = Date.of.Sale), colour = 'red3', size=4, stat = "summary", fun=sum, vjust=-0.4)+
            scale_x_yearqtr(breaks = seq(from = min(selbed$Date.of.Sale), 
                                         to = max(as.yearqtr(selbed$Date.of.Sale)), by = 0.25),format = "%YQ%q")+ 
            theme(panel.grid.minor.y=element_blank())+ theme(plot.margin = unit(c(1,2,1,1), "lines"))+ 
          xlab("Quarter")
        
        g
        
    })
    
    output$pricetotal2 <- renderPlot({
        
        selbed <- unitprice2()
        
        g<-ggplot(selbed) +
            geom_point(aes(x=as.yearqtr(Date.of.Sale),y=as.numeric(Total)))+
            geom_line(aes(x=as.yearqtr(Date.of.Sale),y=as.numeric(Total)))+ggtitle("Price")+
            geom_label(data=selbed,aes(x=Date.of.Sale,y=Total,label = str_c('$',format(round(Total,0), small.interval = 3, big.mark = ','))), size = 4.5) + 
            scale_x_yearqtr(breaks = seq(from = min(selbed$Date.of.Sale), 
                                         to = max(selbed$Date.of.Sale)+1, by = 0.25),format = "%YQ%q") + 
            theme(panel.grid.minor.x =element_blank())+
            geom_hline(yintercept = sum(selbed$Total*selbed$Sold)/
                           sum(selbed$Sold), color="red", linetype = "dashed") + 
            xlab("Quarter") + ylab("Average Price ($)")+
            geom_label(aes(x = tail(Date.of.Sale, n=1), 
                          y = sum(Total*Sold)/sum(Sold), 
                          label = str_c('$',format(round(sum(Total*Sold)/
                                                             sum(Sold), 1), small.interval = 3, big.mark = ',' )),  
                          family = "serif"), colour="red3", hjust=-0.8)+
            coord_cartesian(xlim= c(min(selbed$Date.of.Sale), max(selbed$Date.of.Sale)), clip = 'off')+ theme(plot.margin = unit(c(1,4,1,1), "lines"))
        
        g
    })
    
    
    

    
    output$comppric <- renderPlot({
        
        validate(
            need(input$dev2 != "", "Please select the developments you want to compare")
        )
       myd <-  compd()
       mydd<- myd %>% group_by(Development,Size) %>% dplyr::summarise("Avg" = mean(Price....), "Sold" = n())
       myddd<- myd %>% group_by(Development) %>% dplyr::summarise("Avg_Total" = mean(Price....))
       mydd<-merge(mydd, myddd, by="Development")
       
       gg<-ggplot(mydd,aes(x= Size,y=Avg,colour = Development)) +
           geom_point()+ggtitle("Price Comparison")+
           geom_text(data=mydd,aes(x=Size,y=Avg,label = format(str_c('$',round(Avg,0)), big.mark = ',', 
                                                               small.interval = 3)), position = position_dodge(width=0.8), size = 4.5,show.legend = F) + 
           theme(panel.grid.minor.x =element_blank())+
           geom_hline(data = mydd, aes(yintercept = Avg_Total, colour = Development),linetype = "dashed", show.legend = F) + 
           ylab("Average Psf ($)") + scale_y_continuous(breaks = round(seq(round_any(min(mydd$Avg),1000), max(mydd$Avg), by = 500000),1),labels = scales::comma) +
           geom_label(aes(x = tail(as.numeric(Size), n=1), 
                          y = Avg_Total, 
                          label = str_c('$',round(Avg_Total,0)),  
                          family = "serif"), show.legend = F, hjust=-1.5)+
           theme(legend.position="top") + scale_x_discrete(labels= c("<600 sq ft", "600-800 sq ft","801-1000 sq ft", "1000+ sq ft"))+
           coord_cartesian(xlim= c(min(as.numeric(mydd$Size)), max(as.numeric(mydd$Size))), clip = 'off')+ theme(plot.margin = unit(c(1,3,1,1), "lines"))
       gg
    })
    
    output$comppric2 <- renderPlot({
        
        validate(
            need(input$dev2 != "", "Please select the developments you want to compare")
        )
        myd <-  compd()
        mydd2 <- myd %>% group_by(Development,Size) %>% dplyr::summarise("Avg" = mean(Unit.Price...psf.), "Sold" = n())
        
        myddd<- myd %>% group_by(Development) %>% dplyr::summarise("Avg_Total" = mean(Unit.Price...psf.))
        
        mydd2<-merge(mydd2, myddd, by="Development")
        
        
        ggplot(mydd2,aes(x= Size,y=Avg,colour = Development)) +
            geom_point()+ggtitle("Psf Comparison")+
            geom_text(data=mydd2,aes(x=Size,y=Avg,label = format(str_c('$',round(Avg,0)), big.mark = ',', 
                                                                 small.interval = 3)), position = position_dodge(width=0.8), size = 4.5,show.legend = F) + 
            theme(panel.grid.minor.x =element_blank())+
            geom_hline(data = mydd2, aes(yintercept = Avg_Total, colour = Development),linetype = "dashed", show.lesgend = F) + 
            ylab("Average Psf ($)") + scale_y_continuous(breaks = round(seq(round_any(min(mydd2$Avg),10), max(mydd2$Avg), by = 100),1),labels = scales::comma) +
            geom_label(aes(x = tail(as.numeric(Size), n=1), 
                           y = Avg_Total, 
                           label = str_c('$',round(Avg_Total,0)),  
                           family = "serif"), show.legend = F, hjust=-2.2)+
            theme(legend.position="top") + scale_x_discrete(labels= c("<600 sq ft", "600-800 sq ft","801-1000 sq ft", "1000+ sq ft"))+
            coord_cartesian(xlim= c(min(as.numeric(mydd2$Size)), max(as.numeric(mydd2$Size))+0.25), clip = 'off')+ theme(plot.margin = unit(c(1,3,1,1), "lines"))
    })
    
    output$comppric3 <- renderPlot({
        validate(
            need(input$dev2 != "", "Please select the developments you want to compare")
        )
        myd <-  compd()
        mydd<- myd %>% group_by(Development,Size) %>% dplyr::summarise("Avg" = mean(Price....), "Sold" = n())
        myddd<- myd %>% group_by(Development) %>% dplyr::summarise("Avg_Total" = mean(Price....))
        mydd<-merge(mydd, myddd, by="Development")
        
        
        ggplot(data=mydd, aes(x=Size, y = Sold, fill = Development)) +
            geom_bar(position= "dodge",stat="identity")+
            geom_text(aes(label=Sold), size= 4, position = position_dodge2(width=0.9), vjust=1.5,
                      colour= 'white') + scale_y_continuous()+
            scale_x_discrete()+ 
            theme(panel.grid.minor.y=element_blank())+ scale_x_discrete(labels= c("<600 sq ft", "600-800 sq ft","801-1000 sq ft", "1000+ sq ft"))
    })
    
    
}


shinyApp(ui = ui, server = server)
