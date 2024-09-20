#  Installing required libraries

if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(janitor)){
  install.packages("janitor")
  library(janitor)
}

if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}

if(!require(zoo)){
  install.packages("zoo")
  library(zoo)
}

if(!require(rebus)){
  install.packages("rebus")
  library(rebus)
}

if(!require(plotly)){
  install.packages("plotly")
  library(plotly)
}

if(!require(shinydashboard)){
  install.packages("shinydashboard")
  library(shinydashboard)
}

if(!require(cansim)){
  install.packages("cansim")
  library(cansim)
}

if(!require(DT)){
  install.packages("DT")
  library(DT)
}

if(!require(htmltools)){
  install.packages("htmltools")
  library(htmltools)
}

if(!require(sf)){
  install.packages("sf")
  library(sf)
}

if(!require(leaflet)){
  install.packages("leaflet")
  library(leaflet)
}

if(!require(rmapshaper)){
  install.packages("rmapshaper")
  library(rmapshaper)
}

if(!require(geodata)){
  install.packages("geodata")
  library(geodata)
}

if(!require(here)){
  install.packages("here")
  library(here)
}

if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

options(scipen=999999)



# Downloading Canadian Geo Data and simplifying it 

canada_prov<- geodata::gadm("Canada", level = 1, path = ".",resolution = 2)
canada_prov<-sf::st_as_sf(canada_prov)
canada_prov<- rmapshaper::ms_simplify(canada_prov, keep = 0.002, keep_shapes = TRUE)

# Downloading zip file for NPR data and reading the csv file 
temp <- tempfile()
download.file("https://www150.statcan.gc.ca/n1/tbl/csv/17100121-eng.zip", here(temp))
imm_new <- read_csv(unz(here(temp), "17100121.csv"))
unlink(temp)


# Downloading zip file for NPR data and reading the csv file 
imm_new<-(imm_new) %>% clean_names(.) %>% 
  mutate(datemonth=ref_date %>% as.yearmon(.) %>% 
           as_date) %>% 
  mutate(date=paste0(substr(imm_new$ref_date,1,4),"-Q", lubridate::quarter(substr(imm_new$ref_date,6,7) %>% as.numeric))) %>% 
  group_by(ref_date, geo) %>% 
  mutate(total_imm_cat=ifelse(non_permanent_resident_types=="Total, non-permanent residents",value,0),
         total_imm_cat=sum(total_imm_cat),
         percentage_cat=round(value/total_imm_cat*100,digits = 1)) %>% 
  ungroup() %>% 
  group_by(ref_date, non_permanent_resident_types) %>% 
  mutate(total_imm_canada=ifelse(geo=="Canada",value,0),
         total_imm_canada=sum(total_imm_canada),
         percentage_prov=round(value/total_imm_canada*100,digits = 1)) %>% 
           ungroup() 


# Downloading zip file for inter-provincial immigration data and reading the csv file 

temp <- tempfile()
download.file("https://www150.statcan.gc.ca/n1/tbl/csv/17100020-eng.zip", here(temp))
int_prov_in <- read_csv(unz(here(temp), "17100020.csv"))
unlink(temp)

# Data manipulation adding required variables
int_prov_in<-
  int_prov_in %>% 
  clean_names(.) %>% 
  filter(ref_date>="2009-01") %>% 
  mutate(datemonth=(ref_date %>% as.yearmon(.) %>% as_date)) %>% 
  
  mutate(value=ifelse(interprovincial_migration=="Out-migrants",
                      -value, value)) %>% group_by(geo,ref_date) %>%
  mutate(net_intp_imm=sum(value)) %>%
  ungroup()%>% 
  group_by(geo, interprovincial_migration) %>% 
  mutate(moving_sum_imm=rollapplyr(value, 4,sum, fill=NA)) %>% 
  ungroup() %>% 
  group_by(ref_date,geo) %>% 
  mutate(moving_sum_imm1=sum(moving_sum_imm)) %>% 
  ungroup()%>% 
  mutate(yq=as.yearqtr(ref_date, format = "%Y-%m"))

int_prov_in_ <- int_prov_in %>% filter(geo!="Canada")  %>% 
  mutate(NAME_1=ifelse(geo=="Quebec", "Québec",geo))%>% 
  filter(ref_date>="2010-01", 
         geo!="Canada") 


# Data manipulation before merging with map data
imm_new_prov_ <- imm_new %>% filter(geo!="Canada")  %>% 
  mutate(NAME_1=ifelse(geo=="Quebec", "Québec",geo), 
         yq= as.yearqtr(ref_date, format = "%Y-%m"))

# Merging NPR data and geo data
imm_new_prov<-sp::merge(canada_prov, imm_new_prov_, by="NAME_1", duplicateGeoms = TRUE)
# Merging Inter-provincial migration data and geo data
int_prov_map_data<-sp::merge(canada_prov, int_prov_in_, by="NAME_1", duplicateGeoms = TRUE)

# Detaching the map data 
rm(canada_prov)


# Downloading zip file for international migration data and reading the csv file 

temp <- tempfile()
download.file("https://www150.statcan.gc.ca/n1/tbl/csv/17100040-eng.zip", here(temp))
imm_imm <- read_csv(unz(here(temp), "17100040.csv"))
unlink(temp)

# Downloading zip file for natural increase data and reading the csv file 

temp <- tempfile()
download.file("https://www150.statcan.gc.ca/n1/tbl/csv/17100059-eng.zip", here(temp))
nat_inc <- read_csv(unz(here(temp), "17100059.csv"))
unlink(temp)



# Merging the natural increase, inter-provincial migration  international migration datasets
imm <-
  imm_imm %>% 
    clean_names(.) %>% 
    filter(ref_date>="2009-01", components_of_population_growth=="Immigrants"|
               components_of_population_growth=="Net emigration"|
               components_of_population_growth=="Net non-permanent residents") %>% 
    mutate(value=ifelse(components_of_population_growth=="Net emigration",
                        -value, value)) %>% 
    full_join(.,

              nat_inc %>% 
    clean_names(.) %>% 
    filter(ref_date>="2009-01") %>% 
    mutate(value=ifelse(estimates=="Deaths",
                        -value, value),
           )) %>% 
  full_join(.,int_prov_in %>% 
              
              mutate(components_of_population_growth="Net Interprovincial Migration", value=net_intp_imm) %>%
              group_by(ref_date,geo) %>% 
              mutate(moving_sum_imm=sum(moving_sum_imm)) %>% slice(1) %>% ungroup() ) %>% 
    mutate(category=ifelse(!is.na(estimates), estimates, components_of_population_growth),
           datemonth=(ref_date %>% as.yearmon(.) %>% as_date))%>%
  group_by(geo, ref_date) %>%  
  mutate(net_pop_growth= sum(value)) %>% 
  ungroup() %>% 
  
  mutate(pop_grow_type= ifelse(!is.na(estimates), "Natural Population Growth", 
                               ifelse(is.na(estimates)& !is.na(interprovincial_migration ), "Net Interprovincial Migration", 
                                      "Net International Migration")))

imm_in_out <- 
  imm_imm %>% 
  clean_names(.) %>% 
  filter(ref_date>="2009-01", components_of_population_growth=="Non-permanent residents, inflows"|
           components_of_population_growth=="Non-permanent residents, outflows") %>% 
  mutate(value=ifelse(components_of_population_growth=="Non-permanent residents, outflows",
                      -value, value)) %>% 
  group_by(geo,components_of_population_growth) %>% 
  mutate(moving_sum=rollapplyr(value, 4,sum, fill=NA)) %>% 
  ungroup() %>% 
  mutate(date=paste0(substr(ref_date,1,4),"-Q", lubridate::quarter(substr(ref_date,6,7) %>% as.numeric)),
         datemonth=ref_date %>% as.yearmon(.) %>% as_date) 


# Detaching the intermeidate datasets
rm(imm_imm,nat_inc)

# Adding new variables to the immigration dataset
imm <- 
  imm %>% 
   group_by(geo,category) %>% 
    mutate(moving_sum=rollapplyr(net_pop_growth, 4,sum, fill=NA),
                      moving_sum_by_category=rollapplyr(value, 4,sum, fill=NA)) %>% 
  ungroup() %>%  
  group_by(geo, ref_date, pop_grow_type) %>% 
  mutate(
                       moving_sum_net_imm=ifelse(pop_grow_type=="Net International Migration", sum(moving_sum_by_category,na.rm = TRUE),NA),
                       moving_sum_net_nat_pop=ifelse(pop_grow_type=="Natural Population Growth", sum(moving_sum_by_category,na.rm = TRUE),NA)) %>% 
  ungroup()%>% 
  mutate(date=paste0(substr(ref_date,1,4),"-Q", lubridate::quarter(substr(ref_date,6,7) %>% as.numeric))) %>% 
  filter(ref_date>="2010-01") 
  

# Downloading zip file for total population estimates and reading the csv file (to use in the infobox)
temp <- tempfile()
download.file("https://www150.statcan.gc.ca/n1/tbl/csv/17100009-eng.zip", here(temp))
tot_pop <- read_csv(unz((temp), "17100009.csv")) %>% clean_names(.) %>% filter(geo=="British Columbia") %>%
  mutate(datemonth=(ref_date %>% as.yearmon(.) %>% as_date),
         date=paste0(substr(ref_date,1,4),"-Q", lubridate::quarter(substr(ref_date,6,7) %>% as.numeric)))
unlink(temp)



# Downloading zip file for inter provincialmigration with origin-dest data and reading the csv file 
temp <- tempfile()
download.file("https://www150.statcan.gc.ca/n1/tbl/csv/17100045-eng.zip", here(temp))
int_prov <- read_csv(unz((temp), "17100045.csv"))
unlink(temp)


# Data manipulation adding required variables

int_prov<-
  int_prov %>% 
    clean_names(.) %>% 
    filter(ref_date>="2009-01") %>% 
    mutate(geo=gsub("(.*),.*", "\\1", geo),
           geography_province_of_destination=gsub("(.*),.*", "\\1", geography_province_of_destination),
           datemonth=(ref_date %>% as.yearmon(.) %>% as_date)) %>% 
  group_by(geo, geography_province_of_destination) %>% 
    mutate(moving_sum_imm=rollapplyr(value, 4,sum, fill=NA)) %>% 
  ungroup()%>% 
  filter(ref_date>="2010-01") %>% 
  mutate(date=paste0(substr(ref_date,1,4),"-Q", lubridate::quarter(substr(ref_date,6,7) %>% as.numeric)),
         yq=as.yearqtr(ref_date, format = "%Y-%m"))


# Dataset with only international migration

imm_only <- 
  imm %>% filter(is.na(estimates)&is.na(interprovincial_migration)) %>%
  group_by(geo, ref_date) %>% 
  mutate(total_net_imm=sum(value)) %>% 
  ungroup

# Creating lists for selectinput widgets and choices
province=(imm_new$geo)[imm_new$geo!="Canada"]
setNames(as.list(province), province)

province1=(imm_new$geo)[imm_new$non_permanent_resident_types!="Total, non-permanent residents"&
                        imm_new$non_permanent_resident_types!="Total, permit holders and their family members"]
setNames(as.list(province1), province1) 

province2=(imm$geo)
setNames(as.list(province2), province2)

province3=(imm_in_out$geo)
setNames(as.list(province3), province3)

date_inp<- int_prov$yq
setNames(as.list(date_inp), date_inp)

date_inp1<- imm_new_prov_$yq
setNames(as.list(date_inp1), date_inp1)

date_inp2<- imm_new_prov_$yq
setNames(as.list(date_inp2), date_inp2)


date_inp3<- int_prov_in_$yq
setNames(as.list(date_inp3), date_inp3)

date_inp4<- int_prov_in_$yq
setNames(as.list(date_inp4), date_inp4)

permit_type=(imm_new$non_permanent_resident_types)[imm_new$geo!="Canada"]
setNames(as.list(permit_type), permit_type)

permit_type1=(imm_new$non_permanent_resident_types)[imm_new$non_permanent_resident_types!="Total, non-permanent residents"&
                                                    imm_new$non_permanent_resident_types!="Total, permit holders and their family members"]
setNames(as.list(permit_type1), permit_type1)

permit_type2=(imm_new$non_permanent_resident_types)
setNames(as.list(permit_type2), permit_type2)

permit_type3=(imm_new_prov_$non_permanent_resident_types)
setNames(as.list(permit_type3), permit_type3)


type=(imm$components_of_population_growth)
setNames(as.list(type), type)


category1=(imm_only$category)
setNames(as.list(category1), category1)

category2=(imm$category)
setNames(as.list(category2), category2)

category3=(imm_in_out$components_of_population_growth)
setNames(as.list(category3), category3)

location=(imm$geo)
setNames(as.list(location), location)

location2=(imm_only$geo)
setNames(as.list(location2), location2)

location3=(imm$geo)
setNames(as.list(location3), location3)


pop_grow_type1=(imm$pop_grow_type)
setNames(as.list(pop_grow_type1), pop_grow_type1)

dateq=(imm$datemonth)
setNames(as.list(dateq), dateq)

dateq2=(imm_only$datemonth)
setNames(as.list(dateq2), dateq2)

dateq3=(imm$datemonth)
setNames(as.list(dateq3), dateq3)

date1<- imm_new$datemonth[imm_new$non_permanent_resident_types!="Total, non-permanent residents"&
                             imm_new$non_permanent_resident_types!="Total, permit holders and their family members"]
setNames(as.list(date1), date1)

date2<- imm_new$datemonth
setNames(as.list(date2), date2)

date3<- imm_new$datemonth[imm_new$geo!="Canada"]
setNames(as.list(date3), date3)

date4<- imm_in_out$datemonth
setNames(as.list(date2), date2)

orig=(int_prov$geo)
setNames(as.list(orig), orig)

dest=(int_prov$geography_province_of_destination)
setNames(as.list(dest), dest)


orig1=(int_prov$geo)
setNames(as.list(orig1), orig1)

dest1=(int_prov$geography_province_of_destination)
setNames(as.list(dest1), dest1)

# Deleting intermediate datasets
rm(int_prov_in_,imm_new_prov_)


# Define sidebar item names
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Summary", tabName = "summary", icon = icon("list-alt")),
    # menuItem("NPR by province",  tabName = "NPR",icon = icon("bar-chart")),
    menuItem("Non-permanent residents", tabName = "NPRs_permit_type",icon = icon("line-chart")),
    menuItem("International migration", tabName = "International_immigration",icon = icon("bar-chart")),
    menuItem("Inter-Provincial migration", tabName = "Inter_Provincial_migration", icon = icon("table")),

    menuItem("Population Change Quarterly", tabName = "Population_Change_Quarterly",icon = icon("bar-chart"))
    

  )
)

# Function Minus for substracting one year's data point from the next one
minus <- function(x) {
  for (i in 1:length(x)-1)
    min<- x[i] - x[i+1]
  print(min)
}


# defining function for multiple linebreaks
linebreaks <- function(n){HTML(strrep(br(), n))}

# UI contents by tab item
body <- dashboardBody(    
  tabItems(
      tabItem( tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    tabName="summary",fluidPage(style="background-color:ghostwhite;",
             column(9,titlePanel( 
               h3("This dashboard is produced by LMIRE Branch of Ministry of Post-Secondary Education and Future Skills",
                  align="center"))),
               column(9,titlePanel( 
               h1("B.C. Quarterly Migration Update, March 2024", align="center"))),
            # sidebarLayout(
            #    img(src = "LMIO LOGO2.jpg", height = 140, width = 140, align="left"),
            #    
            #  
             mainPanel(width = 9,
              uiOutput("nprbox"),
              uiOutput("nprpercbox"), 
              uiOutput("intmig"),
              uiOutput("intmigperc"),
              uiOutput("natgrow"),
              uiOutput("natgrowperc"),
              uiOutput("intp"),
              uiOutput("intperc"),
              uiOutput("popgrow"),
              uiOutput("popgrowperc"),
              linebreaks(15),
              htmltools::HTML(" <br> Notes: <br> 4 Quarter Moving Sum represents the sum of values over the latest 4 quarters.<br>
                                
                                <br> Net International migration = International migration+ Net emigration +Net non-permanent residents
                                <br> Natural population growth = Births - Deaths
                                <br>Population growth = Births-Deaths + International migration + Net Emigration + Net Interprovincial Migration +
                                Non-Permanent Residents
                                <br> Population growth rate = Population growth/Population x 100
                               
                                <br>
                                <br> Data sources used in this dashboard are the following Statistics Canada datsets: 
                                
                                Estimates of the components of international migration, quarterly; 
                                Estimates of the number of non-permanent residents by type, quarterly; 
                                Quarterly Demographic Estimates;
                                Estimates of the components of interprovincial migration, quarterly;
                                Estimates of interprovincial migrants by province or territory of origin and destination, quarterly;
                                Estimates of the components of natural increase, quarterly

                              "
                            
                          
                              ),
             
            ))),
  
    
    tabItem(tabName="NPRs_permit_type",
            tabsetPanel(
            tabPanel("NPRs by Permit Type", fluidPage(style="background-color:ghostwhite;",
                                     
             sidebarPanel(
               h6("\nPlease note that if the category 'Total asylum claimants' are selected asylum claimant sub-categories will not appear in the choices"),
               selectizeInput("permit_type1", label="Permit Type", multiple=TRUE, choices =  unique(permit_type1), selected = "Work permit holders only"),
               selectInput("province1", " Province", choices=unique(province1), selected="British Columbia"),
               
                 sliderInput(label = "Date",inputId = "date1",
                             min = min(imm_new$datemonth),
                             max = max(imm_new$datemonth),
                             value=c(min(imm_new$datemonth), max(imm_new$datemonth)),
                             timeFormat="%Y-%m",ticks = TRUE)),

             mainPanel(fluidRow(
               downloadButton("download2", "Download the data"),
               
            column(12, plotlyOutput("distPlot5a", inline = TRUE)),
            column(12, plotlyOutput("distPlot5", inline=TRUE)),
            column(12, h4("This dashboard is produced by LMIRE Branch of Ministry of Post-Secondary Education and Future Skills")),
            column(12, h5("Data source: Statistics Canada. Table 17-10-0121-01  Estimates of the number of non-permanent residents by type, quarterly")), 
            column(12, h5("If you have any questions regarding the dashboard 
                          please reach out to Asli Gurer McNeilage (asli.gurermcneilage@gov.bc.ca) or Erhan Baydar (erhan.baydar@gov.bc.ca)"))     
             )))),
              tabPanel("NPRs by Location",fluidPage(style="background-color:ghostwhite;",
                       sidebarPanel(
                                     
                                     sliderInput(label = "Date",inputId = "date3",
                                                 min = min(imm_new$datemonth),
                                                 max = max(imm_new$datemonth),
                                                 value=c(min(imm_new$datemonth), max(imm_new$datemonth)),
                                                 timeFormat="%Y-%m",ticks = TRUE),
                                    
                                     selectInput("province", "Province", choices=unique(province),  multiple = TRUE, selected="British Columbia"),
                                     selectInput("permit_type", "Permit type",choices=unique(permit_type), selected="Total, non-permanent residents")
                       ),
                       
                       
                       mainPanel(fluidRow(
                         downloadButton("download1", "Download the data"),
                        
                         column(12, plotlyOutput("distPlot", inline=TRUE)),
                         column(12, plotlyOutput("distPlot_a", inline=TRUE)),
                         column(12,  h4("This dashboard is produced by LMIRE Branch of Ministry of Post-Secondary Education and Future Skills")),
                         column(12, h5("Data source: Statistics Canada. Table 17-10-0121-01  Estimates of the number of non-permanent residents by type, quarterly")), 
                         column(12, h5("If you have any questions regarding the dashboard 
                          please reach out to Asli Gurer McNeilage (asli.gurermcneilage@gov.bc.ca) or Erhan Baydar (erhan.baydar@gov.bc.ca)"))    
                         
                         )))),
            
            tabPanel("Map",
                     sidebarPanel(  

                                   selectInput("date_inp2", "Date",
                                              choices=NULL),
                                   selectInput("permit_type3", "Permit type",choices=unique(permit_type3), selected="Total, non-permanent residents")
                     ),
                     
                     
                     mainPanel(fluidRow(
                      
                       column(12, leafletOutput("mymap")),
                       column(12, h4("This dashboard is produced by LMIRE Branch of Ministry of Post-Secondary Education and Future Skills")),
                       column(12, h5("Data source: Statistics Canada. Table 17-10-0121-01  Estimates of the number of non-permanent residents by type, quarterly")), 
                       column(12, h5("If you have any questions regarding the dashboard 
                          please reach out to Asli Gurer McNeilage (asli.gurermcneilage@gov.bc.ca) or Erhan Baydar (erhan.baydar@gov.bc.ca)"))   
                     )))
            ))
    ,
    tabItem( tabName="International_immigration", fluid=TRUE,
             tabsetPanel(
               tabPanel("International migration", fluidPage(style="background-color:ghostwhite;",
                                 
                            sidebarPanel(

                                selectInput("category1", "Immigration category",
                                            choices=NULL, selected="Immigrants", multiple=TRUE),
                                selectInput("location2", " Location", choices=unique(location2), selected="British Columbia"),
                                selectInput("Stat", "Statistics", c("4 Quarter Moving Sum", "Quarterly value"), selected = "4 Quarter Moving Sum"),
                                sliderInput(label = "Date",inputId = "dateq2",
                                            min = min(imm$datemonth),
                                            max = max(imm$datemonth),
                                            value=c(min(imm$datemonth), max(imm$datemonth)),
                                            timeFormat="%Y-%m",ticks = TRUE)
                            ),

                            mainPanel(fluidRow(
                              downloadButton("download4", "Download the data"),
                              
                                column(12, plotlyOutput("distPlot3", inline = TRUE)),
                                column(12, plotlyOutput("distPlot3a", inline=TRUE))),
                                column(12,  h4("This dashboard is produced by LMIRE Branch of Ministry of Post-Secondary Education and Future Skills")),
                                column(12, h5("Data source: Statistics Canada. Table 17-10-0040-01  Estimates of the components of international migration, quarterly")), 
                                column(12, h5("If you have any questions regarding the dashboard 
                          please reach out to Asli Gurer McNeilage (asli.gurermcneilage@gov.bc.ca) or Erhan Baydar (erhan.baydar@gov.bc.ca)"))   
                                ))),
                        tabPanel("NPR inflow and outflow",
                                 sidebarPanel(
                                   downloadButton("download3b", "Download the data"),
                                   
                                   selectInput("category3", "Immigration category",
                                               choices=NULL, selected="Immigrants", multiple=TRUE),
                                   selectInput("province3", " Location", choices=unique(province3), selected="British Columbia"),
                                   selectInput("Stat_b", "Statistics", c("4 Quarter Moving Sum", "Quarterly value"), selected = "4 Quarter Moving Sum"),
                                   sliderInput(label = "Date",inputId = "date4",
                                               min = as_date("2021-07-01"),
                                               max = max(imm_in_out$datemonth),
                                               value=c(as_date("2021-07-01"), max(imm_in_out$datemonth)),
                                               timeFormat="%Y-%m",ticks = TRUE)
                                 ),
                                 
                                 mainPanel(fluidRow(
                                   column(12, plotlyOutput("distPlot3b", inline = TRUE)),
                                   column(12,  h4("This dashboard is produced by LMIRE Branch of Ministry of Post-Secondary Education and Future Skills")),
                                   column(12, h5("Data source: Statistics Canada. Table 17-10-0040-01  Estimates of the components of international migration, quarterly")), 
                                   column(12, h5("If you have any questions regarding the dashboard 
                          please reach out to Asli Gurer McNeilage (asli.gurermcneilage@gov.bc.ca) or Erhan Baydar (erhan.baydar@gov.bc.ca)"))  
                                   )))
                            )),
    
    tabItem(tabName="Population_Change_Quarterly", fluidPage(style="background-color:ghostwhite;",
            
            tabsetPanel(
              tabPanel("Graphs", fluidPage(style="background-color:ghostwhite;",
            sidebarPanel(
              selectizeInput("category2", "Type of population change",
                          choices = NULL,  multiple=TRUE),
              selectInput("location3", " Location", choices=NULL),
              selectInput("Stat2", "Statistics", c("4 Quarter Moving Sum", "Quarterly value"), selected = "4 Quarter Moving Sum"),
              sliderInput(label = "Date",inputId = "dateq3",
                          min = min(imm$datemonth),
                          max = max(imm$datemonth),
                          value=c(min(imm$datemonth), max(imm$datemonth)),
                          timeFormat="%Y-%m",ticks = TRUE)
            ),
            mainPanel(fluidRow(style="background-color:ghostwhite;",
                               downloadButton('download4a',"Download the data"),
              column(12, plotlyOutput("distPlot3_1", inline = TRUE)),
              column(12, plotlyOutput("distPlot_sm", inline = TRUE)),
              column(12, plotlyOutput("distPlot3a_1", inline = TRUE)),
              column(12,  h4("This dashboard is produced by LMIRE Branch of Ministry of Post-Secondary Education and Future Skills")),
              column(12, h5("Data sources: Estimates of the components of international migration, quarterly; 
                                Estimates of the components of interprovincial migration, quarterly;
                                Estimates of the components of natural increase, quarterly")), 
              column(12, h5("If you have any questions regarding the dashboard 
                          please reach out to Asli Gurer McNeilage (asli.gurermcneilage@gov.bc.ca) or Erhan Baydar (erhan.baydar@gov.bc.ca)"))  
              ))
              
            )),
            tabPanel("Table", 
                     sidebarPanel(
                       selectInput("province2", " Location", choices=unique(province2), selected="British Columbia"),
                       selectInput("pop_grow_type1", " Population change type", choices=unique(pop_grow_type1))
                       ), 
                       mainPanel(fluidRow(
                         downloadButton('download',"Download the data"),
                         
                         column(12, dataTableOutput("mytable2"), style='margin-bottom:30px;border:1px solid; padding: 10px;'),
                         column(12,  h4("This dashboard is produced by LMIRE Branch of Ministry of Post-Secondary Education and Future Skills")),
                         column(12, h5("Data source: Statistics Canada. Table 17-10-0045-01 Estimates of interprovincial migrants by province or territory of origin and destination, quarterly")), 
                         column(12, h5("If you have any questions regarding the dashboard 
                          please reach out to Asli Gurer McNeilage (asli.gurermcneilage@gov.bc.ca) or Erhan Baydar (erhan.baydar@gov.bc.ca)"))
                         
                         
                     
                     
                     )))))),
            
    tabItem(tabName = "Inter_Provincial_migration", 
            tabsetPanel(
      tabPanel("Origin and Destination table",
                            sidebarPanel(

                                selectInput("date_inp", "Inter-provincial migration",
                                            choices=NULL),
                                selectInput("Stat3", "Statistics", c("4 Quarter Moving Sum", "Quarterly value"))
                                

                            ),

                            mainPanel(fluidRow(
                              downloadButton("download5", "Download the data"),
                              
                                column(12, plotOutput("distPlot4")),
                                column(12,  h4("This dashboard is produced by LMIRE Branch of Ministry of Post-Secondary Education and Future Skills")),
                                column(12, h5(" Statistics Canada. Table 17-10-0045-01  Estimates of interprovincial migrants by province or territory of origin and destination, quarterly")),
                                column(12, h5("Data sources: Estimates of the components of international migration, quarterly; 
                                Estimates of the components of interprovincial migration, quarterly;
                                Estimates of the components of natural increase, quarterly")), 
                                column(12, h5("If you have any questions regarding the dashboard 
                          please reach out to Asli Gurer McNeilage (asli.gurermcneilage@gov.bc.ca) or Erhan Baydar (erhan.baydar@gov.bc.ca)"))  
                            ))),
            tabPanel("Two Province Comparison",  fluidPage(style="background-color:ghostwhite;",
                     
            sidebarPanel(
              linebreaks(3),
              h5("Date selection for the 2 province line graph"),
              sliderInput(label = "Date",inputId = "date2",
                          min = min(int_prov$datemonth),
                          max = max(int_prov$datemonth),
                          value=c(min(int_prov$datemonth), max(int_prov$datemonth)),
                          timeFormat="%Y-%m",ticks = TRUE),
              linebreaks(4),
             hr(),
             
              selectInput("Stat4", "Statistics", c("4 Quarter Moving Sum", "Quarterly value")),
              selectizeInput ("orig", " Province of origin: Please select 2 provinces", choices=unique(orig),multiple = TRUE, selected=c("British Columbia", "Alberta"),
                              options = list(maxItems = 2)),
              selectizeInput ("dest", " Province of destination", choices=unique(dest),multiple = TRUE, 
                              options = list(maxItems = 2)),
             hr(),
              linebreaks(2),
             h5("Date selection for the table"),
              
              selectInput("date_inp4", "Inter-provincial migration",
                          choices=unique(date_inp4), selected="2023 Q3")
                          ),
                          

                            mainPanel(
                              downloadButton("download5a", "Download the data"),
                              
                                column(12, plotlyOutput("distPlot6")),
                                linebreaks(12),
                                
                                h4(hr(),linebreaks(12),column(11,tableOutput("mytable"), style='margin-bottom:30px;border:1px solid; padding: 10px;',
                                                              offset = 1)),
                                column(12,  h4("This dashboard is produced by LMIRE Branch of Ministry of Post-Secondary Education and Future Skills")),
                                column(12, h5("Data source: Statistics Canada. Table 17-10-0045-01  Estimates of interprovincial migrants by province or territory of origin and destination, quarterly")), 
                                column(12, h5("If you have any questions regarding the dashboard 
                          please reach out to Asli Gurer McNeilage (asli.gurermcneilage@gov.bc.ca) or Erhan Baydar (erhan.baydar@gov.bc.ca)"))  
                            ))),
      tabPanel("Map", 
               sidebarPanel(  
                 
                 selectInput("date_inp3", "Date",
                             choices=NULL),
                 
                 selectInput("Stat5", "Statistics", c("4 Quarter Moving Sum", "Quarterly value"))
               ),
               
               
               mainPanel(fluidRow(

                 column(12, h3("Net-interprovincial Migration"),offset = 3),
                 column(12, leafletOutput("mymap2")),
                 column(12, h4("This dashboard is produced by LMIRE Branch of Ministry of Post-Secondary Education and Future Skills")),
                 column(12, h5("Data source: Statistics Canada. Table 17-10-0020-01  Estimates of the components of interprovincial migration, quarterly")), 
                 
                 column(12, h5("If you have any questions regarding the dashboard 
                          please reach out to Asli Gurer McNeilage (asli.gurermcneilage@gov.bc.ca) or Erhan Baydar (erhan.baydar@gov.bc.ca)"))   
                 ))))
            
            )
            
            
    ))
  

# Define UI combining sidebar and body
ui <-fluidPage( 
    
dashboardPage(
  dashboardHeader(title = ""),
  sidebar,
  body
)
)

# Defining the server
server <- shinyServer(function(input, output, session) {
    
    #Reactive datasets 
    dataset1 <- reactive({
        req(input$date3, input$province, input$permit_type)
        imm_new %>%
          filter(geo!="Canada") %>% 
            dplyr::filter(datemonth <= input$date3[2] & datemonth >= input$date3[1],
                          province%in%input$province, permit_type %in% input$permit_type)
    })
    
    output$download1 <- downloadHandler(
      filename = paste0("NPRs by Location", "_", input$permit_type,  ".csv"),
      content = function(file) {
        readr::write_csv((dataset1() %>% dplyr::select(-total_imm_cat, -percantage_cat)), file)
      }
    )
    dataset2 <- reactive({
        subset(imm_new %>%  
                 filter(non_permanent_resident_types!="Total, non-permanent residents", 
                        non_permanent_resident_types!="Total, permit holders and their family members"), 
               province1 %in%input$province1 & non_permanent_resident_types %in% input$permit_type1&
               datemonth <= input$date1[2] & datemonth >= input$date1[1])
    })

    output$download2 <- downloadHandler(
      filename = paste0("NPRs by Permit type", "_", input$province, ".csv"),
      content = function(file) {
        readr::write_csv((dataset2()%>% dplyr::select(-total_imm_canada)), file)
      }
    )
    
    
    dataset4 <- reactive({
        subset( imm_only, dateq2 <= input$dateq2[2] & dateq2 >= input$dateq2[1]&
                   location2 %in% input$location2 & category1 %in% input$category1)
    })
    
    
    output$download4  <- downloadHandler(
      filename = paste0("Immigration", "_", input$location2, ".csv"),
      content = function(file) {
        readr::write_csv(dataset4() %>% dplyr::select(-moving_sum, -moving_sum_net_nat_pop), file)
      }
    )
    
    
    
    # dataset4_1 <- reactive({
    #   subset( imm_only , dateq2 <= input$dateq2[2] & dateq2 >= input$dateq2[1]&
    #             location2 %in% input$location2 )
    # })
    
    # output$download4_1  <- downloadHandler(
    #   filename = paste0("Immigration", "_", input$location2, ".csv"),
    #   content = function(file) {
    #     readr::write_csv(dataset4_1() %>% dplyr::select(-moving_sum, -moving_sum_net_nat_pop), file)
    #   }
    # )
    dataset4a <- reactive({
      # req(input$dateq3, input$location3, input$category2)
      imm %>%
        dplyr::filter(datemonth <= input$dateq3[2] & datemonth >= input$dateq3[1],
                      location3%in%input$location3, category2 %in% input$category2)
    })
    
    output$download4a  <- downloadHandler(
      filename = paste0("Population change", "_", input$location3, ".csv"),
      content = function(file) {
        readr::write_csv(dataset4a() %>%  dplyr::select(-moving_sum_net_imm,- moving_sum_net_nat_pop), file)
      }
    )
    
    dataset3b <- reactive({
      imm_in_out %>%
        dplyr::filter(datemonth <= input$date4[2] & datemonth >= input$date4[1],
                      geo%in%input$province3, category3 %in% input$category3)
    })
    
    output$download3b  <- downloadHandler(
      filename = paste0("NPR-inflow&outflow", "_", input$province3, ".csv"),
      content = function(file) {
        readr::write_csv(dataset3b(), file)
      }
    )
    
    dataset5 <- reactive({
        subset(int_prov, yq %in% input$date_inp )
    })
    
    output$download5  <- downloadHandler(
      filename = paste0("Interprov_origin_dest", "_", input$date_inp, ".csv"),
      content = function(file) {
        readr::write_csv(dataset5()%>%  dplyr::select(-yq), file)
      }
    )
    
    dataset5a <- reactive({
      req(input$date2, input$orig, input$dest)
      subset(int_prov, datemonth <= input$date2[2] & datemonth >= input$date2[1]&
             geo %in% input$orig & geography_province_of_destination %in% input$dest)
    })
    
    output$download5a  <- downloadHandler(
      filename = paste0("Interprov", "_", input$orig[1],"_",input$orig[2] , ".csv"),
      content = function(file) {
        readr::write_csv(dataset5a() %>%  dplyr::select(-yq), file)
      }
    )
    
    dataset6 <- reactive({
      subset(imm_new_prov, yq %in% input$date_inp2&
             non_permanent_resident_types %in% input$permit_type3, 
             ) 
    })
    
 
    
    dataset7 <- reactive({
      subset(int_prov_map_data,yq %in% input$date_inp3)  
    })
    
    
    dataset8 <- 
      reactive({

     subset(imm %>% filter(str_detect(ref_date, "-10")) %>% 
              group_by(geo, category) %>% 
              mutate(difference_imm = moving_sum_net_imm-lag(moving_sum_net_imm,n=1),
                     difference_net_inp =(moving_sum_imm1)-lag(moving_sum_imm1,n=1),
                     difference_nat_pop =(moving_sum_net_nat_pop)-lag(moving_sum_net_nat_pop,n=1),
                     difference=ifelse(!is.na(difference_net_inp), difference_net_inp, 
                                       ifelse(!is.na(difference_nat_pop), difference_nat_pop, difference_imm)),
                     difference=formatC(round(difference), format="d", big.mark=",")) %>% 
              ungroup() , 
            geo %in% input$province2 & pop_grow_type %in%input$pop_grow_type1)%>% 
          group_by(pop_grow_type, ref_date) %>%
          slice(1) %>% 
          ungroup() %>% 
          mutate(ref_date=substr(ref_date, 1,4)) %>% 
          mutate(imm_mov_sum_broad_cat= ifelse(!is.na(moving_sum_net_nat_pop),
                                                      moving_sum_net_nat_pop, 
                                               ifelse(!is.na(moving_sum_imm1),
                                                      moving_sum_imm1, moving_sum_net_imm))) %>% 
        dplyr::select(ref_date, imm_mov_sum_broad_cat, difference) %>% 
          
            filter(ref_date!="2010") %>%
            mutate(
                   imm_mov_sum_broad_cat= formatC(imm_mov_sum_broad_cat, format="d", big.mark=",")) %>% 
          rename("Year"=ref_date, "Year Total"=imm_mov_sum_broad_cat, "Year to Year Value Change"= difference)
     
    })
    


    
    observe({ updateSelectizeInput(session, "category1", "Immigration category",
                                   choices=category1, selected="Immigrants",
                                   server=TRUE)
    })
    
    
    observe({ updateSelectizeInput(session, "category2", 
                                   choices=unique(category2), selected="Immigrants",
                                   server=TRUE)
    })
    
    
    observe({ updateSelectizeInput(session, "category3", 
                                   choices=unique(category3), selected=c("Non-permanent residents, outflows","Non-permanent residents, inflows"),
                                   server=TRUE)
    })
    
    observe({ updateSelectInput(session, "date_inp3", "Date:",
                                choices=unique(date_inp3), selected=max(date_inp3))
    })
    observe({ updateSelectInput(session, "date_inp2", "Date:",
                                choices=unique(date_inp2), selected=max(date_inp2))
    })
    
    observe({ updateSelectInput(session, "date_inp", "Date:",
                                choices=unique(date_inp), selected=max(date_inp))
    })
    
    observe({ updateSelectInput(session, "location3", "Location:",
                                choices=unique(location3), selected="British Columbia")
    })
    
    observe({ updateSelectInput(session, "province3", "Location:",
                                choices=unique(province3), selected="British Columbia")
    })
    
    observe({
      
      updateSelectInput(session,'dest',
                        choices = if(length(input$orig)==1){
                          ""
                        }else{ 
                          as.list(input$orig)},
                        selected = input$orig)
      
      
      
    })   
    

    
    
    observe({ 
      if (any(input$permit_type1 %in% c("Total, asylum claimants"))) {
        updateSelectizeInput(session, "permit_type1",
                       choices = permit_type1[!(str_detect(permit_type1,"Asylum claimants with"))],
                       selected =input$permit_type1, server = FALSE)
      } else {
        updateSelectizeInput(session, "permit_type1", 
                       choices = unique(permit_type1), selected =input$permit_type1 ,server = FALSE)  
      }  
     
    })
    
    #Defining tables, plots and maps 
    
   table <- 
     reactive({
       subset(int_prov, yq %in% input$date_inp4&
                geo %in% input$orig & geography_province_of_destination %in% input$dest) %>% 
         dplyr::select(geo,geography_province_of_destination,value, moving_sum_imm)
     })     
     
   
      output$mymap<- renderLeaflet({
        
          
          pal <- colorNumeric(
            palette = c("lightgreen", "darkgreen"),
            domain = dataset6()$value)
          
          map1 = 
            leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
            addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
            setView( lat = 62.1304  , lng = -98.3468 , zoom =2.5 )%>%
            addPolygons(data=dataset6(), weight = 1,
                        stroke = TRUE,
                        
                        fillColor = ~pal(value),
                        fillOpacity = 0.85,
                        dashArray = "0.5",
                        label = ~paste(NAME_1,
                                       "<br/>",
                                       "Number of individuals:",  formatC(value, format="d", big.mark=",")) %>% 
                          lapply(htmltools::HTML),
                        highlight = highlightOptions(
                          weight = 2,
                          dashArray = "",
                          color = "black",
                          bringToFront = TRUE
                        ))
          print(map1)
})
    
      
      
      output$mymap2<- renderLeaflet({
        
  
       
        
        if(input$Stat5=="4 Quarter Moving Sum"){  
          
          minVal <- min(dataset7()$moving_sum_imm1)
          maxVal <- max(dataset7()$moving_sum_imm1)
          domain <- c(minVal,maxVal)
          colorPal <- c(colorRampPalette(colors = c("#b2182b", "white"), space = "Lab")(abs(minVal)),
                        colorRampPalette(colors = c("white", "#2166ac"), space = "Lab")(maxVal))
          
        map2 = 
          leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
          addProviderTiles(providers$CartoDB.PositronNoLabels) %>% setView( lat = 62.1304  , lng = -98.3468 , zoom =2.5 )%>%
          addPolygons(data=dataset7(), weight = 1,
                      stroke = TRUE,
                      
                      fillColor = ~get('colorBin')(colorPal, 
                                                   domain)(moving_sum_imm1),
                      fillOpacity = 0.85,
                      dashArray = "0.5",
                      label = ~paste(NAME_1,
                                     "<br/>",
                                     "Net interprovincial migration:",  formatC(moving_sum_imm1, format="d", big.mark=","))%>% lapply(htmltools::HTML),
                      highlight = highlightOptions(
                        weight = 2,
                        dashArray = "",
                        color = "black",
                        bringToFront = TRUE
                      ))}
        
        if(input$Stat5=="Quarterly value"){  
          
          minVal <- min(dataset7()$net_intp_imm)
          maxVal <- max(dataset7()$net_intp_imm)
          domain <- c(minVal,maxVal)
          colorPal <- c(colorRampPalette(colors = c("#b2182b", "white"), space = "Lab")(abs(minVal)),
                        colorRampPalette(colors = c("white", "#2166ac"), space = "Lab")(maxVal))
          
          map2 = 
            leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
            addProviderTiles(providers$CartoDB.PositronNoLabels) %>% setView( lat = 62.1304  , lng = -98.3468 , zoom =2.5 )%>%
            addPolygons(data=dataset7(), weight = 1,
                        stroke = TRUE,
                        
                        fillColor = ~get('colorBin')(colorPal, 
                                                     domain)(net_intp_imm),
                        fillOpacity = 0.85,
                        dashArray = "0.5",
                        label = ~paste(NAME_1,
                                       "<br/>",
                                       "Net interprovincial migration:",  formatC(net_intp_imm, format="d", big.mark=","))%>% lapply(htmltools::HTML),
                        highlight = highlightOptions(
                          weight = 2,
                          dashArray = "",
                          color = "black",
                          bringToFront = TRUE
                        ))}
        
        print(map2)
        
      })      
      
    output$distPlot <-renderPlotly({
    
        
        p=ggplot(dataset1(), aes(x = date, y = value, group=geo, fill=geo,
                                 text=paste("Date:", date, "\nValue:", formatC(value, format="d", big.mark=","), "\nLocation:", geo,
                                            "\nPercentage in total NPRS: %", percentage_prov))) +
          geom_bar(stat="identity", position="stack")+
          theme_minimal()+
          # scale_fill_manual(values=palette_new)+
          labs(title="NPRs by Province", y="Number of Individuals", fill="Province")+
          scale_y_continuous(labels = scales::comma)+
          
          theme(
            plot.title = element_text(size=18, face="bold",hjust = 0.5),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size=9,angle=90),
            axis.title.y = element_text(size=12)
            
         )+
          scale_fill_brewer(palette = "Dark2")


        
        ggplotly(p, tooltip = c("text"))%>% plotly::layout(
                                                           margin = list(b = 120, l = 100, r = 100, t = 80),
                                                           annotations = list(x = 1, y = -0.4, text = paste(""),
                                                                              showarrow = F, 
                                                                              xref = "paper", yref = "paper",
                                                                              xanchor = "right", 
                                                                              yanchor = "auto", xshift = -100, yshift = 100, font = list(size = 10))) %>% 
          plotly::config(toImageButtonOptions = list(format = "svg", 
                                                     width = 960, height = 720))
        
        
    })
 
  
    
    output$distPlot_a <-renderPlotly({
      
      
      p=ggplot(dataset1(), aes(x = date, y = value, group=geo, color=geo,
                               text=paste("Date:", date, "\nValue:", formatC(value, format="d", big.mark=","), "\nLocation:", geo))) +
        geom_line()+
        theme_minimal()+
        
        # scale_fill_manual(values=palette_new)+
        labs(title="NPRs by Province", y="Number of Individuals", color="Province")+
        scale_y_continuous(labels = scales::comma)+
        
        theme(
          plot.title = element_text(size=18, face="bold",hjust = 0.5),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size=9,angle=90),
          axis.title.y = element_text(size=12)
          
        )+
        scale_color_brewer(palette = "Dark2")
      
      
      ggplotly(p, tooltip = c("text")) %>% plotly::layout(
                                                          margin = list(b = 120, l = 100, r = 100, t = 80),
                                                          annotations = list(x = 1, y = -0.4, text = paste(""),
                                                                             showarrow = F, 
                                                                             xref = "paper", yref = "paper",
                                                                             xanchor = "right", 
                                                                             yanchor = "auto", xshift = -100, yshift = 100, font = list(size = 10))) %>% 
        plotly::config(toImageButtonOptions = list(format = "svg", 
                                                   width = 960, height = 720))
      
    })
    
    output$distPlot5a <-renderPlotly({ 
      validate(
        need(input$permit_type1, "Please select a permit type")
      )
      bl=ggplot(dataset2(), 
               aes(x = date, y = value, 
                   group=non_permanent_resident_types, 
                   color=non_permanent_resident_types,
                   text=paste("Date:", date, "\nValue:", formatC(value, format="d", big.mark=","), "\nPermit Type:", non_permanent_resident_types))) +
        geom_line()+
        theme_minimal()+
        labs(title="NPRs by Permit Type", y="Number of Individuals", color="NPR Type")+
        scale_y_continuous(labels = scales::comma)+
        theme(
          plot.title = element_text(size=18, face="bold",hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=12),
          axis.text.x = element_text(size=7,angle=90),
          legend.text = element_text(size=8))+
        scale_color_brewer(palette = "Dark2")
        
     
        ggplotly(bl, tooltip = c("text"))%>% plotly::layout(
                                                            margin = list(b = 120, l = 100, r = 100, t = 80),
                                                            annotations = list(x = 1, y = -0.4, text = paste(""),
                                                                               showarrow = F, 
                                                                               xref = "paper", yref = "paper",
                                                                               xanchor = "right", 
                                                                               yanchor = "auto", xshift = -100, yshift = 100, font = list(size = 10))) %>% 
          plotly::config(toImageButtonOptions = list(format = "svg", 
                                                     width = 960, height = 720))
      
    })

    output$distPlot5 <-renderPlotly({ 
      
       b=ggplot(dataset2(), 
                 aes(x = date, y = value, 
                     group=non_permanent_resident_types, 
                     fill=non_permanent_resident_types,
                     text=paste("Date:", date, "\nValue:", formatC(value, format="d", big.mark=","), "\nPermit Type:", non_permanent_resident_types,
                                "\nPercentage in total NPRS: %", percentage_cat ))) +
         
          geom_bar(stat="identity")+
        
         theme_minimal()+
         labs(title="NPRs by Permit Type", y="Number of Individuals", fill="NPR Type")+
         scale_y_continuous(labels = scales::comma)+
         
         theme(
           plot.title = element_text(size=18, face="bold",hjust = 0.5),
           axis.title.x = element_blank(),
           axis.title.y = element_text(size=12),
           axis.text.x = element_text(size=7,angle=90),
           legend.text = element_text(size=8))+
         scale_fill_brewer(palette = "Dark2")
       
       ggplotly(b, tooltip = c("text"))%>% plotly::layout(
                                                          margin = list(b = 120, l = 100, r = 100, t = 80),
                                                          annotations = list(x = 1, y = -0.4, text = paste(""),
                                                                             showarrow = F, 
                                                                             xref = "paper", yref = "paper",
                                                                             xanchor = "right", 
                                                                             yanchor = "auto", xshift = -100, yshift = 100, font = list(size = 10))) %>% 
         plotly::config(toImageButtonOptions = list(format = "svg", 
                                                    width = 960, height = 720))
       
       
    })
   
    

    observe({ output$distPlot3 <-renderPlotly({ 
      validate(
        need(input$category1, "Please select a category")
      )
        if(input$Stat=="4 Quarter Moving Sum") {
               h=ggplot(dataset4(), 
                   aes(x=date, y=moving_sum_by_category, group=category, fill=category, 
                   text=paste("Date:", date, "\nCategory:", category, "\nMoving Sum:", formatC(moving_sum_by_category, format="d", big.mark=",")))) + 
            geom_bar(stat="identity")+
               
                 theme_minimal()+
                 scale_y_continuous(labels = scales::comma)+
                 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.3,size = 6))+
                 labs(title="International migration", y="Number of Individuals", fill="Type of immigration")+
                 theme(
                   plot.title = element_text(size=18, face="bold",hjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.title.y = element_text(size=12),
                   axis.text.x = element_text(size=7,angle=90),
                   legend.text = element_text(size=8))+
                 scale_fill_brewer(palette = "Dark2")
               }
        if(input$Stat=="Quarterly value") {
              h=ggplot(dataset4(), 
                      aes(x=date, y=value, group=category, fill=category,
                          text=paste("Date:", date, "\nCategory:", category, "\nValue:", formatC(value, format="d", big.mark=",")))) + 
               geom_bar(stat="identity")+
                theme_minimal()+
                scale_y_continuous(labels = scales::comma)+
                
                
               theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.3,size = 6))+
                labs(title="International migration", y="Number of Individuals", fill="Type of immigration")+
                theme(
                  plot.title = element_text(size=18, face="bold",hjust = 0.5),
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(size=12),
                  axis.text.x = element_text(size=7,angle=90),
                  legend.text = element_text(size=8))+
                scale_fill_brewer(palette = "Dark2")} 
        
      ggplotly(h, tooltip = c("text"))%>% plotly::layout(
                                                         margin = list(b = 120, l = 100, r = 100, t = 80),
                                                         annotations = list(x = 1, y = -0.4, text = paste(""),
                                                                            showarrow = F, 
                                                                            xref = "paper", yref = "paper",
                                                                            xanchor = "right", 
                                                                            yanchor = "auto", xshift = -100, yshift = 100, font = list(size = 10))) %>% 
        plotly::config(toImageButtonOptions = list(format = "svg", 
                                                   width = 960, height = 720))
      
      
    })
    
    })
    observe({ output$distPlot3a <-renderPlotly({
      
      validate(
        need(input$category1, "Please select a category")
      )
      
     
      
      if(input$Stat=="4 Quarter Moving Sum") {
      
        n=ggplot(dataset4(),
               aes(x=date, y=moving_sum_net_imm, group=1, color=1,
                   text=paste("Date:", date, "\nMoving Sum:", formatC(moving_sum_net_imm, format="d", big.mark=",")))) +
          geom_point()+
          geom_line()+
         
          theme_minimal()+
          
          theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.3,size = 6), 
                legend.position = "none")+
           scale_y_continuous(labels = scales::comma)+

          
          labs(title="Total Net International migration", y="Number of Individuals")+
          theme(
            plot.title = element_text(size=18, face="bold",hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=12),
            axis.text.x = element_text(size=7,angle=90),
            legend.text = element_text(size=8))+
          scale_fill_brewer(palette = "Dark2")}
      if(input$Stat=="Quarterly value") {
        
        n=ggplot(dataset4(),
                 aes(x=date, y=total_net_imm, group=1, color=1,
                     text=paste("Date:", date, "\nQuarterly sum:", formatC(total_net_imm, format="d", big.mark=",")))) +
          
          geom_point()+
          geom_line()+
          guides(fill=guide_legend(title="Total immigration"))+
          theme_minimal()+
          
          theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.3,size = 6), 
                legend.position = "none")+
          scale_y_continuous(labels = scales::comma)+
          
          labs(title="Total Net International migration", y="Number of Individuals")+
          theme(
            plot.title = element_text(size=18, face="bold",hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=12),
            axis.text.x = element_text(size=7,angle=90),
            legend.text = element_text(size=8))+
          scale_fill_brewer(palette = "Dark2")}

      ggplotly(n, tooltip = "text")%>% plotly::layout(
                                                      margin = list(b = 120, l = 100, r = 100, t = 80),
                                                      annotations = list(x = 1, y = -0.4, text = paste(""),
                                                                         showarrow = F, 
                                                                         xref = "paper", yref = "paper",
                                                                         xanchor = "right", 
                                                                         yanchor = "auto", xshift = -100, yshift = 100, font = list(size = 10))) %>% 
        plotly::config(toImageButtonOptions = list(format = "svg", 
                                                   width = 960, height = 720))
    })
    })
    
    
    observe({ output$distPlot3b <-renderPlotly({ 
      validate(
        need(input$category3, "Please select a category")
      )
      if(input$Stat_b=="4 Quarter Moving Sum") {
        h=ggplot(dataset3b(), 
                 aes(x=date, y=moving_sum, group=components_of_population_growth, fill=components_of_population_growth, 
                     text=paste("Date:", date, "\nCategory:", components_of_population_growth, "\nMoving Sum:", formatC(moving_sum, format="d", big.mark=",")))) + 
          geom_bar(stat="identity", na.rm=TRUE)+
          
          theme_minimal()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.3,size = 6))+
          labs(title="NPR inflows and outflows", y="Number of Individuals", fill="Type of immigration")+
          scale_y_continuous(labels = scales::comma)+
          
          theme(
            plot.title = element_text(size=18, face="bold",hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=12),
            axis.text.x = element_text(size=7,angle=90),
            legend.text = element_text(size=8))+
          scale_fill_brewer(palette = "Dark2")}
      
      if(input$Stat_b=="Quarterly value") {
        h=ggplot(dataset3b(), 
                 aes(x=date, y=value, group=components_of_population_growth, fill=components_of_population_growth,
                     text=paste("Date:", date, "\nCategory:", components_of_population_growth, "\nValue:", formatC(value, format="d", big.mark=",")))) + 
          geom_bar(stat="identity", na.rm=TRUE)+
          theme_minimal()+
          
          theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.3,size = 6))+
          labs(title="International migration", y="Number of Individuals", fill="Type of immigration")+
          scale_y_continuous(labels = scales::comma)+
          
          theme(
            plot.title = element_text(size=18, face="bold",hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=12),
            axis.text.x = element_text(size=7,angle=90),
            legend.text = element_text(size=8))+
          scale_fill_brewer(palette = "Dark2")} 
      
      ggplotly(h, tooltip = c("text"))%>% plotly::layout(
                                                         margin = list(b = 120, l = 100, r = 100, t = 80),
                                                         annotations = list(x = 1, y = -0.4, text = paste(""),
                                                                            showarrow = F, 
                                                                            xref = "paper", yref = "paper",
                                                                            xanchor = "right", 
                                                                            yanchor = "auto", xshift = -100, yshift = 100, font = list(size = 10))) %>% 
        plotly::config(toImageButtonOptions = list(format = "svg", 
                                                   width = 960, height = 720))
      
    })
    })
    
    observe({ output$distPlot3_1 <-renderPlotly({ 
       validate(
         need(input$category2, "Please select a category")
       )
      if(input$Stat2=="4 Quarter Moving Sum") {
        ha=ggplot(dataset4a(), 
                 aes(x=date, y=moving_sum_by_category, group=category, fill=category,
                     text=paste("Date:", date, "\nMoving Sum:", formatC(moving_sum_by_category, format="d", big.mark=","),
                                "\nCategory:", category))) +
          
          geom_bar(stat="identity")+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.3,size = 6))+
          labs(title="Elements of Population Change", y="Number of Individuals", fill="Type of population change")+
          theme_minimal()+
          scale_y_continuous(labels = scales::comma)+
          
           theme(
            plot.title = element_text(size=18, face="bold",hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=12),
            axis.text.x = element_text(size=7,angle=90),
            legend.text = element_text(size=8))+
          scale_fill_brewer(palette = "Dark2")}
       
      if(input$Stat2=="Quarterly value") {
        ha=ggplot(dataset4a(), 
                 aes(x=date, y=value, group=category, fill=category, 
                     text=paste("Date:", date, "\nValue:", formatC(value, format="d", big.mark=","),
                                "\nCategory:", category))) +
          geom_bar(stat="identity")+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.3,size = 6))+
          labs(title="Elements of Population Change", y="Number of Individuals", fill="Type of population change")+
          theme_minimal()+
          scale_y_continuous(labels = scales::comma)+
          
          theme(
            plot.title = element_text(size=18, face="bold",hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=12),
            axis.text.x = element_text(size=7,angle=90),
            legend.text = element_text(size=8)+
              scale_fill_brewer(palette = "Dark2"))} 
      
       ggplotly(ha, tooltip = "text")%>% plotly::layout(
                                                        margin = list(b = 120, l = 100, r = 100, t = 80),
                                                        annotations = list(x = 1, y = -0.4, text = paste(""),
                                                                           showarrow = F, 
                                                                           xref = "paper", yref = "paper",
                                                                           xanchor = "right", 
                                                                           yanchor = "auto", xshift = -100, yshift = 100, font = list(size = 10))) %>% 
         plotly::config(toImageButtonOptions = list(format = "svg", 
                                                    width = 960, height = 720))
       
     })
    
    })
     
     
     
    observe({ output$distPlot_sm <-renderPlotly({ 
    
       validate(
         need(input$category2, "Please select a category")
       )
       if(input$Stat2=="4 Quarter Moving Sum") {
         sm=ggplot(dataset4a(), 
                   aes(x=date, y=moving_sum_by_category, group=category, fill=category, shape=category,color=category,
                       text=paste("Date:", date, "\nMoving Sum:", formatC(value, format="d", big.mark=","),
                                  "\nCategory:", category))) +
           geom_point() +
           geom_smooth()+
           theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.3,size = 6))+
           labs(title="Elements of Population Change-LOESS", y="Number of Individuals", fill="Type of population change", color="", shape="")+
           theme_minimal()+
           scale_y_continuous(labels = scales::comma)+
           
           theme(
             plot.title = element_text(size=18, face="bold",hjust = 0.5),
             axis.title.x = element_blank(),
             axis.title.y = element_text(size=12),
             axis.text.x = element_text(size=7,angle=90),
             legend.text = element_text(size=8))+
           scale_fill_brewer(palette = "Dark2")+
           scale_color_brewer(palette = "Dark2")
         # +
         #   scale_shape_viridis(discrete = TRUE, option = "turbo")
           } 
         
        
       
       if(input$Stat2=="Quarterly value") {
         sm=ggplot(dataset4a(), 
                   aes(x=date, y=value,group=category, fill=category, shape=category, color=category,
                       text=paste("Date:", date, "\nValue:", formatC(value, format="d", big.mark=","),
                                  "\nCategory:", category))) +
           geom_point() +
           geom_smooth()+
           theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.3,size = 6))+
           labs(title="Elements of Population Change", y="Number of Individuals", fill="Type of population change")+
           theme_minimal()+
           scale_y_continuous(labels = scales::comma)+
           
           theme(
             plot.title = element_text(size=18, face="bold",hjust = 0.5),
             axis.title.x = element_blank(),
             axis.title.y = element_text(size=12),
             axis.text.x = element_text(size=7,angle=90),
             legend.text = element_text(size=8))+
           scale_fill_brewer(palette = "Dark2")+
           scale_color_brewer(palette = "Dark2")
           } 

              ggplotly(sm, tooltip = "text")%>% plotly::layout(
                                                               margin = list(b = 120, l = 100, r = 100, t = 80),
                                                               annotations = list(x = 1, y = -0.4, text = paste(""),
                                                                                  showarrow = F, 
                                                                                  xref = "paper", yref = "paper",
                                                                                  xanchor = "right", 
                                                                                  yanchor = "auto", xshift = -100, yshift = 100, font = list(size = 10))) %>% 
                plotly::config(toImageButtonOptions = list(format = "svg", 
                                                           width = 960, height = 720))
              
     })
    })
     
     output$distPlot3a_1 <-renderPlotly({
       validate(
         need(input$category2, "Please select a category")
       )
       if(input$Stat2=="4 Quarter Moving Sum") {
         validate(
           need(input$location3, "Please select a province")
         )
         popgrowth=ggplot(dataset4a(),
                  aes(x=date, y=moving_sum, group=1, color=1,
                      text=paste("Date:", date, "\nMoving Sum:", formatC(moving_sum, format="d", big.mark=",")))) +
           
           geom_point()+
          geom_line()+
            theme_minimal()+
           scale_y_continuous(labels = scales::comma)+
           
           
           theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.3,size = 6), 
                 legend.position = "none")+
           labs(title="Total Population Growth", y="Number of Individuals")+
           theme(
             plot.title = element_text(size=18, face="bold",hjust = 0.5),
             axis.title.x = element_blank(),
             axis.title.y = element_text(size=12),
             axis.text.x = element_text(size=7,angle=90),
             legend.text = element_text(size=8))}
      
        if(input$Stat2=="Quarterly value") {
         validate(
           need(input$location3, "Please select a province")
         )
         popgrowth=ggplot(dataset4a(),
                  aes(x=date, y=net_pop_growth,group=1, color=1,
                      text=paste("Date:", date, "\nNet Population Growth:", formatC(net_pop_growth, format="d", big.mark=",")))) +
           geom_point()+
           geom_line()+
           guides(fill=guide_legend(title="Total immigration"))+
            theme_minimal()+
           scale_y_continuous(labels = scales::comma)+
           
           theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=0.3,size = 6), 
                 legend.position = "none")+
           labs(title="Total Population Growth", y="Number of Individuals")+
           theme(
             plot.title = element_text(size=18, face="bold",hjust = 0.5),
             axis.title.x = element_blank(),
             axis.title.y = element_text(size=12),
             axis.text.x = element_text(size=7,angle=90),
             legend.text = element_text(size=8))}
       
       ggplotly(popgrowth, tooltip = "text")%>% plotly::layout(
                                                               margin = list(b = 120, l = 100, r = 100, t = 80),
                                                               annotations = list(x = 1, y = -0.4, text = paste(""),
                                                                                  showarrow = F, 
                                                                                  xref = "paper", yref = "paper",
                                                                                  xanchor = "right", 
                                                                                  yanchor = "auto", xshift = -100, yshift = 100, font = list(size = 10))) %>% 
         plotly::config(toImageButtonOptions = list(format = "svg", 
                                                    width = 960, height = 720))
       
       
     })

     output$distPlot4 <-renderPlot({ 
       
      if(input$Stat3=="4 Quarter Moving Sum") {
        t=ggplot(dataset5(), aes(x = geography_province_of_destination, 
                                  y = geo)) +
          geom_tile(aes(fill = moving_sum_imm))+
          scale_fill_gradient2(low = "lightgreen", high = "darkgreen", midpoint = 1, space = "Lab",
                               na.value = "grey50", guide = "colourbar")+
          geom_text(aes(label=formatC(moving_sum_imm, format="d", big.mark=",")))+
          theme_minimal()+

          scale_x_discrete(labels=function(x){sub("\\s", "\n", x)},position = "top") +
          theme(legend.position="none",
                axis.text.x = element_text(size = 10),
                axis.text.y = element_text(size = 10),
                plot.title = element_text(size=18, face="bold",hjust = 0.5),
                axis.title.y = element_text(size=14, face="bold"),
                axis.title.x = element_text(size=14, face="bold"),
                legend.text = element_text(size=8))+
          geom_segment(x =0, xend = 13.5, y = 13.5, yend = 13.5)+

           labs(title="Interprovincial migration\n", x="Province of destination\n", y="Province of origin", fill="Province")
 
        
      } 
       
      if(input$Stat3=="Quarterly value") {
        t=ggplot(dataset5(), aes(x = geography_province_of_destination, 
                                  y = geo)) +
          geom_tile(aes(fill = value))+
          scale_fill_gradient2(low = "lightgreen", high = "darkgreen", midpoint = 1, space = "Lab",
                               na.value = "grey50", guide = "colourbar")+
          geom_text(aes(label=formatC(value, format="d", big.mark=",")))+
          theme_minimal()+
          scale_x_discrete(labels=function(x){sub("\\s", "\n", x)},position = "top") +
          # scale_y_discrete(labels=function(x){sub("\\s", "\n", x)}) +
          
          theme(legend.position="none",
                axis.text.x = element_text(size = 10),
                axis.text.y = element_text(size = 10),
                plot.title = element_text(size=18, face="bold",hjust = 0.5),
                axis.title.y = element_text(size=14, face="bold"),
                axis.title.x = element_text(size=14, face="bold"),
                legend.text = element_text(size=8))+
          geom_segment(x =0, xend = 13.5, y = 13.5, yend = 13.5)+
          
          labs(title="Interprovincial migration\n", x="Province of destination\n", y="Origin", fill="Province")
      } 
      
      print(t)
     })

     
     output$distPlot6 <-renderPlotly({ 
       validate(
         need(input$orig, "Please select two provinces")
       )
       validate(
         need(input$dest, "Please select two provinces")
       )
       if(input$Stat4=="4 Quarter Moving Sum") {
         
        t1=ggplot(dataset5a(), 
                   aes(x = date, 
                       y = moving_sum_imm,
                       text =paste("Moving Sum:",formatC(moving_sum_imm, format="d", big.mark=",") , "\nDate:", date,
                                   "\nOrigin:", geo, "\nDestination", geography_province_of_destination))) + 
            geom_line(aes( group=geography_province_of_destination,
                           color=geography_province_of_destination
                                                                                                        ))+
          
          
        theme( axis.text.x = element_text(size=7,angle=90),
               legend.title=element_text(size=14))+
          scale_y_continuous(labels = scales::comma)+
          scale_color_brewer(palette = "Dark2")+
          
          labs(title="Interprovincial Migration 2 Province Line Graph - 4 Quarter Moving Sum", x="Date", y="Number of Individuals", 
               color="Destination")
        
        } 
       if(input$Stat4=="Quarterly value") {
         validate(
           need(input$orig, "Please select a province")
         )
         t1=ggplot(dataset5a(),aes(x = date, 
                                   y = value, 
                                  group=geography_province_of_destination, 
                                  color=geography_province_of_destination,
                                  text =paste("\nValue:", formatC(value, format="d", big.mark=","), "\nDate:", date,
                                              "\nOrigin:", geo, "\nDestination", geography_province_of_destination))) + 
           geom_line()+
           scale_y_continuous(labels = scales::comma)+
           
           theme( axis.text.x = element_text(size=7,angle=90),
                  legend.title=element_text(size=14))+

           scale_color_brewer(palette = "Dark2")+
           labs(title="Interprovincial Migration 2 Province Line Graph", x="Date", y="Number of Individuals", color="Destination")
         
       } 
       ggplotly(t1, tooltip = "text")%>% plotly::layout(
                                                        margin = list(b = 120, l = 100, r = 100, t = 80),
                                                        annotations = list(x = 1, y = -0.4, text = paste(""),
                                                                           showarrow = F, 
                                                                           xref = "paper", yref = "paper",
                                                                           xanchor = "right", 
                                                                           yanchor = "auto", xshift = -100, yshift = 100, font = list(size = 10))) %>% 
         plotly::config(toImageButtonOptions = list(format = "svg", 
                                                    width = 960, height = 720))
       
       
       })
      
       
     output$mytable2 <-  DT::renderDataTable({
       mytab2<- dataset8() 
    

       colnames(mytab2)<-c("Year", "Year Total", "Year to Year Value Change")
    
       
       print(mytab2)
     }) 

  output$download <- downloadHandler(
  filename = function(){paste0("population_change", input$pop_grow_type1, "_data.csv")}, 
  content = function(fname){
    write.csv(dataset8(), fname)
  }
  )
    
  
    
     output$mytable <- renderTable({
       
       if(input$Stat4=="4 Quarter Moving Sum"){
      mytab<- table()  %>% 
         mutate(
                Difference =ifelse(row_number()==2,formatC(minus(moving_sum_imm), format="d", big.mark=","),""),
                moving_sum_imm= formatC(moving_sum_imm, format="d", big.mark=","))%>% 
                  dplyr::select(-value)
      colnames(mytab)<-c("Origin", "Destination", "4 Quarter Moving Sum", "Net Migration Between the Provinces")
       }
       
       
       if(input$Stat4=="Quarterly value"){
         mytab<- table() %>% dplyr::select(-moving_sum_imm) %>% 
          mutate( Difference =ifelse(row_number()==2,formatC(minus(value), format="d", big.mark=","),""),
       value= formatC(value, format="d", big.mark=","))
         colnames(mytab)<-c("Origin", "Destination", "Number of People", "Net Migration Between the Provinces")
       }
       
       print(mytab)
     }) 
     

#Infoboxes for the summary page 
    
  output$nprbox <- renderUI ({
    infoBox(
     title= paste( "Non-permanent residents in BC in",  imm_new %>% filter(datemonth==max(datemonth)) %>% dplyr::select(date) %>% slice(1)),
     value= 
       imm_new %>% filter(ref_date==max(ref_date), 
                          geo=="British Columbia",
                          non_permanent_resident_types=="Total, non-permanent residents") %>% 
       dplyr::select(value) %>% 
       mutate(value=formatC(value, format="d", big.mark=",")), 
      icon = icon("list"),
      color = "purple", fill=TRUE, width=6
    )
  })
  
 
  long_text_ <- htmltools::HTML("Change in the number of NPRs")
  
  output$nprpercbox <- renderUI({
    infoBox(
      title=long_text_, 
      subtitle = paste("(from",
                        imm_new %>% filter( datemonth==floor_date(max(datemonth), "month") - years(1)) %>% dplyr::select(date) %>% slice(1),
                        "to", 
                        imm_new %>% filter( date== max(date)) %>% dplyr::select(date) %>% slice(1), ")"),
      
      value= 
        imm_new %>% filter( datemonth%in% c(max(datemonth), floor_date(max(datemonth), "month") - years(1)),
                            geo=="British Columbia",
                            non_permanent_resident_types=="Total, non-permanent residents") %>% 
        mutate(perc=paste0("%",perc=round((value[2]-value[1])/abs(value[1])*100))) %>% 
        dplyr::select(perc) %>%  
        slice(1), 
      icon = icon("percent"),
      color = "purple",fill=TRUE, width=6
    )
  })
 
  
  
  output$intmig <- renderUI({
    infoBox(title = "Net International migration over the last year",

            
            subtitle=paste("(4 quarter moving sum for" , imm %>% filter(datemonth==max(datemonth)) %>% dplyr::select(date) %>% slice(1), ")"),
            value= 
              imm %>% 
              filter(ref_date==max(ref_date), geo=="British Columbia",pop_grow_type=="Net International Migration") %>% 
              mutate(moving_sum_net_imm=formatC(moving_sum_net_imm, format="d", big.mark=",")) %>% 
              dplyr::select(moving_sum_net_imm) %>% 
              slice(1),
            
            icon = icon("line-chart"),
      color = "yellow", fill = TRUE, width=6
    )
  }) 
  

  
  long_text1 <- htmltools::HTML("Change in net international migration")
  
  output$intmigperc <- renderUI({
    infoBox(
      subtitle=paste("(Percent change of 4 quarter moving sum",
                     imm %>% filter( datemonth==floor_date(max(datemonth), "month") - years(1)) %>% dplyr::select(date) %>% slice(1),
                     "to", 
                     imm %>% filter( date== max(date)) %>% dplyr::select(date) %>% slice(1), ")"),
      title= long_text1, 
      value= 
        imm %>% filter(datemonth %in% c(max(datemonth), floor_date(max(datemonth), "month") - years(1)),
                       geo=="British Columbia") %>%
        group_by(ref_date) %>% 
        slice(1) %>% ungroup() %>% 
        mutate(perc=paste0("%",perc=round((moving_sum_net_imm[2]-moving_sum_net_imm[1])/abs(moving_sum_net_imm[1])*100))) %>% 
        dplyr::select(perc) %>%  
        slice(1), 
      
      icon = icon("percent"),
      color = "yellow", fill = TRUE, width=6
    )
  })
  
  
  
  
  output$natgrow <- renderUI({
    infoBox(title = "Natural population growth over the last year",
            
            subtitle=paste("(4 quarter moving sum for" , imm %>% filter(datemonth==max(datemonth)) %>% dplyr::select(date) %>% slice(1), ")"),
            value= 
              imm %>% 
              filter(ref_date==max(ref_date), geo=="British Columbia",pop_grow_type=="Natural Population Growth") %>% 
              mutate(moving_sum_net_nat_pop=formatC(moving_sum_net_nat_pop, format="d", big.mark=",")) %>% 
              dplyr::select(moving_sum_net_nat_pop) %>% 
              slice(1),
            
            icon = icon("line-chart"),
            color = "maroon", fill = TRUE, width=6
    )
  }) 
  
  
  
  
  # Same as above, but with fill=TRUE
  output$natgrowperc <- renderUI({
    infoBox(
      subtitle=paste("(Percent change of 4 quarter moving sum",
                     imm %>% filter( datemonth==floor_date(max(datemonth), "month") - years(1)) %>% dplyr::select(date) %>% slice(1),
                     "to", 
                     imm %>% filter( date== max(date)) %>% dplyr::select(date) %>% slice(1), ")"),
      title= "Change in natural population growth", 
      value= 
        imm %>% filter(datemonth %in% c(max(datemonth), floor_date(max(datemonth), "month") - years(1)),
                       geo=="British Columbia",pop_grow_type=="Natural Population Growth") %>%
        group_by(ref_date) %>% 
        slice(1) %>% ungroup() %>% 
        mutate(perc=paste0("%",perc=round((moving_sum_net_nat_pop[2]-moving_sum_net_nat_pop[1])/abs(moving_sum_net_nat_pop[1])*100))) %>% 
        dplyr::select(perc) %>%  
        slice(1), 
      
      icon = icon("percent"),
      color = "maroon", fill = TRUE, width=6
    )
  })
  
    
  
  
  long_text_2 <- htmltools::HTML("Net interprovincial migration over the last year")
  
  output$intp <- renderUI({
    infoBox(title= long_text_2,
            subtitle=paste("(4 quarter moving sum for" , imm %>% filter(datemonth==max(datemonth)) %>% dplyr::select(date) %>% slice(1), ")"),
            value= 
                imm %>%
                filter(ref_date==max(ref_date), geo=="British Columbia",pop_grow_type=="Net Interprovincial Migration" ) %>%
              
                mutate(moving_sum_imm=formatC(moving_sum_imm, format="d", big.mark=",")) %>%
                dplyr::select(moving_sum_imm) %>%
                slice(1) ,
              
               icon = icon("line-chart"),
      color = "green", fill = TRUE, width=6
    )
  }) 
  
 long_text2 <- htmltools::HTML("Change in net interprovincial migration")
  
  # Same as above, but with fill=TRUE
  output$intperc <- renderUI({
    infoBox(
      
      subtitle=paste("(Percent change of 4 quarter moving sum",
                     imm %>% filter( datemonth==floor_date(max(datemonth), "month") - years(1)) %>% dplyr::select(date) %>% slice(1),
                     "to", 
                     imm %>% filter( date== max(date)) %>% dplyr::select(date) %>% slice(1), ")"),
      value= 
        imm %>% filter(datemonth %in% c(max(datemonth), floor_date(max(datemonth), "month") - years(1)),
                       geo=="British Columbia",pop_grow_type=="Net Interprovincial Migration" ) %>%
        group_by(ref_date) %>% 
        slice(1) %>% ungroup() %>% 
        mutate(perc=paste0("%",perc=round((moving_sum_imm[2]-moving_sum_imm[1])/abs(moving_sum_imm[1])*100))) %>% 
        dplyr::select(perc) %>%  
        slice(1),
      title= long_text2,  
      icon = icon("percent"),
      color = "green", fill = TRUE, width=6
    )
  })
  
  

  
  output$popgrow <- renderUI({
    infoBox(
      title = "Population growth over the last year",
      subtitle=paste("(4 quarter moving sum for" , imm %>% filter(datemonth==max(datemonth)) %>% dplyr::select(date) %>% slice(1), ")"),
      value= 
        imm %>% 
        filter(ref_date==max(ref_date), geo=="British Columbia") %>% 
        mutate(moving_sum=formatC(moving_sum, format="d", big.mark=",")) %>% 
        dplyr::select(moving_sum) %>% 
        slice(1),
      icon = icon("list"),
      color = "orange", fill = TRUE, width=6
    )
  })
  
  long_text <- htmltools::HTML("Yearly population growth rate")
  
  # Same as above, but with fill=TRUE
  output$popgrowperc <- renderUI({
    infoBox(
      title= long_text, 
      subtitle=paste("(from",
                     imm %>% filter( datemonth==floor_date(max(datemonth), "month") - years(1)) %>% dplyr::select(date) %>% slice(1),
                     "to", 
                     imm %>% filter( date== max(date)) %>% dplyr::select(date) %>% slice(1), ")"),
      value= paste0("%", round( (imm %>% 
        filter(ref_date==max(ref_date), geo=="British Columbia") %>% slice(1) %>% dplyr::select(moving_sum))/
        (tot_pop %>% filter(datemonth== floor_date(max(imm$datemonth), "month") - years(1)) %>%
        dplyr::select(value))*100, digits = 1)) ,
      icon = icon("percent"),
      color = "orange", fill = TRUE, width=6
    )
  })
  
})
# Run the application 
shinyApp(ui = ui, server = server)


