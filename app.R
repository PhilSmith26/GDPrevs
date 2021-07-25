# Shiny app to calculate monthly real GDP growth rate 
# revisions and analyze them - July 24, 2021

library(shiny)
library(tidyverse)
library(stringr)
library(lubridate)
library(simplecolors)

r0 <- readRDS("r0.rds")

#================================================================================
Rev_table <- function(indLG,r0) {
  r1 <- dplyr::filter(r0,NAICS==indLG)
  indST <- sub(".*?\\[","[",indLG)
  r1 <- dplyr::rename(r1,"REF"="REF_DATE","VAL"="VALUE") 
  # convert REL to a date
  r1 <- dplyr::mutate(r1,REL=as.Date(Release,format="%B %d, %Y"))
  r1 <- dplyr::mutate(r1,DAY=lubridate::day(REL))
  r1 <- dplyr::mutate(r1,MONTH=lubridate::month(REL))
  r1 <- dplyr::mutate(r1,YEAR=lubridate::year(REL))
  # r1[,.N,DAY] # Display the frequency for all days of the week
  # Result is that 1, 2 and 3 occur occasionally, 21, 22 and 23 
  # sometimes (Xmas) and 28, 29, 30 and 31 most of the time
  # select only the variables needed
  r1 <- dplyr::select(r1,REF,REL,VAL,DAY,MONTH,YEAR)
  # Convert release dates to months (make DAY=1 and shift MONTH 
  # back if needed). For example, release date of February 2, 
  # 2020 becomes January 1, 2020
  r1 <- dplyr::mutate(r1,MONTH=ifelse(DAY==1 | DAY==2 | DAY==3 | 
    DAY==4 | DAY==5, MONTH-1,MONTH))
  # Now reconstruct the release date as the first of the month 
  # in that possibly modified month
  r1 <- dplyr::mutate(r1,REL=as.Date(paste0(YEAR,"-",MONTH,"-01")))
  r1 <- dplyr::select(r1,REF,REL,VAL)
  # Nesting creates a list-column of data frames; unnesting flattens 
  # it back out into regular columns. Nesting is implicitly a 
  # summarising operation: you get one row for each group defined 
  # by the non-nested columns. This is useful in conjunction with 
  # other summaries that work with whole datasets, most notably models.
  # In this case, df1 becomes a list-column with one row for each 
  # release date (REL). The first column is REL and the second 
  # column has two columns with all the reference dates and all 
  # the corresponding values.
  df1 <- tidyr::nest(r1, data = -REL ) # r1 has 5852 rows, df1 has 77
  df1 <- dplyr::rename(df1, REL2 = REL )
  df1 <- dplyr::rowwise(df1) # seems to have no effect
  # The following is the only use of the do_per_REL() function
  # It converts the values to percentage changes
  df1 <- dplyr::mutate(df1, data = list( do_per_REL( data ) ) )
  df1 <- dplyr::ungroup(df1)
  df1 <- tidyr::unnest(df1, cols = "data" )
  df2 <- dplyr::select(df1, REF2, REL2, VAL2 )
  df2 <- dplyr::arrange(df2, REF2, desc( REL2 ), VAL2 )
  df4 <- df2[!is.na(df2$VAL2),]
  df4 <- dplyr::filter(df4,REF2>=as.Date("2015-01-01"))
  if (indST=="[44-45]") {
    REF2 <- c(as.Date("2020-04-01"),
      as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),
      as.Date("2020-08-01"),as.Date("2020-09-01"))
    REL2 <- c(as.Date("2020-05-01"),
      as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),
      as.Date("2020-09-01"),as.Date("2020-10-01"))
    VAL2 <- c(-15.6,19.1,24.5,0.7,1.1,0.0)
    newdf4 <- data.frame(REF2,REL2,VAL2)
    df4 <- rbind(df4,newdf4)
    df4 <- dplyr::arrange(df4,REF2,REL2)
  }
  df5 <- df4
  df5 <- dplyr::group_by(df5,REF2)
  df5 <- dplyr::mutate(df5,range=max(VAL2)-min(VAL2))
  df5 <- dplyr::ungroup(df5)
  df5 <- dplyr::mutate(df5,REF2_pure=REF2)
  df5 <- dplyr::mutate(df5,REF2=paste0(str_sub(as.character(REF2),1,7),"\n Range = ",
    as.character(round(range,2))))
  return(df5)
}
#================================================================================
Rev_charts <- function(df5,indLG) {  
  # This prints the "revisions paths" facet chart
  c1 <- ggplot2::ggplot(dplyr::filter(df5,REF2_pure<as.Date("2021-04-01")),
      aes(x=REL2,y=VAL2,group=REF2))+
    geom_line()+
    labs(title=as.character(indLG),
         caption=paste0("Source: Statistics Canada table 36-10-0491-01."),
         x="",y="Monthly % change") +
    theme(strip.text.x=element_text(size=10,colour="black",face="bold"),
          strip.background=element_rect(colour="black",fill=sc("violet1")),
          panel.background=element_rect(fill=sc("brightgreen1"),colour="black")) +
    theme(panel.grid.minor=element_line(colour="lightgrey",size=0.5)) +
    theme(panel.grid.major=element_line(colour="grey",size=0.5)) +
    theme(plot.title = element_text(size=16,face="bold")) +
    theme(plot.subtitle = element_text(size=12,face="bold")) +
    theme(axis.text.x = element_text(angle=0,hjust=1,size=9)) +
    theme(axis.text.y = element_text(size=9))+
    facet_wrap(~REF2,scales="free_y",ncol=6)
  return(c1)
}  
#================================================================================
Rev_stats <- function(df5) { # df5 is same as r2
  Stats=data.frame()
  rng <- range(str_sub(df5$REF2,1,10))
  r3 <- data.frame(REF=seq.Date(as.Date(paste0(str_sub(rng[1],
    1,7),"-01")),as.Date(paste0(str_sub(rng[2],1,7),"-01")),by="month"))
  r3 <- dplyr::filter(r3,REF<=as.Date("2020-12-01"))
  r3 <- dplyr::mutate(r3,VAL00=round(LUPV(REF,0,df5),2),
    VAL01=round(LUPV(REF,1,df5),2),
    VAL02=round(LUPV(REF,2,df5),2),
    VAL03=round(LUPV(REF,3,df5),2),
    VAL04=round(LUPV(REF,4,df5),2),
    REV01=round(VAL01-VAL00,2),
    REV02=round(VAL02-VAL01,2),
    REV03=round(VAL03-VAL02,2),
    REV04=round(VAL04-VAL03,2))
  (MEAN01 <- mean(r3$REV01))
  (MEAN02 <- mean(r3$REV02))
  (MEAN03 <- mean(r3$REV03))
  (MEAN04 <- mean(r3$REV04))
  (MABS01 <- mean(abs(r3$REV01)))
  (MABS02 <- mean(abs(r3$REV02)))
  (MABS03 <- mean(abs(r3$REV03)))
  (MABS04 <- mean(abs(r3$REV04)))
  (RMS01 <- RMS(r3$REV01))
  (RMS02 <- RMS(r3$REV02))
  (RMS03 <- RMS(r3$REV03))
  (RMS04 <- RMS(r3$REV04))
  Stats[1,1] <- round(MEAN01,3)
  Stats[1,2] <- round(MEAN02,3)
  Stats[1,3] <- round(MEAN03,3)
  Stats[1,4] <- round(MEAN04,3)
  Stats[2,1] <- round(MABS01,3)
  Stats[2,2] <- round(MABS02,3)
  Stats[2,3] <- round(MABS03,3)
  Stats[2,4] <- round(MABS04,3)
  Stats[3,1] <- round(RMS01,3)
  Stats[3,2] <- round(RMS02,3)
  Stats[3,3] <- round(RMS03,3)
  Stats[3,4] <- round(RMS04,3)
  colnames(Stats) <- c("1st revision","2nd revision",
    "3rd revision","4th revision")
  rownames(Stats) <- c("Arithmetic mean","Mean absolute value",
    "Root mean squared")
  dat <- list(Stats,r3)
  return(dat)
}
#================================================================================
Rev_scatter <- function(r3,LAG,indLG) {
  # Scatter chart for 1-, 2-, 3- and 4- month later revs
  if (LAG==1) {titl <- paste0("\nScatter diagram for one-month-later",
    "\nreal GDP growth rate revisions")}
  else if (LAG==2) {titl <- paste0("\nScatter diagram for two-months-later\n",
    "real GDP growth rate revisions")}
  else if (LAG==3) {titl <- paste0("\nScatter diagram for three-months-later\n",
    "real GDP growth rate revisions")}
  else {titl <- paste0("\nScatter diagram for four-months-later\n",
    "real GDP growth rate revisions")}
  p0 <- ggplot(r3)+
    labs(title=paste0(indLG,titl),
         x="Reference months, February 2015 to December 2020",
         y="Percentage point revisions to growth rates",
         caption=paste0("Source: Statistics Canada table 36-10-0491-01."))+
    theme(plot.title = element_text(size=20,face="bold")) +
    theme(plot.subtitle = element_text(size=16,face="bold")) +
    theme(axis.text.x = element_text(angle=0,hjust=1,size=16)) +
    theme(axis.text.y = element_text(size=16))+
    theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
    theme(panel.border = element_rect(fill=NA,colour="black"))+
    theme(plot.background = element_rect(fill="#FFFACD",colour="black"))+
    geom_hline(yintercept=0,colour="black",size=0.8)+
    if (LAG==1) {geom_point(aes(x=REF,y=REV01),colour="black")}
    else if (LAG==2) {geom_point(aes(x=REF,y=REV02),colour="black")}
    else if (LAG==3) {geom_point(aes(x=REF,y=REV03),colour="black")}
    else if (LAG==4) {geom_point(aes(x=REF,y=REV04),colour="black")}
    else {}
    return(p0)
}
#================================================================================
Rev_autocor <- function(r3,LAG,indLG) {
  if (LAG==1) {AUT0 <- acf(r3$REV01,plot=FALSE)} 
  else if (LAG==2) AUT0 <- acf(r3$REV02,plot=FALSE) 
  else if (LAG==3) AUT0 <- acf(r3$REV03,plot=FALSE) 
  else if (LAG==4) AUT0 <- acf(r3$REV04,plot=FALSE) 
  else {}
  tmp <- AUT0[[1]]
  df <- data.frame(lag1=1:length(tmp),auto1=tmp)
  # Autocorrelation chart for 1-, 2-, 3- and 4- month later revs
  if (LAG==1) {titl <- paste0("\nAutocorrelation diagram for one-month-later",
    "\nreal GDP growth rate revisions")}
  else if (LAG==2) {titl <- paste0("\nAutocorrelation diagram for two-months-later\n",
    "real GDP growth rate revisions")}
  else if (LAG==3) {titl <- paste0("\nAutocorrelation diagram for three-months-later\n",
    "real GDP growth rate revisions")}
  else {titl <- paste0("\nAutocorrelation diagram for four-months-later\n",
    "real GDP growth rate revisions")}
  p0 <- ggplot(df)+
    labs(title=paste0(indLG,titl),
         x="Autocorrelation lag",
         y="Correlation",
         caption=paste0("Source: Statistics Canada table 36-10-0491-01."))+
    theme(plot.title = element_text(size=20,face="bold")) +
    theme(plot.subtitle = element_text(size=16,face="bold")) +
    theme(axis.text.x = element_text(angle=0,hjust=1,size=16)) +
    theme(axis.text.y = element_text(size=16))+
    theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
    theme(panel.border = element_rect(fill=NA,colour="black"))+
    theme(plot.background = element_rect(fill="#FFFACD",colour="black"))+
    geom_hline(yintercept=0,colour="black",size=0.8)+
    geom_col(aes(x=lag1,y=auto1),fill="blue",colour="black")
  return(p0)
}
#================================================================================
Rev_density <- function(r3,LAG,indLG) {
  # Probability densities for 1-, 2-, 3- and 4- months later revs
  if (LAG==1) {titl <- paste0("\nEstimated density for one-month-later",
    "\nreal GDP growth rate revisions")}
  else if (LAG==2) {titl <- paste0("\nEstimated density for two-months-later\n",
    "real GDP growth rate revisions")}
  else if (LAG==3) {titl <- paste0("\nEstimated density for three-months-later\n",
    "real GDP growth rate revisions")}
  else {titl <- paste0("\nEstimated density for four-months-later\n",
    "real GDP growth rate revisions")}  
  p1 <- ggplot2::ggplot(r3)+
         geom_vline(xintercept=0,linetype="dotted")+  
         labs(title=paste0(indLG,titl),
         x="Monthly percentage revision in percentage points",
         y="Probability density",
         caption=paste0("Source: Statistics Canada table 36-10-0491-01."))+
         theme(plot.title = element_text(size=20,face="bold")) +
         theme(plot.subtitle = element_text(size=16,face="bold")) +
         theme(axis.text.x = element_text(angle=0,hjust=1,size=16)) +
         theme(axis.text.y = element_text(size=16))+
         theme(panel.border = element_rect(fill=NA,colour="black"))+
         theme(plot.background = element_rect(fill="#FFFACD",colour="black"))+
         geom_hline(yintercept=0,colour="black",size=0.8)+
         if (LAG==1)      {geom_density(aes(x=REV01),fill="hotpink2",
           colour="black",alpha=1.0)}
         else if (LAG==2) {geom_density(aes(x=REV02),fill="darkolivegreen2",
           colour="black",alpha=1.0)}
         else if (LAG==3) {geom_density(aes(x=REV03),fill="cadetblue2",
           colour="black",alpha=1.0)}
         else if (LAG==4) {geom_density(aes(x=REV04),fill="bisque",
           colour="black",alpha=1.0)}
         else {}
  p1 <- p1+geom_vline(xintercept=0,linetype="dotted",size=1)
  return(p1)
}
#================================================================================
IndustriesS <- c("All industries [T001]",                                                                                      
"Agriculture, forestry, fishing and hunting [11]",                                                            
"Mining, quarrying, and oil and gas extraction [21]",                                                         
"Utilities [22]",                                                                                             
"Construction [23]",                                                                                          
"Manufacturing [31-33]",                                                                                      
"Wholesale trade [41]",                                                                                       
"Retail trade [44-45]",                                                                                       
"Transportation and warehousing [48-49]",                                                                     
"Information and cultural industries [51]",                                                                   
"Finance and insurance [52]",                                                                                 
"Real estate and rental and leasing [53]",                                                                    
"Professional, scientific and technical services [54]",                                                       
"Management, scientific and technical consulting services [5416]",                                            
"Administrative and support, waste management and remediation services [56]",                                 
"Educational services [61]",                                                                                  
"Health care and social assistance [62]",                                                                     
"Arts, entertainment and recreation [71]",                                                                    
"Accommodation and food services [72]",                                                                       
"Other services (except public administration) [81]",                                                         
"Public administration [91]")
#================================================================================
IndustriesL <- c("All industries [T001]",                                                                                      
"Goods-producing industries [T002]",
"Service-producing industries [T003]",
"Business sector industries [T004]",                                                                          
"Business sector, goods [T005]",                                                                              
"Business sector, services [T006]",                                                                           
"Non-business sector industries [T007]",                                                                      
"Non-business sector, goods [T008]",                                                                          
"Non-business sector, services [T009]",                                                                       
"Industrial production [T010]",                                                                               
"Non-durable manufacturing industries [T011]",                                                                
"Durable manufacturing industries [T012]",                                                                    
"Information and communication technology sector [T013]",                                                     
"Information and communication technology, manufacturing [T014]",                                             
"Information and communication technology, services [T015]",                                                  
"Energy sector [T016]",                                                                                       
"Industrial production (1950 definition) [T017]",                                                             
"Public Sector [T018]",                                                                                       
"Content and media sector [T019]",                                                                            
#"All industries (except cannabis sector) [T020]",                                                             
#"Cannabis sector [T021]",                                                                                     
#"Cannabis sector (licensed) [T022]",                                                                          
#"Cannabis sector (unlicensed) [T023]",                                                                        
#"All industries (except unlicensed cannabis sector) [T024]",                                                  
"Agriculture, forestry, fishing and hunting [11]",                                                            
"Crop and animal production [11A]",                                                                           
"Crop production [111]",                                                                                      
#"Crop production (except cannabis) [111X]",                                                                   
#"Cannabis production [111C]",                                                                                 
#"Cannabis production (licensed) [111CL]",                                                                     
#"Cannabis production (unlicensed) [111CU]",                                                                   
"Animal production [112]",                                                                                    
"Forestry and logging [113]",                                                                                 
"Fishing, hunting and trapping [114]",                                                                        
"Support activities for agriculture and forestry [115]",                                                      
"Mining, quarrying, and oil and gas extraction [21]",                                                         
"Oil and gas extraction [211]",                                                                               
"Oil and gas extraction (except oil sands) [21111]",                                                          
"Oil sands extraction [21114]",                                                                               
"Mining and quarrying (except oil and gas) [212]",                                                            
"Coal mining [2121]",                                                                                         
"Metal ore mining [2122]",                                                                                    
"Iron ore mining [21221]",                                                                                    
"Gold and silver ore mining [21222]",                                                                         
"Copper, nickel, lead and zinc ore mining [21223]",                                                           
"Other metal ore mining [21229]",                                                                             
"Non-metallic mineral mining and quarrying [2123]",                                                           
"Stone mining and quarrying [21231]",                                                                         
"Sand, gravel, clay, and ceramic and refractory minerals mining and quarrying [21232]",                       
"Other non-metallic mineral mining and quarrying [21239]",                                                    
"Potash mining [212396]",                                                                                     
"Other non-metallic mineral mining and quarrying (except potash) [21239X]",                                   
"Support activities for mining and oil and gas extraction [213]",                                             
"Utilities [22]",                                                                                             
"Electric power generation, transmission and distribution [2211]",                                            
"Natural gas distribution [2212]",                                                                            
"Water, sewage and other systems [2213]",                                                                     
"Construction [23]",                                                                                          
"Residential building construction [23A]",                                                                    
"Non-residential building construction [23B]",                                                                
"Repair construction [23D]",                                                                                  
"Engineering and other construction activities [23X]",                                                        
"Manufacturing [31-33]",                                                                                      
"Food manufacturing [311]",                                                                                   
"Animal food manufacturing [3111]",                                                                           
"Grain and oilseed milling [3112]",                                                                           
"Sugar and confectionery product manufacturing [3113]",                                                       
"Fruit and vegetable preserving and specialty food manufacturing [3114]",                                     
"Dairy product manufacturing [3115]",                                                                         
"Meat product manufacturing [3116]",                                                                          
"Seafood product preparation and packaging [3117]",                                                           
"Bakeries and tortilla manufacturing [3118]",                                                                 
"Other food manufacturing [3119]",                                                                            
"Beverage and tobacco product manufacturing [312]",                                                           
"Soft drink and ice manufacturing [31211]",                                                                   
"Breweries [31212]",                                                                                          
"Wineries and distilleries [3121A]",                                                                          
"Tobacco manufacturing [3122]",                                                                               
"Textile, clothing and leather product manufacturing [31X]",                                                  
"Textile and textile product mills [31A]",                                                                    
"Clothing and leather and allied product manufacturing [31B]",                                                
"Wood product manufacturing [321]",                                                                           
"Sawmills and wood preservation [3211]",                                                                      
"Veneer, plywood and engineered wood product manufacturing [3212]",                                           
"Other wood product manufacturing [3219]",                                                                    
"Paper manufacturing [322]",                                                                                  
"Pulp, paper and paperboard mills [3221]",                                                                    
"Converted paper product manufacturing [3222]",                                                               
"Printing and related support activities [323]",                                                              
"Petroleum and coal product manufacturing [324]",                                                             
"Petroleum refineries [32411]",                                                                               
"Petroleum and coal products manufacturing (except petroleum refineries) [3241A]",                            
"Chemical manufacturing [325]",                                                                               
"Basic chemical manufacturing [3251]",                                                                        
"Resin, synthetic rubber, and artificial and synthetic fibres and filaments manufacturing [3252]",            
"Pesticide, fertilizer and other agricultural chemical manufacturing [3253]",
"Pharmaceutical and medicine manufacturing [3254]",                                                           
"Paint, coating and adhesive manufacturing [3255]",                                                           
"Soap, cleaning compound and toilet preparation manufacturing [3256]",                                        
"Other chemical product manufacturing [3259]",                                                                
"Plastics and rubber products manufacturing [326]",                                                           
"Plastic product manufacturing [3261]",                                                                       
"Rubber product manufacturing [3262]",                                                                        
"Non-metallic mineral product manufacturing [327]",                                                           
"Cement and concrete product manufacturing [3273]",                                                           
"Non-metallic mineral product manufacturing (except cement and concrete products) [327A]",                    
"Primary metal manufacturing [331]",                                                                          
"Iron and steel mills and ferro-alloy manufacturing [3311]",                                                  
"Steel product manufacturing from purchased steel [3312]",                                                    
"Alumina and aluminum production and processing [3313]",                                                      
"Non-ferrous metal (except aluminum) production and processing [3314]",                                       
"Foundries [3315]",                                                                                           
"Fabricated metal product manufacturing [332]",                                                               
"Forging and stamping [3321]",                                                                                
"Architectural and structural metals manufacturing [3323]",                                                   
"Boiler, tank and shipping container manufacturing [3324]",                                                   
"Hardware manufacturing [3325]",                                                                              
"Spring and wire product manufacturing [3326]",                                                               
"Machine shops, turned product, and screw, nut and bolt manufacturing [3327]",                                
"Coating, engraving, heat treating and allied activities [3328]",                                             
"Cutlery, hand tools and other fabricated metal product manufacturing [332A]",                                
"Machinery manufacturing [333]",                                                                              
"Agricultural, construction and mining machinery manufacturing [3331]",                                       
"Industrial machinery manufacturing [3332]",                                                                  
"Commercial and service industry machinery manufacturing [3333]",                                             
"Ventilation, heating, air-conditioning and commercial refrigeration equipment manufacturing [3334]",         
"Metalworking machinery manufacturing [3335]",                                                                
"Engine, turbine and power transmission equipment manufacturing [3336]",                                      
"Other general-purpose machinery manufacturing [3339]",                                                       
"Computer and electronic product manufacturing [334]",                                                        
"Computer and peripheral equipment manufacturing [3341]",                                                     
"Electronic product manufacturing [334B]",                                                                    
"Communications equipment manufacturing [3342]",                                                              
"Semiconductor and other electronic component manufacturing [3344]",                                          
"Other electronic product manufacturing [334A]",                                                              
"Electrical equipment, appliance and component manufacturing [335]",                                          
"Electric lighting equipment manufacturing [3351]",                                                           
"Household appliance manufacturing [3352]",                                                                   
"Electrical equipment manufacturing [3353]",                                                                  
"Other electrical equipment and component manufacturing [3359]",                                              
"Transportation equipment manufacturing [336]",                                                               
"Motor vehicles and parts manufacturing [336Y]",                                                              
"Motor vehicle manufacturing [3361]",                                                                         
"Motor vehicle body and trailer manufacturing [3362]",                                                        
"Motor vehicle parts manufacturing [3363]",                                                                   
"Aerospace product and parts manufacturing [3364]",                                                           
"Miscellaneous transportation equipment manufacturing [336W]",                                                
"Railroad rolling stock manufacturing [3365]",                                                                
"Ship and boat building [3366]",                                                                              
"Other transportation equipment manufacturing [3369]",                                                        
"Furniture and related product manufacturing [337]",                                                          
"Household and institutional furniture and kitchen cabinet manufacturing [3371]",                             
"Office furniture (including fixtures) manufacturing [3372]",                                                 
"Other furniture-related product manufacturing [3379]",                                                       
"Miscellaneous manufacturing [339]",                                                                          
"Medical equipment and supplies manufacturing [3391]",                                                        
"Other miscellaneous manufacturing [3399]",                                                                   
"Wholesale trade [41]",                                                                                       
"Farm product wholesaler-distributors [411]",                                                                 
"Petroleum product wholesaler-distributors [412]",                                                            
"Food, beverage and tobacco wholesaler-distributors [413]",                                                   
"Personal and household goods wholesaler-distributors [414]",                                                 
"Motor vehicle and parts wholesaler-distributors [415]",                                                      
"Building material and supplies wholesaler-distributors [416]",                                               
"Machinery, equipment and supplies wholesaler-distributors [417]",                                            
"Miscellaneous wholesaler-distributors [418]",                                                                
"Wholesale electronic markets, and agents and brokers [419]",                                                 
"Retail trade [44-45]",                                                                                       
"Motor vehicle and parts dealers [441]",                                                                      
"Furniture and home furnishings stores [442]",                                                                
"Electronics and appliance stores [443]",                                                                     
"Building material and garden equipment and supplies dealers [444]",                                          
"Food and beverage stores [445]",                                                                             
"Health and personal care stores [446]",                                                                      
"Gasoline stations [447]",                                                                                    
"Clothing and clothing accessories stores [448]",                                                             
"Sporting goods, hobby, book and music stores [451]",                                                         
"General merchandise stores [452]",                                                                           
"Miscellaneous store retailers [453]",                                                                        
#"Miscellaneous store retailers (except cannabis) [453A]",                                                     
#"Cannabis stores [453B]",                                                                                     
#"Cannabis stores (licensed) [453BL]",                                                                         
#"Cannabis stores (unlicensed) [453BU]",                                                                       
"Non-store retailers [454]",                                                                                  
#"Retail trade (except unlicensed cannabis) [4AZ]",                                                            
"Transportation and warehousing [48-49]",                                                                     
"Air transportation [481]",                                                                                   
"Rail transportation [482]",                                                                                  
"Water transportation [483]",                                                                                 
"Truck transportation [484]",                                                                                 
"Transit, ground passenger, scenic and sightseeing transportation [48Z]",                                     
"Urban transit systems [4851]",
"Taxi and limousine service [4853]",                                                                          
"Other transit and ground passenger transportation and scenic and sightseeing transportation [48A]",          
"Pipeline transportation [486]",                                                                              
"Pipeline transportation of natural gas [4862]",                                                              
"Crude oil and other pipeline transportation [486A]",                                                         
"Support activities for transportation [488]",                                                                
"Postal service and couriers and messengers [49A]",                                                           
"Postal service [491]",                                                                                       
"Couriers and messengers [492]",                                                                              
"Warehousing and storage [493]",                                                                              
"Information and cultural industries [51]",                                                                   
"Publishing industries (except Internet) [511]",                                                              
"Motion picture and sound recording industries [512]",                                                        
"Broadcasting (except Internet) [515]",                                                                       
"Radio and television broadcasting [5151]",                                                                   
"Pay and specialty television [5152]",                                                                        
"Telecommunications [517]",                                                                                   
"Data processing, hosting, and related services [518]",                                                       
"Other information services [519]",                                                                           
"Finance and insurance [52]",                                                                                 
"Credit intermediation and monetary authorities [52X]",                                                       
"Depository credit intermediation and monetary authorities [52B]",                                            
"Local credit unions [52213]",                                                                                
"Banking, monetary authorities and other depository credit intermediation [52BX]",                            
"Non-depository credit intermediation and activities related to credit intermediation [522A]",                
"Non-depository credit intermediation [5222]",                                                                
"Activities related to credit intermediation [5223]",                                                         
"Insurance carriers and related activities [524]",                                                            
"Insurance carriers [5241]",                                                                                  
"Agencies, brokerages and other insurance related activities [5242]",                                         
"Financial investment services, funds and other financial vehicles [52A]",                                    
"Real estate and rental and leasing [53]",                                                                    
"Real estate [531]",                                                                                          
"Lessors of real estate [5311]",                                                                              
"Owner-occupied dwellings [5311A]",                                                                           
"Offices of real estate agents and brokers and activities related to real estate [531A]",                     
"Rental and leasing services and lessors of non-financial intangible assets (except copyrighted works) [53B]",
"Rental and leasing services [532]",                                                                          
"Automotive equipment rental and leasing [5321]",                                                             
"Rental and leasing services (except automotive equipment) [532A]",                                           
"Lessors of non-financial intangible assets (except copyrighted works) [533]",                                
"Professional, scientific and technical services [54]",                                                       
"Legal, accounting and related services [541A]",                                                              
"Legal services [5411]",                                                                                      
"Accounting, tax preparation, bookkeeping and payroll services [5412]",                                       
"Architectural, engineering and related services [5413]",                                                     
"Computer systems design and related services [5415]",                                                        
"Advertising, public relations, and related services [5418]",                                                 
"Other professional, scientific and technical services including scientific research and development [541B]", 
"Specialized design services [5414]",                                                                         
"Management, scientific and technical consulting services [5416]",                                            
"Scientific research and development services [5417]",                                                        
"Other professional, scientific and technical services [5419]",                                               
"Management of companies and enterprises [55]",                                                               
"Administrative and support, waste management and remediation services [56]",                                 
"Administrative and support services [561]",                                                                  
"Office administrative services [5611]",                                                                      
"Employment services [5613]",                                                                                 
"Business support services [5614]",                                                                           
"Travel arrangement and reservation services [5615]",                                                         
"Investigation and security services [5616]",                                                                 
"Services to buildings and dwellings [5617]",                                                                 
"Facilities and other support services [561A]",                                                               
"Waste management and remediation services [562]",                                                            
"Educational services [61]",                                                                                  
"Elementary and secondary schools [6111]",                                                                    
"Community colleges and C.E.G.E.P.s [6112]",                                                                  
"Universities [6113]",                                                                                        
"Other educational services [611A]",                                                                          
"Health care and social assistance [62]",                                                                     
"Ambulatory health care services [621]",                                                                      
"Hospitals [622]",                                                                                            
"Nursing and residential care facilities [623]",                                                              
"Social Assistance [624]",                                                                                    
"Health care [62X]",                                                                                          
"Arts, entertainment and recreation [71]",                                                                    
"Performing arts, spectator sports and related industries, and heritage institutions [71A]",                  
"Amusement, gambling and recreation industries [713]",                                                        
"Gambling industries [7132]",                                                                                 
"Amusement and recreation industries [713A]",                                                                 
"Accommodation and food services [72]",                                                                       
"Accommodation services [721]",                                                                               
"Food services and drinking places [722]",                                                                    
"Other services (except public administration) [81]",                                                         
"Repair and maintenance [811]",                                                                               
"Personal and laundry services [812]",                                                                        
"Religious, grant-making, civic, and professional and similar organizations [813]",                           
"Private households [814]",                                                                                   
"Public administration [91]",                                                                                 
"Federal government public administration [911]",                                                             
"Defence services [9111]",                                                                                    
"Federal government public administration (except defence) [911A]",                                           
"Provincial and territorial public administration [912]",                                                     
"Local, municipal and regional public administration [913]",                                                  
"Aboriginal public administration [914]")
#================================================================================
# The LUP function retrieves values from the r2 data frame, for a given
# reference date and a given LEAD (LEAD=0 for the first estimate)
LUP <- function(REF_value,LEAD,r2) { # lookup single value function
  # First, if the reference date that is passed to the function
  # is a character value, convert it to a date value
  if (class(REF_value)=="character") REF_value <- as.Date(REF_value)
  # The %m+% function adds a number of months (or years) to a
  # given date without rollover, so as.Date("2021-02-01") %m+% 
  # months(3) gives as.Date("2021-05-01").
  # So the following statement calculates a particular release 
  # value for a given reference date, either the first one (LEAD=0)
  # or some future revised one
  REL_value <- as.Date(REF_value %m+% months(2+LEAD))
  # This filter shrinks the data frame so that the revision
  # value VAL2 can be isolated and returned to the calling
  # function
  df <- dplyr::filter(r2,REF2_pure==REF_value & REL2==REL_value)
  return(df$VAL2)
}
#================================================================================
# The LUPV function is similar to the LUP function, but it returns a
# vector of values rather than a single value. It makes calls to
# the LUP function to build up the vector to return
LUPV <- function(REF_vector,LEAD,r2) { # lookup vector function
  if (class(REF_vector)=="character") REF_vector <- as.Date(REF_vector)
  REL_vector <- as.Date(REF_vector %m+% months(2+LEAD))
  VAL_vector <- numeric()
  for (i in 1:length(REF_vector)) {
    VAL_vector[i] <- LUP(REF_vector[i],LEAD,r2)
  }
  return(VAL_vector)
}
#================================================================================
RMS <- function(x) {
  rms <- sqrt(mean((x-mean(x))^2))
}
#================================================================================
do_per_REL <- function(DF) {
    # range() returns a vector containing the minimum and maximum of 
    # all the given arguments.
    rng <- range(DF$REF) # watch out for missing months?
    DF <- (data.frame(REF=seq(rng[1],rng[2],by="month"))
      # left_join() returns all rows from x, and all columns from x and y. 
      # Rows in x with no match in y will have NA values in the new columns. 
      # If there are multiple matches between x and y, all combinations of 
      # the matches are returned.
      # The next statement compares the input DF to the sequential one
      # just created and adds NAs if any are missing in DF
      %>% dplyr::left_join(DF,by="REF")
          %>% dplyr::arrange(REF) # arrange REF from first to last ref period
          )
    # with() evaluates an R expression in an environment constructed from data, 
    # possibly modifying (a copy of) the original data.
    # REF2=REF[-1] just drops the first item in the vector, which is
    # necessary because the % change calculation shortens the vector
    # with() example: z = with(mtcars, mean(cyl + disp + wt))
    with(DF,data.frame(REF2=REF[-1],VAL2=100*diff(VAL)/VAL[-length(VAL)]))
  }
#===============================================================================

ui <- fluidPage(
  theme=bslib::bs_theme(bg="#EEC900",fg="black",base_font="Source Sans Pro",
    primary="#0000FF",secondary="#63B8FF",success="#54FF9F",info="#40E0D0",
    warning="#FFFF00",danger="#FF0000",heading_font="Helvetica",font_scale=1.1),
  # #0b3d91 - blue
  #tags$head(tags$style(HTML('* {font-family: "Helvetica"};'))),
  title = tags$b(tags$span(style="color:red", 
  "Monthly GDP revisions analysis")),
  windowTitle = "Canadian monthly GDP revisions analysis",
  fluidRow(column(3,HTML("<h2><b>GDP revisions analysis</b></h2>")),
    tags$style(HTML(".selectize-input, .option {
        color:blue; 
        font-size:26px;
        font-family:'Source Sans Pro'
      }")),
    column(9,selectInput("Industry",tags$b(tags$span(style="color:blue;
      font-weight:900;font-size:20px;font-family:'Source Sans Pro'", 
      "Choose an industry:")),    # "Choose an industry:",
      choices=IndustriesL,width="50%"))),
  navlistPanel(widths=c(3,9),
    tabPanel("Description",
      htmlOutput("textInfo"),
      HTML("<br><br><br><br><br><br><br><br>")),
    tabPanel("Scatter diagrams",
      htmlOutput("scatInfo"),
      fluidRow(
        column(6,plotOutput("scatter1")),
        column(6,plotOutput("scatter2"))),
      HTML("<br>"),
      fluidRow(
        column(6,plotOutput("scatter3")),
        column(6,plotOutput("scatter4")))),
    tabPanel("Measures of central tendency and dispersion",
      htmlOutput("centralTendencyInfo"),
      htmlOutput("industry"),
      tableOutput("stats")),
    tabPanel("Autocorrelations",
      htmlOutput("autocInfo"),
      fluidRow(
        column(6,plotOutput("autocor1")),
        column(6,plotOutput("autocor2"))),
      HTML("<br>"),
      fluidRow(
        column(6,plotOutput("autocor3")),
        column(6,plotOutput("autocor4")))),      
    tabPanel("Estimated probability densities",
      htmlOutput("densityInfo"),
      fluidRow(
        column(6,plotOutput("density1")),
        column(6,plotOutput("density2"))),
      HTML("<br>"),
      fluidRow(
        column(6,plotOutput("density3")),
        column(6,plotOutput("density4")))),
    tabPanel("Revision time paths",
      htmlOutput("pathsInfo"),
      plotOutput("RevChrt",width="100%",height="2500px"))
  )
)
server <- function(input,output,session) {
  info <- "textInfo.html"
  output$textInfo <- renderUI(includeHTML(info))
  ctinfo <- "Central_tendency_measures.html"
  output$centralTendencyInfo <- renderUI(includeHTML(ctinfo))
  scat <- "scatterInfo.html"
  output$scatInfo <- renderUI(includeHTML(scat))
  autoc <- "autocorInfo.html"
  output$autocInfo <- renderUI(includeHTML(autoc))
  dens <- "densityInfo.html"
  output$densityInfo <- renderUI(includeHTML(dens))
  paths <- "pathsInfo.html"
  output$pathsInfo <- renderUI(includeHTML(paths))
  output$industry <- renderText(ind())
  ind <- reactive(input$Industry)
  revs <- reactive(Rev_table(ind(),r0))
  chrt <- reactive(Rev_charts(revs(),ind()))
  dat <- reactive({Rev_stats(revs())})
  output$RevChrt <- renderPlot({chrt()})
  output$stats <- renderTable(dat()[[1]],rownames=TRUE,bordered=TRUE)
  output$scatter1 <- renderPlot({Rev_scatter(dat()[[2]],1,ind())})
  output$scatter2 <- renderPlot({Rev_scatter(dat()[[2]],2,ind())})
  output$scatter3 <- renderPlot({Rev_scatter(dat()[[2]],3,ind())})
  output$scatter4 <- renderPlot({Rev_scatter(dat()[[2]],4,ind())})
  output$autocor1 <- renderPlot({Rev_autocor(dat()[[2]],1,ind())})
  output$autocor2 <- renderPlot({Rev_autocor(dat()[[2]],2,ind())})
  output$autocor3 <- renderPlot({Rev_autocor(dat()[[2]],3,ind())})
  output$autocor4 <- renderPlot({Rev_autocor(dat()[[2]],4,ind())})
  output$density1 <- renderPlot({Rev_density(dat()[[2]],1,ind())})
  output$density2 <- renderPlot({Rev_density(dat()[[2]],2,ind())})
  output$density3 <- renderPlot({Rev_density(dat()[[2]],3,ind())})
  output$density4 <- renderPlot({Rev_density(dat()[[2]],4,ind())})
}
shinyApp(ui,server)
