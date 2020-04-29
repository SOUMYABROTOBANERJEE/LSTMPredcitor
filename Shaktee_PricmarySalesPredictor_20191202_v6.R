########### Change date at three places 
library(tidyr)
library(data.table)
library(forecast)
library(dplyr)
library(lubridate)
library(reshape2)

#Set Path
setwd("C:\\Users\\152368\\Desktop\\06. PARAS Primary Sales Predictor\\06. Input Data")
pred_path <- "C:\\Users\\152368\\Desktop\\06. PARAS Primary Sales Predictor\\08. Output\\02. Prediction"
validate_path <- "C:\\Users\\152368\\Desktop\\06. PARAS Primary Sales Predictor\\08. Output\\01. Validation"
final_result <- "C:\\Users\\152368\\Desktop\\06. PARAS Primary Sales Predictor\\08. Output\\03. Final Files"

#Clear off the Prediction and Validation Results
if (length(list.files(pred_path,pattern="*.csv")) > 0){ do.call(file.remove, list(list.files(pred_path, full.names = TRUE)))}
if (length(list.files(validate_path,pattern="*.csv")) > 0){ do.call(file.remove, list(list.files(validate_path, full.names = TRUE)))}

#Input Consideration
ZSS_Dump <- read.csv(readline("What is the name of KE30 input?"), stringsAsFactors = FALSE, header = T)
Summary_Sales <- read.csv(readline("What is the name of Sales Summary input?"), stringsAsFactors = FALSE, header = T)
date_consideration <- (as.Date(strptime(readline("What is the standing month? (YYYY-mm-dd)"), "%Y-%m-%d")))
WhishList <- read.csv(readline("What is the name of Wishlist input?"), stringsAsFactors = FALSE, header = T)
colnames(WhishList)[which(names(WhishList) == readline("What is the column name for validation?"))] <- "Wishlist_SaleMT"

#Rename Columns

setnames(ZSS_Dump, old = c("ï..Month." , "Invoice.Date",  "Customer.Name",          "Customer.Code",           "Material.Code",  
                           "Product.Width",       "Material.Description",  "Thickness",       "PCode",             "Actual.Volume"), 
         new = c("MON_YR","BILLING.DATE","CUSTOMER.NAME","CUSTOMER.CD",
                 "MATERIAL.NO","SEC2.MAX","MATERIAL.DESCRI","SEC1.MAX","PCODE","QTY"))

ZSS_Dump$MATERIAL.NO <- as.numeric(ZSS_Dump$MATERIAL.NO)

#Manipulate Wishlist
WhishList$Dist_code <- trimws(substr(WhishList$Customer.Code,1,10))
WhishList$Dist_code <- gsub("(?<![0-9])0+", "", WhishList$Dist_code, perl = TRUE)
WhishList$Dist_Matl <- paste(WhishList$Dist_code, sep = "-" ,WhishList$Material.No.)


#Creating Mon_Yr
ZSS_Dump$BILLING.DATE <- as.Date(format(as.POSIXct(ZSS_Dump$BILLING.DATE,format='%m/%d/%Y'),format='%Y-%m-%d'))

#Creating Mon_Yr
#ZSS_Dump$MON_YR_1 <- (as.Date(strptime(ZSS_Dump$MON_YR, "%m/%d/%Y")))
ZSS_Dump$MonYr <- format(as.Date(ZSS_Dump$BILLING.DATE, format='%Y-%m-%d'),"%Y-%m-01")
ZSS_Dump$MON_YR_1 <- (as.Date(strptime(ZSS_Dump$MonYr, "%Y-%m-%d")))
summary(ZSS_Dump$MON_YR_1)

#Input variables

date_validation_month <- as.Date(date_consideration , "%Y-%m-%d") %m-% months(1) 
date_max_validation_data <-  as.Date(date_validation_month , "%Y-%m-%d") %m-% months(1) 
pred_month_data <- date_validation_month

min_date <- format(as.Date("04/01/2016", "%m/%d/%Y"),"%m/%d/%Y")
max_date <- format(as.Date("12/01/2021", "%m/%d/%Y"),"%m/%d/%Y")

#Distributors considered  
dist_list <-  unique(ZSS_Dump$CUSTOMER.CD)

############################Creating Rawdata for module ##################################
##########################################################################################

########## ADD FILTER 
######## 1. Sale > 0
######## 2. PCODE: C04

ZSS_Dump_2 <- subset(ZSS_Dump , (QTY > 0 & PCODE == "C04"))
sum(ZSS_Dump_2$QTY) ## 722008.5
Dist_Matl_MonYr <- ZSS_Dump_2 %>% 
  group_by(CUSTOMER.CD, MATERIAL.NO, MON_YR_1) %>% 
  summarise(SaleMT = sum(QTY))
Dist_Matl_MonYr$Dist_Matl <- paste0(Dist_Matl_MonYr$CUSTOMER.CD , "-", Dist_Matl_MonYr$MATERIAL.NO)

#### Change Startdate and end date as per original data ######################################################
df <- data.frame(Name = unique(Dist_Matl_MonYr$Dist_Matl),
                 StartDate = (as.Date(strptime(min_date, "%m/%d/%Y"))),
                 EndDate = (as.Date(strptime(max_date, "%m/%d/%Y"))))
df$Name <- as.character(df$Name)
str(df)

M <- Map(seq, df$StartDate, df$EndDate, by = "month")
df2 <- data.frame(
  Name = rep.int(df$Name, vapply(M, length, 1L)), 
  Dte = do.call(c, M))
df2$Name <- as.character(df2$Name)

###QC Check 
sum(Dist_Matl_MonYr$SaleMT) ## 722008.5


############################### Validation Module #######################################################
#########################################################################################################
temp_1 <- Dist_Matl_MonYr[which(Dist_Matl_MonYr$CUSTOMER.CD %in% dist_list),]

df3 <- merge(df2, temp_1[c("Dist_Matl", "MON_YR_1","SaleMT")],
             by.x = c("Name", "Dte"), by.y =  c("Dist_Matl", "MON_YR_1"),
             all.x = T)
df3[is.na(df3)] <- 0

#########################################################################################################
###CHANGE
df3 <- df3[which(df3$Dte < as.Date(strptime(date_max_validation_data, "%Y-%m-%d"))),]
max(df3$Dte)
unique(df3$Dte)
str(df3)

df3_1 <- df3 %>% arrange(Name, Dte)
df3_1 <- df3_1 %>% group_by(Name) %>% mutate(CSum1 = cumsum(SaleMT))
df4 <- subset(df3_1, df3_1$CSum1 > 0)
df4 <- df4 %>% group_by(Name) %>% mutate(Count_uni = length(unique(SaleMT)))
df4 <- df4 %>% group_by(Name) %>% mutate(Sum_sales = sum(SaleMT))
df4$Flag <- ifelse( df4$Count_uni <= 2 ,1,0)

sum(df4$Flag)

df5 <- subset(df4 , Flag == 0 )
df5 <- subset(df5, df5$Name %in% unique(WhishList$Dist_Matl))

getwd()

Dist_SKU <- unique(df5$Name)

#Prediction Output Directory
setwd(validate_path)

### length(Dist_SKU) to be updated later

for (i in 1:length(Dist_SKU)) {
  print(i)
  # browser()
  
  ##### Using the Sink command to divert R output to a connection
  Text_File_Code <- paste0("sink('",i,"_",Dist_SKU[i],".txt')")
  eval(parse(text = Text_File_Code))
  
  ##### Printing Dist_SKU name at the top of Each .txt file that is being exported later
  print("  ################################            Printing 'Dist_SKU ID' output      ##################################   ")
  cat("\n\n")
  print(Dist_SKU[i])
  cat("\n\n")
  
  ##### Subsetting the relevant Dist_SKU ID from the Source Input Data based on Index 
  Current_ID <- subset(df5, df5$Name %in% Dist_SKU[i])    ### CHANGE
  # browser()
  
  ##### Remove STARTING ZERO Sales
  Current_ID_1 <- Current_ID %>% arrange(Dte)
  Current_ID_1 <-Current_ID_1 %>% mutate(CSum = cumsum(SaleMT))
  Current_ID <- Current_ID_1[which(Current_ID_1$CSum > 0),]
  
  ##### Extracting the Month from the MonYr Column of CurrentID
  month <-  month(as.POSIXlt(Current_ID$Dte[1], format="%m"))
  
  ##### Extracting the Year from the MonYr_1 Column of CurrentID
  year <-  year(as.POSIXlt(Current_ID$Dte[1], format="%Y"))
  
  ##### COnverting the SaleKG series in the TimeSeries, using the Year and Month column used above
  tsData = ts(Current_ID$SaleMT, start = c(year,month), frequency = 12)     ### CHANGE
  
  ##### Using the auto arima on the SaleKG Series 
  fit_xreg_Try <- auto.arima(Current_ID[,"SaleMT"])
  
  #####
  print("  ################################            Printing 'fit_xreg_Try' output      ##################################   ")
  cat("\n\n")
  print(fit_xreg_Try)
  cat("\n\n")
  
  ##### Using the arimaorder function to extract the values of p,d,q from "fit_xreg_Try"
  PDQ_Table <- arimaorder(fit_xreg_Try)
  
  ##### Printing the P,D,Q values in the console
  print("  ################################            Printing 'pdq' values      ##################################   ")
  cat("\n\n")
  print(PDQ_Table)
  cat("\n\n")
  
  ##### Extracting each value of P,D,Q respectively and putting in a variable for arima function up ahead!
  Auto_Arima_P <- print(PDQ_Table[[1]])
  Auto_Arima_D <- print(PDQ_Table[[2]])
  Auto_Arima_Q <- print(PDQ_Table[[3]])
  
  ##### Using the Variable Price to put the entire series of SaleKg in it
  Price <- Current_ID$SaleMT
  ### QC STEP : The length of Price should match with the Current ID rowcount
  
  fitARIMA <- arima(tsData, order = c(Auto_Arima_P,Auto_Arima_D,Auto_Arima_Q),                  ### CHANGE
                    seasonal = list(order = c(0,0,0),
                                    period = 12),
                    method = "CSS-ML")
  print("  ################################            Printing 'fitarima' results      ##################################   ")
  cat("\n\n")
  print(fitARIMA)
  cat("\n\n")
  
  pred_ARIMA <- as.data.frame(predict(fitARIMA, n.ahead = 4))
  print("  ###############            Printing 'Predicted' results      #################   ")
  cat("\n\n")
  print(pred_ARIMA_df <- as.data.frame(pred_ARIMA))
  cat("\n\n")
  
  ##### Adding the SKU Code so that it is recognise the results and perform opration once all SKUs are collated
  pred_ARIMA_df$Dist_MaterialCode <- Dist_SKU[i]
  
  write_code <- paste0("write.csv(pred_ARIMA_df,'pred_ARIMA_df", i, "_", Dist_SKU[i], ".csv')")
  eval(parse(text = write_code))
  
  sink()
  closeAllConnections()
}

totalFiles <- length(list.files(getwd(),pattern="*.csv"))

files <- list.files(pattern = "\\.csv$")

for (j in 1:totalFiles) {
  DF <-  read.csv(files[j], header = T, stringsAsFactors = F)
  if (j == 1) {
    DF_Final <- DF
  } else DF_Final <- rbind(DF_Final, DF)
}
setwd(final_result)
QC <- data.frame("Results" = 'Validation' ,"Min Date" = min(df5$Dte), "Max Date" = max(df5$Dte))
write.csv(DF_Final,"Validation_SaleMT.csv",row.names = F)


# Get Predictions for Validation
DF_Final_Pred <- subset(DF_Final,DF_Final$X == 2)
#Coalesce values <0 to 0
DF_Final_Pred$pred <- ifelse(DF_Final_Pred$pred <0,0,DF_Final_Pred$pred)
DF_Final_Pred <- DF_Final_Pred[,c("Dist_MaterialCode","pred")]

#Actual for Month of Validation
Dist_Matl_MonYr_Actual <- subset(Dist_Matl_MonYr,(Dist_Matl_MonYr$MON_YR_1 == pred_month_data) & Dist_Matl_MonYr$CUSTOMER.CD %in% dist_list)
#write.csv(Dist_Matl_MonYr_Actual,"Dist_Matl_MonYr_Actual.csv")
#Wishlist for Aug'19
# library(stringr)

DF_Final_Pred_1 <- merge(DF_Final_Pred, Dist_Matl_MonYr_Actual,
                         by.x = c("Dist_MaterialCode"), by.y =  c("Dist_Matl"),
                         all.x = T)

#QC Check
WhishList <- subset(WhishList, WhishList$Dist_code %in% dist_list)
WhishList_1 <- merge(WhishList,DF_Final_Pred_1,
                     by.x = c("Dist_Matl"), by.y =  c("Dist_MaterialCode"),
                     all.x = T)
WhishList_1 <- subset(WhishList_1 , select = c("Zone","Dist_code","Dist_Matl","Customer.Code","Customer.Name","Material.No.","Material.Description",
                                               "SKU.Group","Thickness" ,"Width","Wishlist_SaleMT","SaleMT","pred"))
names(WhishList_1)[names(WhishList_1) == 'pred'] <- 'Pred_SaleMT'


WhishList_1$SaleMT[is.na(WhishList_1$SaleMT)] <- 0
WhishList_1$Pred_SaleMT[is.na(WhishList_1$Pred_SaleMT)] <- 0
#write.csv(WhishList_1,"WhishList_1.csv")
##### Group by Dist Code #######
Summary_Matl <- WhishList_1 %>% group_by(Zone,Dist_code,Customer.Name) %>% summarise(SaleMT = sum(SaleMT),
                                                                      Wishlist_SaleMT = sum(Wishlist_SaleMT),
                                                                      Pred_SaleMT = sum(Pred_SaleMT)
                                                                     )

Summary_Matl$MAPE_WISHLIST <- abs(((Summary_Matl$Wishlist_SaleMT) - Summary_Matl$SaleMT)/Summary_Matl$SaleMT)
Summary_Matl$MAPE_Pred <- abs(((Summary_Matl$Pred_SaleMT) - Summary_Matl$SaleMT)/Summary_Matl$SaleMT)

Summary_Matl$SaleMT <- round(Summary_Matl$SaleMT,0)
Summary_Matl$Pred_SaleMT <- round(Summary_Matl$Pred_SaleMT,0)
Summary_Matl$Wishlist_SaleMT <- round(Summary_Matl$Wishlist_SaleMT,0)
Summary_Matl$MAPE_WISHLIST <- round(Summary_Matl$MAPE_WISHLIST*100,0)
colnames(Summary_Matl)[which(names(Summary_Matl) == "MAPE_WISHLIST")] <- "MAPE_WISHLIST(%)"
Summary_Matl$MAPE_Pred <- round(Summary_Matl$MAPE_Pred*100,0)
colnames(Summary_Matl)[which(names(Summary_Matl) == "MAPE_Pred")] <- "MAPE_Pred(%)"

##### Group by SKU #######

WhishList_1$MATERIAL.NO_1 <- paste0(ifelse(WhishList_1$Width == 1220, 'WIDER', 'NARROW'),WhishList_1$Thickness *100, "MM")

Summary_Matl_1 <- WhishList_1 %>% group_by(MATERIAL.NO_1) %>% summarise(SaleMT = sum(SaleMT),
                                                                  Wishlist_SaleMT = sum(Wishlist_SaleMT),
                                                                  Pred_SaleMT = sum(Pred_SaleMT))

Summary_Matl_1$MAPE_WISHLIST <- abs(((Summary_Matl_1$Wishlist_SaleMT) - Summary_Matl_1$SaleMT)/Summary_Matl_1$SaleMT)
Summary_Matl_1$MAPE_Pred <- abs(((Summary_Matl_1$Pred_SaleMT) - Summary_Matl_1$SaleMT)/Summary_Matl_1$SaleMT)


Summary_Matl_1$SaleMT <- round(Summary_Matl_1$SaleMT,0)
Summary_Matl_1$Pred_SaleMT <- round(Summary_Matl_1$Pred_SaleMT,0)
Summary_Matl_1$Wishlist_SaleMT <- round(Summary_Matl_1$Wishlist_SaleMT,0)
Summary_Matl_1$MAPE_WISHLIST <- round(Summary_Matl_1$MAPE_WISHLIST * 100,0)
colnames(Summary_Matl_1)[which(names(Summary_Matl_1) == "MAPE_WISHLIST")] <- "MAPE_WISHLIST(%)"
Summary_Matl_1$MAPE_Pred <- round(Summary_Matl_1$MAPE_Pred *100,0)
colnames(Summary_Matl_1)[which(names(Summary_Matl_1) == "MAPE_Pred")] <- "MAPE_Pred(%)"
setwd(final_result)
# Start a sink file with a CSV extension
sink(paste0('Model_Results',format(as.Date(pred_month_data , "%Y-%m-%d") , "%b-%Y"),'.csv'))

# Write the first dataframe, with a title and final line separator 
cat('Distributor Code')
cat('\n')
write.csv(Summary_Matl,row.names = F)
cat('____________________________')

cat('\n')
cat('\n')

# Write the 2nd dataframe to the same sink
cat('SKU Name')
cat('\n')
write.csv(Summary_Matl_1,row.names = F)
cat('____________________________')

# Close the sink
sink()

closeAllConnections()
############################### Prediction Module #######################################################
#########################################################################################################
temp <- Dist_Matl_MonYr[which(Dist_Matl_MonYr$CUSTOMER.CD %in% dist_list),]

df3 <- merge(df2, temp[c("Dist_Matl", "MON_YR_1","SaleMT")],
             by.x = c("Name", "Dte"), by.y =  c("Dist_Matl", "MON_YR_1"),
             all.x = T)
df3[is.na(df3)] <- 0


df3 <- df3[which(df3$Dte <= as.Date(strptime(pred_month_data, "%Y-%m-%d"))),]

max(df3$Dte)
str(df3)

df3_1 <- df3 %>% arrange(Name, Dte)
df3_1 <- df3_1 %>% group_by(Name) %>% mutate(CSum1 = cumsum(SaleMT))
df4 <- subset(df3_1, df3_1$CSum1 > 0)
df4 <- df4 %>% group_by(Name) %>% mutate(Count_uni = length(unique(SaleMT)))
df4 <- df4 %>% group_by(Name) %>% mutate(Sum_sales = sum(SaleMT))
df4$Flag <- ifelse( df4$Count_uni <= 3 ,1,0)
sum(df4$Flag)

df5 <- subset(df4 , Flag == 0 )
getwd()

Dist_SKU <- unique(df5$Name)

#Prediction Output Directory
setwd(pred_path)

### length(Dist_SKU) to be updated later

for (i in 1:length(Dist_SKU)) {
  print(i)
  # browser()
  
  ##### Using the Sink command to divert R output to a connection
  Text_File_Code <- paste0("sink('",i,"_",Dist_SKU[i],".txt')")
  eval(parse(text = Text_File_Code))
  
  ##### Printing Dist_SKU name at the top of Each .txt file that is being exported later
  print("  ################################            Printing 'Dist_SKU ID' output      ##################################   ")
  cat("\n\n")
  print(Dist_SKU[i])
  cat("\n\n")
  
  ##### Subsetting the relevant Dist_SKU ID from the Source Input Data based on Index 
  Current_ID <- subset(df5, df5$Name %in% Dist_SKU[i])    ### CHANGE
  # browser()
  
  ##### Remove STARTING ZERO Sales
  Current_ID_1 <- Current_ID %>% arrange(Dte)
  Current_ID_1 <-Current_ID_1 %>% mutate(CSum = cumsum(SaleMT))
  Current_ID <- Current_ID_1[which(Current_ID_1$CSum > 0),]
  
  ##### Extracting the Month from the MonYr Column of CurrentID
  month <-  month(as.POSIXlt(Current_ID$Dte[1], format="%m"))
  
  ##### Extracting the Year from the MonYr_1 Column of CurrentID
  year <-  year(as.POSIXlt(Current_ID$Dte[1], format="%Y"))
  
  ##### COnverting the SaleKG series in the TimeSeries, using the Year and Month column used above
  tsData = ts(Current_ID$SaleMT, start = c(year,month), frequency = 12)     ### CHANGE
  
  ##### Using the auto arima on the SaleKG Series 
  fit_xreg_Try <- auto.arima(Current_ID[,"SaleMT"])
  
  #####
  print("  ################################            Printing 'fit_xreg_Try' output      ##################################   ")
  cat("\n\n")
  print(fit_xreg_Try)
  cat("\n\n")
  
  ##### Using the arimaorder function to extract the values of p,d,q from "fit_xreg_Try"
  PDQ_Table <- arimaorder(fit_xreg_Try)
  
  ##### Printing the P,D,Q values in the console
  print("  ################################            Printing 'pdq' values      ##################################   ")
  cat("\n\n")
  print(PDQ_Table)
  cat("\n\n")
  
  ##### Extracting each value of P,D,Q respectively and putting in a variable for arima function up ahead!
  Auto_Arima_P <- print(PDQ_Table[[1]])
  Auto_Arima_D <- print(PDQ_Table[[2]])
  Auto_Arima_Q <- print(PDQ_Table[[3]])
  
  ##### Using the Variable Price to put the entire series of SaleKg in it
  Price <- Current_ID$SaleMT
  ### QC STEP : The length of Price should match with the Current ID rowcount
  
  fitARIMA <- arima(tsData, order = c(Auto_Arima_P,Auto_Arima_D,Auto_Arima_Q),                  ### CHANGE
                    seasonal = list(order = c(0,0,0),
                                    period = 12),
                    method = "CSS-ML")
  print("  ################################            Printing 'fitarima' results      ##################################   ")
  cat("\n\n")
  print(fitARIMA)
  cat("\n\n")
  
  pred_ARIMA <- as.data.frame(predict(fitARIMA, n.ahead = 4))
  print("  ###############            Printing 'Predicted' results      #################   ")
  cat("\n\n")
  print(pred_ARIMA_df <- as.data.frame(pred_ARIMA))
  cat("\n\n")
  
  ##### Adding the SKU Code so that it is recognise the results and perform opration once all SKUs are collated
  pred_ARIMA_df$Dist_MaterialCode <- Dist_SKU[i]
  
  write_code <- paste0("write.csv(pred_ARIMA_df,'pred_ARIMA_df", i, "_", Dist_SKU[i], ".csv')")
  eval(parse(text = write_code))
  
  sink()
  closeAllConnections()
}

totalFiles <- length(list.files(getwd(),pattern="*.csv"))

files <- list.files(pattern = "\\.csv$")

for (j in 1:totalFiles) {
  DF <-  read.csv(files[j], header = T, stringsAsFactors = F)
  if (j == 1) {
    DF_Final <- DF
  } else DF_Final <- rbind(DF_Final, DF)
}
#Coalesce values <0 to 0
DF_Final$pred <- ifelse(DF_Final$pred <0,0,DF_Final$pred)
write.csv(DF_Final,"Pred_SaleMT.csv")

### Making the final Dataset
DF_Final$pred <- round(DF_Final$pred,1)
dataset_final <- dcast(DF_Final, Dist_MaterialCode~X, value.var = 'pred')


dataset_final[,c("1")] <- NULL
colnames(dataset_final)[which(names(dataset_final) == "2")] <- format(as.Date(pred_month_data , "%Y-%m-%d") %m+% months(2), "%b-%Y")
colnames(dataset_final)[which(names(dataset_final) == "3")] <- format(as.Date(pred_month_data , "%Y-%m-%d") %m+% months(3), "%b-%Y")
colnames(dataset_final)[which(names(dataset_final) == "4")] <- format(as.Date(pred_month_data , "%Y-%m-%d") %m+% months(4), "%b-%Y")
t <- paste(format(as.Date(pred_month_data , "%Y-%m-%d") %m+% months(2), "%b-%Y"), format(as.Date(pred_month_data , "%Y-%m-%d") %m+% months(4), "%b-%Y"), sep = '-')
setwd(final_result)
QC_1 <- data.frame("Results" = 'Prediction' ,"Min Date" = min(df5$Dte), "Max Date" = max(df5$Dte))
QC <- rbind(QC,QC_1)

#### Creating the Final File

Lookup_1 <- unique(ZSS_Dump[,c("MATERIAL.NO","MATERIAL.DESCRI", "SEC1.MAX", "SEC2.MAX")])
Lookup_1 <- Lookup_1[which(!is.na(Lookup_1$SEC2.MAX)),]

Lookup_1 <- Lookup_1 %>%
  group_by(MATERIAL.NO) %>% 
  slice(which.max(length(MATERIAL.DESCRI)))

dataset_final <- dataset_final %>% separate(Dist_MaterialCode, into = c("Distibutor_Code", "Material_Code"))
dataset_final2 <- merge(dataset_final, Lookup_1,
                        by.x = c("Material_Code"), by.y =  c("MATERIAL.NO"),
                        all.x = T)

WhishList_2 <- unique(WhishList[,c("Zone","Dist_code", "Customer.Name")])

dataset_final3 <- merge(dataset_final2, WhishList_2,
                        by.x = c("Distibutor_Code"), by.y =  c("Dist_code"),
                        all.x = T)
dataset_final4 <- dataset_final3[,c("Zone", "Distibutor_Code", "Customer.Name" , "Material_Code" ,
                                    "MATERIAL.DESCRI", "SEC1.MAX", "SEC2.MAX",format(as.Date(pred_month_data , "%Y-%m-%d") %m+% months(2), "%b-%Y"),
                                    format(as.Date(pred_month_data , "%Y-%m-%d") %m+% months(3), "%b-%Y"), format(as.Date(pred_month_data , "%Y-%m-%d") %m+% months(4), "%b-%Y") )]
setnames(dataset_final4, c("Distibutor_Code", "Customer.Name" , "Material_Code" ,"MATERIAL.DESCRI", "SEC1.MAX", "SEC2.MAX"), 
                           c("Distibutor Code", "Distibutor Name" , "Material Code" ,"Material Description", "Thickness", "Width"))  
  
write.csv(dataset_final4,paste0("Pred_SaleMT_",t,".csv"), row.names = F)
