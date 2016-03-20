#清除變數
rm(list=ls(all=TRUE));

#讀入library
library(dplyr);
library(ggplot2);

#設定工作目錄
setwd('/Volumes/AhaStorage/A_Project/23_R_SWIRL/');

#讀入資料
Rawdata_HK = read.csv(stringsAsFactors=F,file='./HK/data.csv',skip=3);
Rawdata_Singapore = read.csv(stringsAsFactors=F,file='./Singapore/data.csv',skip=3);
Rawdata_Korea = read.csv(stringsAsFactors=F,file='./Korea/data.csv',skip=3);
Rawdata_Taiwan = read.csv(stringsAsFactors=F,file='./Taiwan.csv',skip=2);

#取出每個資料的人口, GDP(美元), 人均GDP, 年 - 南韓, 新加坡, 香港
extractCol <- function(df,year,colname){
  cols_name = paste("X",year,sep="");
  A = unlist(subset(df,select=cols_name,subset=c(Indicator.Name == colname)));
  names(A) = NULL;
  return(A);
}

extractData <- function(df){
  year = c(1960:2015);
  GDP = extractCol(df,year,"GDP at market prices (current US$)");
  GDP_GROWTH = extractCol(df,year,"GDP growth (annual %)");
  GDP_PER_CAP = extractCol(df,year,"GDP per capita (current US$)");
  GDP_PER_CAP_GROWTH = extractCol(df,year,"GDP per capita growth (annual %)");
  POP_GROWTH = extractCol(df,year,"Population growth (annual %)");
  POP = extractCol(df,year,"Population, total");
  ans = data.frame(year,GDP,GDP_GROWTH,GDP_PER_CAP,GDP_PER_CAP_GROWTH,POP_GROWTH,POP);
  return(ans);
}

Data_HK = extractData(Rawdata_HK)
Data_Singapore = extractData(Rawdata_Singapore)
Data_Korea = extractData(Rawdata_Korea)

#取出每個資料的人口, GDP(美元), 人均GDP, 年 - 台灣
