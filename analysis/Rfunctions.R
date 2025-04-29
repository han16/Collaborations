rm(list=ls())
library(tidyverse)
library(kableExtra)
library(ggpubr)
library(rstatix) # use anova_test function 
library(grid)
library(DT)
library(gridExtra)
library(matrixStats)
library(cowplot)
library(readxl)
library(irr)

library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(ggplot2)

#install.packages("devtools")
library(devtools)
# devtools::install_github("zabore/condsurv")
library(condsurv)

library("VennDiagram") 

set.seed(123)


######## adjust p values 
pvalue_adjust=function(p_value)
{
  p_value_adjust=numeric()
  for (i in 1:length(p_value))
  {
    if (is.na(p_value[i])==T)
      p_value_adjust[i]=p_value[i]
    if (is.na(p_value[i])==F & p_value[i]<0.0001)
      p_value_adjust[i]="<0.0001"
    if (is.na(p_value[i])==F & p_value[i]>0.0001)
      p_value_adjust[i]=round(p_value[i],4)
  }
  return(p_value_adjust)
}
############### summary statistics for continuous variable 
summary_statistics=function(data)
{  # each column is one variable, and row one subject 
  
  variables=colnames(data)
  num_sample=numeric()
  Mean=numeric()
  Median=numeric()
  SD=numeric()
  SE=numeric()
  range_min=numeric()
  range_max=numeric()
  normality_pvalue=numeric()
  per_25_quantile=numeric()
  per_75_quantile=numeric()
  
  for (i in 1:ncol(data))
  {
    observation=as.numeric(data[,i])
    observation=observation[!is.na(observation)] # remove missing values 
    num_sample[i]=length(observation)
    if (num_sample[i]>0)
    {
      Mean[i]=round(mean(observation),4)
      Median[i]=round(median(observation),4)
      SD[i]=round(sd(observation),4)
      SE[i]=round(SD[i]/sqrt(num_sample[i]),4)
      per_25_quantile[i]=round(quantile(observation, prob=0.25),4)
      per_75_quantile[i]=round(quantile(observation, prob=0.75),4)
      range_min[i]=round(min(observation),4)
      range_max[i]=round(max(observation),4)
      if (length(observation)<=3 | var(observation)==0)
        normality_pvalue[i]="NA: sample size too small or no variantion"
       if (length(observation)>3 & var(observation)>0)
      normality_pvalue[i]=round(shapiro.test(observation)$p.value,4)
    }
    if (num_sample[i]==0)
    {
      Mean[i]=NA
      Median[i]=NA
      SD[i]=NA
      SE[i]=NA
      per_25_quantile[i]=NA
      per_75_quantile[i]=NA
      range_min[i]=NA
      range_max[i]=NA
      normality_pvalue[i]=NA
    }
    
  }
  
  summary_data=data.frame(variable=variables, num_sample=num_sample, Mean=Mean, Median=Median, SD=SD, SE=SE,per_25_quantile, per_75_quantile, Range_low=range_min, Range_upper=range_max, normality_pvalue=normality_pvalue)
  
  summary_data%>%
    datatable(extensions = 'Buttons',
              options = list(dom = 'Blfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             lengthMenu = list(c(10,25,50,-1),
                                               c(10,25,50,"All"))))
  
  return(summary_data) 
}

multiplesheets <- function(fname) {
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  # print data frame
  print(data_frame)
}

discrete_summary=function(data)
{
  res=data.frame(var=data) %>% dplyr::count(var) %>% mutate(prop=round(n/(sum(n)),4))
  return(res)
}  
  

multiplesheets_without_headers <- function(fname) {
  
  # Get info about all Excel sheets
  sheets <- readxl::excel_sheets(fname)
  
  # Read each sheet without headers
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x, col_names = FALSE))
  
  # Convert to data frames
  data_frame <- lapply(tibble, as.data.frame)
  
  # Assign names to data frames
  names(data_frame) <- sheets
  
  # Print data frames
  print(data_frame)
}

