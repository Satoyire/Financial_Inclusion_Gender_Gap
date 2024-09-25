library(plotly)
library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(maps)
library(gridExtra)
library(GGally)


# Create new variable names for certain columns
New_Col_Names <- c("Ctry_Name", "Ctry_code", "Year", "Adt_Pop", "Region", "Inc_Grp", "No", "Acc", "No", "FI_Acc", "No", "FI_Borrowed", "No", "F_Save", "No", "M_Save", "No", "F_Loan", "No", "M_Loan", "No", "F_Acc", "No", "M_Acc")

# Load the Findex data set in R and apply the new variable names and convert some variables to numeric 
Findexdata <- read_excel("C:\\Users\\satoy\\Desktop\\Project\\Data\\FIGG.xlsx",
                         col_names = New_Col_Names) %>% 
  select(Ctry_Name, Ctry_code, Year, FI_Acc, F_Acc, M_Acc, F_Save, M_Save, F_Loan, M_Loan) %>% 
  mutate(across(c(FI_Acc, F_Acc, M_Acc, F_Save, M_Save, F_Loan, M_Loan),
                ~ as.numeric(gsub("[^0-9.]", "", .))))  
FI_data <- Findexdata[-1,] %>%
  mutate(Year = as.numeric(Year))



# Load the WDI data and do some cleaning
WDIdata <- read_excel("C:\\Users\\satoy\\Desktop\\Project\\Data\\New\\World_Development_Indicators_SSA.xlsx") %>% 
  select("Country Name", "Series Name", "2011", "2014", "2017", "2021") %>% 
  rename(Ctry_Name="Country Name", Srs_Name="Series Name") %>% 
  pivot_longer(cols = c("2011", "2014", "2017", "2021"),
               names_to = "Year",
               values_to = "Value")%>% 
  select(Ctry_Name, Year, Value, Srs_Name) %>% 
  pivot_wider(names_from = Srs_Name,
              values_from = Value)




# Remove some rows and columns and change specific variables to numeric
WDI_data <- WDIdata[-c(1:4), -c(3:5, 10:13, 20)] %>% 
  mutate(across(c(Elec_Acc, Internet_Access, Mobile_Telephony, "GDP (current US$)", 
                  Rur_Pop, Adt_F_Pop, Adt_M_Pop, Fem_Pop, Male_Pop),   
                ~ as.numeric(gsub("[^0-9.]", "", .)))) %>% 
  rename(GDP = "GDP (current US$)") %>% 
  mutate(Year = as.numeric(Year))


# Load the Economic freedom data and select variables and years of interest
Ecofree <- read_csv("C:\\Users\\satoy\\Desktop\\Project\\Data\\New\\economicdata2011-2021.csv")%>% 
  rename(Ctry_Name=Countries, Ctry_code="ISO Code", EF_Index= "Economic Freedom Summary Index") %>% 
  select(Year, Ctry_Name, Ctry_code, EF_Index) %>% 
  mutate(EF_Index=EF_Index*10) %>% 
  filter(Year>=2011) #%>%


#Load Employment Data
Employment <- read_excel("C:\\Users\\satoy\\Desktop\\Project\\Data\\New\\Employment Data.xlsx") %>% 
  select("Country Name", "Series Name", "2011", "2014", "2017", "2021") %>% 
  rename(Ctry_Name="Country Name", Srs_Name="Series Name") %>% 
  pivot_longer(cols = c("2011", "2014", "2017", "2021"),
               names_to = "Year",
               values_to = "Value")%>% 
  select(Ctry_Name, Year, Value, Srs_Name) %>% 
  pivot_wider(names_from = Srs_Name,
              values_from = Value) 

Employment <- Employment[, -6] %>% 
  mutate(across(c(M_Emp_Rate, F_Emp_Rate, T_Emp_Rate),   
                ~ as.numeric(gsub("[^0-9.]", "", .)))) %>%
  mutate(Year = as.numeric(Year))



# Join the four data sets created above into a single data set 
FIGG_Data <- FI_data %>%
  inner_join(WDI_data, by = c("Ctry_Name", "Year")) %>%
  left_join(Ecofree, by = c("Ctry_Name" = "Ctry_Name", "Ctry_code" = "Ctry_code", "Year" = "Year")) %>%
  select(-20) %>%
  mutate(
    Acc_G_Gap = round((M_Acc - F_Acc)/(M_Acc + F_Acc), 4),
    Sav_G_Gap = round((M_Save - F_Save)/(M_Save + F_Save), 4),
    Loan_G_Gap = round((M_Loan - F_Loan)/(M_Loan + F_Loan), 4),
    Adt_F_Pop = round(Adt_F_Pop, 2),
    Adt_M_Pop = round(Adt_M_Pop, 2)) %>%  left_join(Employment, by = c("Ctry_Name", "Year")) %>% 
  mutate( Emp_GGap = round((M_Emp_Rate-F_Emp_Rate)/T_Emp_Rate, 4)) %>% mutate(fint=(0.516*Internet_Access)+(0.258*Elec_Acc)+(0.226*Mobile_Telephony))
view(FIGG_Data)  

# Compute the average values for each variable in each year
Yearly_FIGG <- FIGG_Data %>%
  group_by(Year) %>% 
  summarise(avg_F_Acc = mean(F_Acc), 
            avg_M_Acc = mean(M_Acc), 
            avg_F_Save = mean(F_Save), 
            avg_M_Save = mean(M_Save), 
            avg_F_Loan = mean(F_Loan), 
            avg_M_Loan = mean(M_Loan), 
            avg_int_Acc=mean(Internet_Access, na.rm = TRUE),
            avg_MTel=mean(Mobile_Telephony, na.rm = TRUE), 
            avg_E_Acc=mean(Elec_Acc, na.rm = TRUE), 
            avg_EF_Index=mean(EF_Index, na.rm = TRUE), 
            avg_Rur_Pop=mean(Rur_Pop, na.rm = TRUE), 
            avg_F_Emp=mean(F_Emp_Rate, na.rm = TRUE), 
            avg_M_Emp=mean(M_Emp_Rate, na.rm = TRUE), 
            avg_GDP=mean(GDP/1000000000, na.rm = TRUE),
            med_GDP=median(GDP/1000000000, na.rm = TRUE),
            .groups = "drop")



#Compute the average values for each variable in each country
Avg_FIGG <- FIGG_Data %>%
  mutate(Ctry_code = ifelse(Ctry_code == "SSD", "SDN", Ctry_code)) %>%
  group_by(Ctry_Name, Ctry_code) %>% 
  summarise(
    avg_F_Acc = mean(F_Acc), 
    avg_M_Acc = mean(M_Acc), 
    Acc_GGap=mean(Acc_G_Gap, 2), 
    avg_F_Save = mean(F_Save), 
    avg_M_Save = mean(M_Save), 
    Sav_GGap=mean(Sav_G_Gap, 2), 
    avg_F_Loan = mean(F_Loan), 
    Loan_GGap=mean(Loan_G_Gap, 2), 
    avg_M_Loan = mean(M_Loan), 
    avg_int_Acc=mean(Internet_Access, na.rm = TRUE), 
    avg_MTel=mean(Mobile_Telephony, na.rm = TRUE), 
    avg_E_Acc=mean(Elec_Acc, na.rm = TRUE), 
    avg_EF_Index=mean(EF_Index, na.rm = TRUE),
    avg_Emp_GGap=mean(Emp_GGap, na.rm = TRUE),
    .groups = "drop") 


#Compute the composite(overall) Fintech variable
Avg_FIGG <- Avg_FIGG %>% mutate(avg_fint=(0.516*avg_int_Acc)+(0.258*avg_E_Acc)+(0.226*avg_MTel))

save(Avg_FIGG, Yearly_FIGG, FIGG_Data, Employment, Ecofree, FIGG_SSA, My_labels, 
     file="GradProject.RData")