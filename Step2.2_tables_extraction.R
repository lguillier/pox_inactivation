## This R script enables to extract raw data gathered in the Excel raw data spreadsheet

#### Extract raw data from each study #####
Table_Bronson1943 <- read_excel(RawData_FileName, sheet = "Table_Bronson1943_1")
Table_deOliveira2010 <- read_excel(RawData_FileName, sheet = "Fig_deOliveira2010_3") 

Table_Elzein1983_2 <- read_excel(RawData_FileName, sheet = "Table_Elzein1983_2")
Table_Elzein1983_3 <- read_excel(RawData_FileName, sheet = "Table_Elzein1983_3")



#### Combining data from all tables in a unique data frame ####
# dt: (data frame) with 4 columns: Kinetic_key, time_hours, y_variable (name of y) and y (measured values)
dt <- data.frame(Kinetic_key = c(Table_Bronson1943$Kinetic_key,
                                 Table_deOliveira2010$Kinetic_key,
                                 Table_Elzein1983_2$Kinetic_key,
                                 Table_Elzein1983_3$Kinetic_key),
                 time_min = c(Table_Bronson1943$time_min,
                              Table_deOliveira2010$time_min,
                              Table_Elzein1983_2$time_min,
                              Table_Elzein1983_3$time_min),
                 y_variable = c(rep("logTCID50", nrow(Table_Bronson1943)),
                                rep("logPFU", nrow(Table_deOliveira2010)),
                                rep("logPFU", nrow(Table_Elzein1983_2)),
                                rep("logPFU", nrow(Table_Elzein1983_3))),
                 y = c(Table_Bronson1943$logTCID50,
                       Table_deOliveira2010$logPFU,
                       Table_Elzein1983_2$logPFU,
                       Table_Elzein1983_3$logPFU)
)

# Clean the environment by remove intermediate tables
rm(Table_Bronson1943,
   Table_deOliveira2010,
   Table_Elzein1983_2,
   Table_Elzein1983_3)
