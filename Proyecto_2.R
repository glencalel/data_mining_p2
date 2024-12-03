# Instalar librerias
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")


# LOAD LIBRARIES
library(haven)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)

# LOAD ALL DATA BY YEAR
data_2022 <- read_sav('C:\\files\\2022.sav')
data_2021 <- read_sav('C:\\files\\2021.sav')
data_2020 <- read_sav('C:\\files\\2020.sav')
data_2019 <- read_sav('C:\\files\\2019.sav')
data_2018 <- read_sav('C:\\files\\2018.sav')
data_2017 <- read_sav('C:\\files\\2017.sav')
data_2016 <- read_sav('C:\\files\\2016.sav')
data_2015 <- read_sav('C:\\files\\2015.sav')
data_2014 <- read_sav('C:\\files\\2014.sav')
data_2013 <- read_sav('C:\\files\\2013.sav')
data_2012 <- read_sav('C:\\files\\2012.sav')
data_2011 <- read_sav('C:\\files\\2011.sav')
data_2010 <- read_sav('C:\\files\\2010.sav')
data_2009 <- read_sav('C:\\files\\2009.sav')


# SOLVE CONFLICTS WITH LABELS ON COLUMNS: PPERTENENCIA, DEPTORESIDEN, MUNIRESIDEN
data_2019$PPERTENENCIA <- labelled(data_2019$PPERTENENCIA, labels = attr(data_2022$PPERTENENCIA, "labels"))
data_2019$DEPTORESIDEN <- labelled(data_2019$DEPTORESIDEN, labels = attr(data_2022$DEPTORESIDEN, "labels"))
data_2019$MUNIRESIDEN <- labelled(data_2019$MUNIRESIDEN, labels = attr(data_2022$MUNIRESIDEN, "labels"))

data_2018$PPERTENENCIA <- labelled(data_2018$PPERTENENCIA, labels = attr(data_2022$PPERTENENCIA, "labels"))
data_2018$DEPTORESIDEN <- labelled(data_2018$DEPTORESIDEN, labels = attr(data_2022$DEPTORESIDEN, "labels"))
data_2018$MUNIRESIDEN <- labelled(data_2018$MUNIRESIDEN, labels = attr(data_2022$MUNIRESIDEN, "labels"))

names(data_2017)[names(data_2017) == "GRUPETNICO"] <- "PPERTENENCIA"

data_2017$PPERTENENCIA <- labelled(data_2017$PPERTENENCIA, labels = attr(data_2022$PPERTENENCIA, "labels"))
data_2017$DEPTORESIDEN <- labelled(data_2017$DEPTORESIDEN, labels = attr(data_2022$DEPTORESIDEN, "labels"))
data_2017$MUNIRESIDEN <- labelled(data_2017$MUNIRESIDEN, labels = attr(data_2022$MUNIRESIDEN, "labels"))

names(data_2016)[names(data_2016) == "GRUPETNICO"] <- "PPERTENENCIA"

data_2016$PPERTENENCIA <- labelled(data_2016$PPERTENENCIA, labels = attr(data_2022$PPERTENENCIA, "labels"))
data_2016$DEPTORESIDEN <- labelled(data_2016$DEPTORESIDEN, labels = attr(data_2022$DEPTORESIDEN, "labels"))
data_2016$MUNIRESIDEN <- labelled(data_2016$MUNIRESIDEN, labels = attr(data_2022$MUNIRESIDEN, "labels"))

names(data_2015)[names(data_2015) == "GRUPETNICO"] <- "PPERTENENCIA"

data_2015$PPERTENENCIA <- labelled(data_2015$PPERTENENCIA, labels = attr(data_2022$PPERTENENCIA, "labels"))
data_2015$DEPTORESIDEN <- labelled(data_2015$DEPTORESIDEN, labels = attr(data_2022$DEPTORESIDEN, "labels"))
data_2015$MUNIRESIDEN <- labelled(data_2015$MUNIRESIDEN, labels = attr(data_2022$MUNIRESIDEN, "labels"))


data_2022_2015 <- bind_rows(data_2022, data_2021, data_2020, data_2019, data_2018, data_2017, data_2016, data_2015)
data_2014_2009 <- bind_rows(data_2014, data_2013, data_2012, data_2011, data_2010, data_2009)


# DECISION TREE - NO.1
data_dt_1 <- subset(data_2022_2015, AÑO %in% c(2020,2021,2022))
data_dt_1 <- subset(data_dt_1, CAUFIN %in% c('C910'))

tree1 <- rpart(SEXO ~ 
                 AÑO+
                 DEPTORESIDEN+
                 EDAD+
                 PPERTENENCIA+
                 PERIODOEDA+
                 TC,
               data = data_dt_1,
               method = "class")

rpart.plot(tree1, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de area", cex = 1)

dataPrediction1 <- data.frame(
  AÑO=c(2021),
  DEPTORESIDEN=c(1),
  EDAD=c(16),
  PPERTENENCIA=c(4),
  PERIODOEDA=c(3),
  TC=c(2)
)

result1 <- predict(tree1, dataPrediction1, type="class")

result1

# DECISION TREE - NO.2
data_dt_2 <- subset(data_2022_2015, DEPTORESIDEN %in% c(9, 7, 13, 12, 14, 8))
data_dt_2 <- subset(data_dt_2, AÑO %in% c(2020,2021,2022))
data_dt_2 <- subset(data_dt_2, CAUFIN %in% c('Z33X'))
data_dt_2 <- subset(data_dt_2, SEXO %in% c(2))

tree2 <- rpart(MUNIRESIDEN ~ 
                 EDAD+
                 PPERTENENCIA+
                 PERIODOEDA+
                 DEPTORESIDEN+
                 TC,
               data = data_dt_2,
               method = "class")

rpart.plot(tree2, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de area", cex = 0.75)

dataPrediction2 <- data.frame(
  EDAD=c(10),
  PPERTENENCIA=c(4),
  PERIODOEDA=c(3),
  DEPTORESIDEN=c(1)
)

result2 <- predict(tree2, dataPrediction2, type="class")

result2


# DECISION TREE - NO.3
data_dt_3 <- subset(data_2022_2015, CAUFIN %in% c("A30","A300","A301","A302","A303","A304","A305","A308","A309"))

tree3 <- rpart(CAUFIN ~ 
                 EDAD+
                 SEXO+
                 PPERTENENCIA+
                 PERIODOEDA+
                 DEPTORESIDEN,
               data = data_dt_3,
               method = "class")

rpart.plot(tree3, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de sexo", cex = 0.5)


dataPrediction3 <- data.frame(
  EDAD=c(10),
  SEXO=c(1),
  PPERTENENCIA=c(4),
  PERIODOEDA=c(3),
  DEPTORESIDEN=c(1)
)

result3 <- predict(tree3, dataPrediction3, type="class")

result3


# DECISION TREE - NO.4
data_dt_4 <- subset(data_2022_2015, CAUFIN %in% c("G470", "F510"))

rm(tree4)

tree4 <- rpart(CAUFIN ~ 
                 EDAD+
                 SEXO+
                 MUNIRESIDEN,
               data = data_dt_4,
               method = "class")

rpart.plot(tree4, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de transtorno de sueño", cex = 0.75)


dataPrediction4 <- data.frame(
  EDAD=c(4),
  SEXO=c(3),
  DEPTORESIDEN=c(1),
  MUNIRESIDEN=c(0101)
)

result4 <- predict(tree4, dataPrediction4, type="class")

result4


# RANDOM FOREST - NO.1
data_rf_1 <- subset(data_2022_2015, TC %in% c(3,4))
data_rf_1 <- subset(data_rf_1, MUNIRESIDEN = 0101)
data_rf_1 <- subset(data_rf_1, CAUFIN %in% c('I10X'))
data_rf_1 <- data_rf_1[, !(names(data_rf_1) %in% c("DEPTORESIDEN"))]
data_rf_1 <- data_rf_1[, !(names(data_rf_1) %in% c("MUNIRESIDEN"))]



























