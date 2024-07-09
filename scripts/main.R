# setwd(r"(C:\working\projects\2022\master_student\scripts)")

DIR_SCRIPTS  <- getwd()
DIR_PROJECT  <- dirname(DIR_SCRIPTS)
DIR_DATASET  <- file.path(DIR_PROJECT, 'dataset')
DIR_ANALYSIS <- file.path(DIR_PROJECT, 'analysis')

FILE_DATASET <- file.path(DIR_DATASET, 'dataset_d.xlsx')

library(tidyverse)
library(survival)

source('preprocess_split_df.R')
source('feature-cleaning.R')
source('outcome-cleaning.R')
source('process_outcomes.R')

if (!dir.exists(DIR_ANALYSIS)) {
    
    dir.create(DIR_ANALYSIS)
}

setwd(DIR_ANALYSIS)

if (!dir.exists('./preprocessed-data')) {
    
    dir.create('./preprocessed-data')
}


BuildModel <- function() {

features <- read_csv("./preprocessed-data/feature-data_cleaned.csv")
outcome_surv <- read_csv('./preprocessed-data/Amen-outcome-data_surv.csv')

complete_df <- merge(x=features, y=outcome_surv, by="ID")
complete_df <- complete_df[, -c(1)]
factor_df <- data.frame(complete_df)
factor_df[, c(2:5)] = lapply(factor_df[, c(2:5)], factor)

model0 <- coxph(Surv(time, survive) ~ ., 
                data=factor_df)
anova(model0)
plot(survfit(model0), xlab="Months", ylab="Proportion of recovery")
step(model0)

cycles_numeric_df <- data.frame(complete_df)
cycles_numeric_df[, c(2:4)] = lapply(cycles_numeric_df[, c(2:4)], factor)
model1 <- coxph(Surv(time, survive) ~ ., 
                cycles_numeric_df)
anova(model1)
step(model1)

model2 <- coxph(Surv(time, survive) ~ Age_diagnosis + Invasiveness_flg +
                  ER_Status + CMF_Cycles,
                  data = cycles_numeric_df)
anova(model2)
step(model2)
}


Run <- function() {
    
    getPreprocessedData(FILE_DATASET)
    
    cleanFeatureData("./preprocessed-data/feature-data.csv")
    
    cleanOutcomeData('./preprocessed-data/Amen-outcome-data.csv')
    
    BuildModel()    
}


AutoBackupScripts <- function() {
    
    return(invisible(NULL))
}


BackupResults <- function() {

}


AutoBackupScripts()
