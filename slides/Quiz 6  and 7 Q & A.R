library(tidyverse)
 library(janitor)
# MERGE and making smoking variable 

clinical <- read.csv(file = "F:\\myGitRepo\\Introduction-to-R\\data\\tcga-clinical.csv", header = TRUE)
geneexp <- read.csv(file = "F:\\myGitRepo\\Introduction-to-R\\data\\tcga-gene-exp.csv", header = TRUE)
 
  tcga <- left_join(clinical, geneexp, by = "bcr_patient_barcode")
  dim(tcga)
  
  names(tcga)
  
  
  # LOAD clinical data ####
  load("F:\\myGitRepo\\Introduction-to-R\\data\\clinical_survival_pancancer_atlas.RData")
  dim(tcga)
  tcga <- left_join(tcga, dat[,c("bcr_patient_barcode","OS.time")], by = "bcr_patient_barcode")
  dim(tcga)
  names(tcga)
  
  
  
  
 # Quiz 6 Q & A ####
 names(tcga)

  tcga %>% correlation::cor_test("SLC12A8_exp", "HRCT1_exp", method = c("pearson") ) 

  tcga %>% correlation::cor_test("SLC12A8_exp",  "HRCT1_exp", method = c("spearman") ) 

  
  
  
 
 x11();
 plot(tcga$DTL_exp,tcga$BUB1_exp )
 summary( lmfit <-  lm(BUB1_exp ~ DTL_exp, data = tcga) )  
 abline(lmfit)
 
  
  
 # Quiz 7 Q & A #####
  
  library(survival)
  library(ggplot2)
  library(survminer)
  
  tcga %>% tabyl( radiation_therapy, gender , show_na = FALSE ) %>%
    chisq.test() 
    
  
  OS_obj <- survival::Surv(tcga$OS.time, I(tcga$vital_status=="Dead")) 
  kmfit <-  survfit(OS_obj ~ gender, data = tcga)
 
  survdiff(formula = OS_obj ~ gender, data = tcga)
 

  x11()
    ggsurvplot(kmfit, data = tcga,
                   # Add median survival lines
                   surv.median.line = "hv", 
                   # Change legends: title & labels
                   title= "Overall Survival by   gender", 
                   xlab ="Years", 
                   legend.title = "",
                   legend.labs = c("M","F"),
                   # Add p-value and CIs
                   pval = TRUE,
                   conf.int = FALSE,
                   # Add risk table
                   risk.table = TRUE,
                   tables.height = 0.2,
                   tables.theme = theme_cleantable(),
                   # Color palettes.
                   palette = c("#F8766D","#00BA38" ),  
                   # Change ggplot2 theme
                   ggtheme = theme_bw(), 
                   risk.table.title = "Number at risk")
    
    