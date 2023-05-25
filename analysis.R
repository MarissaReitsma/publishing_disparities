rm(list = ls())

library(data.table)
library(gender)
library(splitstackshape)
library(readxl)

setwd("./publishing_disparities/")

files <- list.files(paste0("Top Journals Data/"))

## Read in Data
pubs_metrics_top <- NULL
for (f in files) {
  temp <- as.data.table(read_xlsx(paste0("Top Journals Data/", f)))
  pubs_metrics_top <- rbind(pubs_metrics_top, temp, fill = T)
}

## Extract First Names and Authorship Order
pubs_long <-  cSplit(pubs_metrics_top, "Authors", ", ", 'long')
pubs_long <- pubs_long[!is.na(Authors)]
pubs_long <- pubs_long[, n_times:=.N, by = "Authors"]
pubs_long <- pubs_long[n_times < 100]
pubs_long <-  cSplit(pubs_long, "Authors", " ", 'wide')

pubs_long <- pubs_long[, auth_order:=seq_len(.N), by = "PMID"]
pubs_long <- pubs_long[, max_auth:=max(auth_order), by = "PMID"]
pubs_long <- pubs_long[auth_order==1 | auth_order==max_auth]

## Predict Gender Based on First Name
preds <- gender(unique(pubs_long$Authors_01))
pubs_long <- merge(pubs_long, preds, by.x = "Authors_01", by.y = "name")

## Analyze

  # First author
  table(pubs_long$gender[pubs_long$auth_order==1 & pubs_long$max_auth>1 & pubs_long$Journal=="J Gen Intern Med"])/nrow(pubs_long[Journal=="J Gen Intern Med" & max_auth>1 & auth_order==1])
  table(pubs_long$gender[pubs_long$auth_order==1 & pubs_long$max_auth>1 & pubs_long$Journal=="Ann Intern Med"])/nrow(pubs_long[Journal=="Ann Intern Med" & max_auth>1 & auth_order==1])
  table(pubs_long$gender[pubs_long$auth_order==1 & pubs_long$max_auth>1 & pubs_long$Journal=="JAMA Netw Open"])/nrow(pubs_long[Journal=="JAMA Netw Open" & max_auth>1 & auth_order==1])
  table(pubs_long$gender[pubs_long$auth_order==1 & pubs_long$max_auth>1 & pubs_long$Journal=="JAMA"])/nrow(pubs_long[Journal=="JAMA" & max_auth>1 & auth_order==1])
  table(pubs_long$gender[pubs_long$auth_order==1 & pubs_long$max_auth>1 & pubs_long$Journal=="Nature"])/nrow(pubs_long[Journal=="Nature" & max_auth>1 & auth_order==1])
  table(pubs_long$gender[pubs_long$auth_order==1 & pubs_long$max_auth>1 & pubs_long$Journal=="N Engl J Med"])/nrow(pubs_long[Journal=="N Engl J Med" & max_auth>1 & auth_order==1])

  # Senior author
  table(pubs_long$gender[pubs_long$auth_order==pubs_long$max_auth & pubs_long$max_auth>1 & pubs_long$Journal=="J Gen Intern Med"])/nrow(pubs_long[Journal=="J Gen Intern Med" & max_auth>1 & auth_order==max_auth])
  table(pubs_long$gender[pubs_long$auth_order==pubs_long$max_auth & pubs_long$max_auth>1 & pubs_long$Journal=="Ann Intern Med"])/nrow(pubs_long[Journal=="Ann Intern Med" & max_auth>1 & auth_order==max_auth])
  table(pubs_long$gender[pubs_long$auth_order==pubs_long$max_auth & pubs_long$max_auth>1 & pubs_long$Journal=="JAMA Netw Open"])/nrow(pubs_long[Journal=="JAMA Netw Open" & max_auth>1 & auth_order==max_auth])
  table(pubs_long$gender[pubs_long$auth_order==pubs_long$max_auth & pubs_long$max_auth>1 & pubs_long$Journal=="JAMA"])/nrow(pubs_long[Journal=="JAMA" & max_auth>1 & auth_order==max_auth])
  table(pubs_long$gender[pubs_long$auth_order==pubs_long$max_auth & pubs_long$max_auth>1 & pubs_long$Journal=="Nature"])/nrow(pubs_long[Journal=="Nature" & max_auth>1 & auth_order==max_auth])
  table(pubs_long$gender[pubs_long$auth_order==pubs_long$max_auth & pubs_long$max_auth>1 & pubs_long$Journal=="N Engl J Med"])/nrow(pubs_long[Journal=="N Engl J Med" & max_auth>1 & auth_order==max_auth])

  # Analyze first-last combos
  first_last <- pubs_long[max_auth>1]
  first_last <- first_last[,.(Year, PMID, Journal, RCR, `Citations Per Year`, gender, auth_order)]
  first_last <- first_last[auth_order == 1, position := "First"]
  first_last <- first_last[auth_order != 1, position := "Last"]
  first_last <- first_last[,.(Year, PMID, Journal, RCR, `Citations Per Year`, gender, position)]

  first_last <- dcast(first_last, PMID + Year + Journal + RCR + `Citations Per Year` ~ position, value.var = "gender")
  first_last <- first_last[!is.na(First) & !is.na(Last)]

  for (journal in unique(first_last$Journal)) {
    message(paste0(journal))
    print(table(first_last$First[first_last$Journal==journal], first_last$Last[first_last$Journal==journal]))
  }

  for (journal in unique(first_last$Journal)) {
    for (first in c("male", "female")) {
      for (last in c("male", "female")) {
        message(paste0(journal), ": First = ", first, "; Last = ", last)
        print(round(mean(first_last$`Citations Per Year`[first_last$First==first & first_last$Last==last & first_last$Journal==journal], na.rm=T), 2))
      }
    }
  }

  for (journal in unique(first_last$Journal)) {
    for (last in c("male", "female")) {
      message(paste0(journal), ": Last = ", last)
      print(round(mean(first_last$`Citations Per Year`[first_last$Last==last & first_last$Journal==journal], na.rm=T), 2))
      
    }
  }
  
  for (journal in unique(first_last$Journal)) {
    message(paste0(journal))
    print(round((mean(first_last$`Citations Per Year`[first_last$Last=="male" & first_last$Journal==journal], na.rm=T) - 
                   mean(first_last$`Citations Per Year`[first_last$Last=="female" & first_last$Journal==journal], na.rm=T))/
                  mean(first_last$`Citations Per Year`[first_last$Last=="female" & first_last$Journal==journal], na.rm=T), 2))
  }
