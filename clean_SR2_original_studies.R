


#load packages----
library(ASySD)
library(tidyverse)
library(stringr)

# add evaluation name here
evaluation_name <- "SR2_elicit"

#original review: DOI 10.11124/JBIES-23-00291


#1. load files----

#1.1 original review -----
#bibliography from original study. original study did not provide details on studies excluded at TiAb screening. 
#For studies included at TiAb screening, 316 excluded at FT screening, label 'original_ft_exclude'. from supplement digital content 1
#of studies advanced from above, 118 excluded following critical appraisal, e.g. using quadas; label 'original_ca_exclude'. from supplemental digital content 2. 
# original prisma states 143 included at FT but there is one duplicate in their included -
#71. Garcia-Cardenas F, Franco A, Cortés R, Bertin J, Valdéz R, Peñaloza F, et al. Analytical performances of the COVISTIX™ and Panbio™ antigen rapid tests for SARS-CoV-2 detection in an unselected population (all comers). medRxiv 2021.
#72. Garcia-Cardenas F, Peñaloza F, Bertin-Montoya J, Valdéz-Vázquez R, Franco A, Cortés R, et al. Analytical performances of the COVISTIX™ antigen rapid test for SARS-CoV-2 detection in an unselected population (all-comers). Pathogens 2022;11(6):628.

original_search <- read.csv('data/SR2/original/SR2_all original.csv')
original_search <- original_search %>%
  mutate(doi_clean = str_extract(doi, "10\\.\\d{4,9}/[^\\s]+"))%>%
  mutate(doi_clean = sub('\\.$', '', doi_clean))%>%
  mutate(doi_clean = sub('\\;$', '', doi_clean))%>%
  mutate(record_id = paste0("sr2_original_", row_number()))

#check original for duplicate dois

originaldoiduplicates <-original_search%>%
  filter(!is.na(doi_clean))%>%
  group_by(doi_clean)%>%
  summarise(n = length(record_id))%>%
  filter(n>1)

originalduplicates <-original_search%>%
  filter(doi_clean %in% originaldoiduplicates$doi_clean)%>%
  arrange(doi_clean)

write.csv(originalduplicates, 'output/SR2/original/original_duplicates.csv', row.names=FALSE)

#manual review dois and correct any errors

#correct wrong dois within original review
original_search[original_search$record_id =='sr2_original_343',]$doi_clean = '10.3346/jkms.2021.36.e101'
original_search[original_search$record_id =='sr2_original_379',]$doi_clean = '10.4038/sljid.v11i1.8356'
original_search[original_search$record_id =='sr2_original_341',]$doi_clean = '10.1016/j.ijid.2020.10.073'
original_search[original_search$record_id =='sr2_original_330',]$doi_clean = '10.1016/j.jcvp.2021.100011'
original_search[original_search$record_id =='sr2_original_262',]$doi_clean = '10.1016/j.enfcli.2020.10.001'
originalduplicate_record_id_toremove <- c('sr2_original_361', #listed in table of excluded studies following critical appraisal and included studies. keep label for included studies
                                          
                                          #below have duplicate dois with both excluded -i.e. original_ft_exclude; remove one
                                          'sr2_original_192',
                                          'sr2_original_111',
                                          'sr2_original_164',
                                          'sr2_original_147',
                                          'sr2_original_191',
                                          'sr2_original_253',
                                          'sr2_original_184',
                                          'sr2_original_225',
                                          'sr2_original_305'
)

#remove duplicates

original_search <- original_search%>%
  filter(!record_id %in% originalduplicate_record_id_toremove)

corrected_original_prisma<- original_search%>%
  group_by(label)%>%
  summarise(n = length(record_id))

write.csv(corrected_original_prisma, 'output/SR2/original/corrected_original_SR2_prisma.csv', row.names = FALSE )

write.csv(original_search, 'output/SR2/original/cleaned_SR2_original_studies.csv', row.names = FALSE)