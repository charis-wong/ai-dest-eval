
#load packages----
library(ASySD)
library(tidyverse)
library(rcrossref)
library(stringr)

# add evaluation name here
sr <- "SR2"
srp <- "elicit"

evaluation_name <- paste0(sr, '_', srp)
folder_name <- paste0(sr, '/', srp, '/')


#original review: DOI 10.11124/JBIES-23-00291

#1. load files----

#1.1 original review -----

#original review studies clean using 'clean_SR2_original_studies.R'. Load output from that script 

original_search <- read.csv('output/SR2/original/cleaned_SR2_original_studies.csv')

#1.2 elicit file----

elicit_file<-list.files(path = 'data/SR2/elicit', pattern = "\\.csv$")

# Read each file
for (file in elicit_file) {
  assign('elicit', read.csv(file.path('data/SR2/elicit', file)), envir = .GlobalEnv)
}

elicit <- elicit %>% mutate(record_id = paste0("sr2_elicit_", row_number()),
                            label = ifelse(Screening.judgement == "Include", 'elicit_include', 'elicit_exclude')
)

elicitdoiduplicates <- elicit%>%
  filter(DOI!="-")%>%
  group_by(DOI)%>%
  summarise(n = length(record_id))%>%
  filter(n>1)

elicitduplicates <-elicit%>%
  filter(DOI %in% elicitdoiduplicates$DOI)

write.csv(elicitduplicates, paste0('output/', folder_name, 'elicit_duplicates', evaluation_name, '.csv'), row.names=FALSE)

#manual check, then filter out duplicate record_ids - note elicit includes different versions of preprints submitted to medrxiv (10.1101/2020.12.16.20248357); list 'OUP accepted manuscript' as title for 10.1093/jpids/piab081 and 10.1093/ajcp/aqab173
#elicit lists two titles for 10.1016/j.jpeds.2021.02.052, one correct 'Diagnostic performance of antigen testing for severe acute respiratory syndrome coronavirus 2', the other abbreviated 'Diagnostic performance of antigen testing for SARS-CoV-2', but reasonings given does not match citation (letter to editor)

elicitduplicates_record_ids<-c(
  'sr2_elicit_212',
  'sr2_elicit_349',
  'sr2_elicit_231',
  'sr2_elicit_464',
  'sr2_elicit_492'
)


elicit <- elicit%>%
  filter(!record_id %in% elicitduplicates_record_ids)


doi_match <- original_search%>%
  filter(doi_clean %in% elicit$DOI)

elicittomatch <- elicit%>%
  filter(!DOI %in% doi_match$doi_clean)

elicit_asysd_format <-elicittomatch %>% 
  select(author = Authors,
         year = Year,
         journal = Venue,
         doi = DOI,
         title = Title,
         record_id,
         label)%>%
  mutate(source = 'elicit',
         pages = "",
         volume = "",
         number = "",
         abstract = "",
         isbn = ""
  )

originaltomatch <- original_search%>%
  filter(!doi_clean %in% doi_match$doi_clean)

original_search_asysd <- originaltomatch %>%
  select(author,
         year,
         journal,
         doi = doi_clean,
         title,
         pages,
         volume,
         number,
         abstract,
         record_id,
         isbn,
         label,
         source)

todedup <- rbind(elicit_asysd_format, original_search_asysd)

results <-dedup_citations(todedup, merge_citations = TRUE, keep_source = 'elicit')

unique <- results$unique
manual_dedup<- results$manual_dedup

#write csv file for record keeping
write.csv(manual_dedup, paste0('output/', folder_name, 'manual_dedup_', evaluation_name, '.csv'))
# use shiny interface to identify if these overlap
manual_review <- manual_dedup_shiny(results$manual_dedup)

final_dedup_result <- dedup_citations_add_manual(results$unique, additional_pairs = manual_review)

write_citations(final_dedup_result, type = 'csv', paste0('output/', folder_name, evaluation_name, '_search_screen.csv'))
#wrangle to correct format
doi_match<-doi_match%>%
  select(-doi)%>%
  select(author,
         year,
         journal,
         doi = doi_clean,
         title,
         pages,
         volume,
         number,
         abstract,
         record_id,
         isbn,
         label1=label,
         source)
elicit_labels <- elicit%>%
  select(doi = DOI,
         elicitrecord_id = record_id,
         elicitlabel = label)%>%
  mutate(elicitsource = 'elicit')
doi_match_joinlabels <- left_join(doi_match, elicit_labels, by = 'doi')%>%unique()
doi_match_joinlabels <- doi_match_joinlabels%>%
  mutate(label = paste(label1, elicitlabel, sep = ', '),
         record_ids = paste(record_id, elicitrecord_id, sep = ','),
         source = paste(source, elicitsource, sep = ','),
         duplicate_id = record_id)%>%
  select(names(final_dedup_result))
all_citations <- rbind(doi_match_joinlabels, final_dedup_result)
#clean data
all_citations[all_citations$label == 'elicit_include, elicit_include',]$label = 'elicit_include'

breakdown <- all_citations%>%
     group_by(label)%>%
     summarise(count = length(duplicate_id))

#write results out
write.csv(all_citations, paste0('output/SR2/elicit/all_citations_', evaluation_name, '.csv'), row.names=FALSE)
write.csv(breakdown,  paste0('output/SR2/elicit/breakdown_', evaluation_name, '.csv'), row.names = FALSE)
