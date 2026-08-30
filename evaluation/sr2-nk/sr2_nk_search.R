#load packages----
library(ASySD)
library(tidyverse)
library(rcrossref)
library(stringr)

# add evaluation name here
sr <- "SR2"
srp <- "NK"

evaluation_name <- paste0(sr, '_', srp)
folder_name <- paste0(sr, '/', srp, '/')

#original review: DOI 10.11124/JBIES-23-00291

#1. load files----

#1.1 original review -----
#original review studies cleaned using 'clean_SR2_original_studies.R'. Load output from that script; duplicates removed, doi format cleaned. 
original_search <- read.csv('output/SR2/original/cleaned_SR2_original_studies.csv')
original_search <- original_search %>%
  mutate(doi_clean = tolower(enc2utf8(doi_clean)))


nk <- read.csv('data/SR2/NK/SR2_NK_all_screening.csv')%>%
  mutate(record_id = paste0("sr2_nk_", row_number()))

nk_ft_exclude <- filter(nk, grepl('Excluded', Full.Text.Screening.Status))
nk <- nk %>% mutate(label = ifelse(Full.Text.Screening.Status == "Included", 'nk_ft_include', 
                                           ifelse(record_id %in% nk_ft_exclude$record_id, 'nk_ft_exclude', 'nk_tiab_exclude')))


# first map matching dois - no abstract supplied by original article

doi_match <- original_search%>%
  filter(doi_clean %in% nk$DOI)

#add record_ids of other matches manually picked up during data cleaning - lack of data/metadata meant these were not picked up as matches by asysd
manual_match_original_ids <- c('sr2_original_443', #sr2_nk_686
                      'sr2_original_485', #sr2_nk_123
                      'sr2_original_493', #sr2_nk_230
                      'sr2_original_497'#sr2_nk_859
)

manual_match_nk_ids<-c('sr2_nk_686',
                      'sr2_nk_123',
                      'sr2_nk_230,',
                      'sr2_nk_859')

manual_match<-data.frame(
  original_ids = c('sr2_original_443', #sr2_nk_686
                     'sr2_original_485', #sr2_nk_123
                     'sr2_original_493', #sr2_nk_230
                     'sr2_original_497'#sr2_nk_859
),nk_ids=c('sr2_nk_686',
          'sr2_nk_123',
          'sr2_nk_230',
          'sr2_nk_859',
          'sr3_nk_238')

)

nktomatch <- nk%>%
  filter(!DOI %in% doi_match$doi_clean)%>%
  filter(!record_id%in%manual_match$nk_ids)

# wrangle to asysd compatible format----
nk_asysd_format <-nktomatch %>% 
  select(author = Author,
         year = Year,
         journal = Journal,
         doi = DOI,
         title = Title,
         abstract = Abstract,
         record_id,
         label)%>%
  mutate(source = 'nk',
         
         pages = '',
         volume ='',
         number ='',
         isbn = '')

originaltomatch <- original_search%>%
  filter(!doi_clean %in% doi_match$doi_clean)%>%
  filter(!record_id%in%manual_match$original_ids)


#review overlap between original search and elicit
original_search_asysd <- originaltomatch %>%
  select(author,
         year,
         journal,
         doi,
         title,
         pages,
         volume,
         number,
         abstract,
         record_id,
         isbn,
         label,
         source)%>%
  mutate(across(everything(), as.character))

todedup <- rbind(nk_asysd_format, original_search_asysd)


# use asysd to identify overlaps ----
results <-dedup_citations(todedup, merge_citations = TRUE, keep_source = 'nk')
#asysd have deemed these unique - with deduplicates collapsed into one row each with record_ids concatenated
unique <- results$unique

manual_dedup<- results$manual_dedup

#write csv file for record keeping
write.csv(manual_dedup, paste0('output/', folder_name, 'manual_dedup_', evaluation_name, '.csv'))

# use shiny interface to identify if these overlap
manual_review <- manual_dedup_shiny(results$manual_dedup)

# Complete deduplication
final_result <- dedup_citations_add_manual(results$unique, additional_pairs = manual_review)
#clean labels and source
final_result$label <- sapply(strsplit(final_result$label, ", "), function(z) {
  paste(sort(z), collapse = ",")
})
final_result$source <- sapply(strsplit(final_result$source, ", "), function(z) {
  paste(sort(z), collapse = ",")
})
overlap <- filter(final_result, grepl(',', record_ids))

write_citations(final_result, type = 'csv', paste0('output/', folder_name, evaluation_name, '_search_screen.csv'))



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


  

nk_labels <- nk%>%
  select(doi = DOI,
         nkrecord_id = record_id,
         nklabel = label)%>%
  mutate(nksource = 'nk')
doi_match_joinlabels <- left_join(doi_match, nk_labels, by = 'doi')%>%unique()
doi_match_joinlabels <- doi_match_joinlabels%>%
  mutate(label = paste(label1, nklabel, sep = ', '),
         record_ids = paste(record_id, nkrecord_id, sep = ','),
         source = paste(source, nksource, sep = ','),
         duplicate_id = record_id)%>%
  select(names(final_result))




manual_matched <- original_search%>%filter(record_id%in%manual_match$original_ids)
manual_matched <- left_join(manual_matched, manual_match, by = c("record_id"="original_ids"))
manual_matched_joinlabels <- left_join(manual_matched, select(nk_labels,-doi), by = c('nk_ids' = 'nkrecord_id'))
manual_matched_joinlabels <- manual_matched_joinlabels%>%
  mutate(label = paste(label, nklabel, sep = ', '),
         record_ids = paste(record_id, nk_ids, sep = ','),
         source = paste(source, nksource, sep = ','),
         duplicate_id = record_id)%>%
  select(names(final_result))

all_citations <- rbind(doi_match_joinlabels, final_result,manual_matched_joinlabels)

all_citations_record_ids <- all_citations%>%
  group_by(duplicate_id)%>%
  summarise(record_ids = paste(record_ids, collapse = ','))

all_citations <- all_citations%>%
  select(-record_ids)%>%unique()
all_citations <- all_citations[!duplicated(all_citations$duplicate_id),]

all_citations <-right_join(all_citations, all_citations_record_ids, by = "duplicate_id")%>%unique()

#clean data
all_citations[all_citations$label == 'nk_ft_exclude,nk_ft_exclude',]$label = 'nk_ft_exclude'
all_citations[all_citations$label == 'nk_tiab_exclude,nk_tiab_exclude',]$label = 'nk_tiab_exclude'

breakdown <- all_citations%>%
  group_by(label)%>%
  summarise(count = length(unique(duplicate_id)))

#write results out
write.csv(all_citations, paste0('output/SR2/NK/all_citations_', evaluation_name, '.csv'), row.names=FALSE)
write.csv(breakdown,  paste0('output/SR2/NK/breakdown_', evaluation_name, '.csv'), row.names = FALSE)




#write results out
write.csv(overlap, paste0('output/', folder_name, 'overlapping_citations_', evaluation_name, '.csv'), row.names=FALSE)


### check robot screener performance

nk_robot_screener1 <- nk %>% 
  filter(Abstract.Screening.Reviewer.1 == "Robot")%>%
  mutate(label = ifelse(Abstract.Screening.Status..Reviewer.1.== "Advanced", 'nk_robot_include', 'nk_robot_exclude'))
  
nk_robot_screener2 <- nk %>% 
  filter(Abstract.Screening.Reviewer.2 == "Robot")%>%
  mutate(label = ifelse(Abstract.Screening.Status..Reviewer.2.== "Advanced", 'nk_robot_include', 'nk_robot_exclude'))
  
nk_robot <- rbind(nk_robot_screener1, nk_robot_screener2)  

robot_doi_match <- original_search%>%
  filter(doi_clean %in% nk_robot$DOI)

nkrobottomatch <- nk_robot%>%
  filter(!DOI %in% robot_doi_match$doi_clean)


nk_robot_asysd_format <- nkrobottomatch%>% 
  select(author = Author,
         year = Year,
         journal = Journal,
         doi = DOI,
         title = Title,
         abstract = Abstract,
         record_id,
         label)%>%
  mutate(source = 'nk',
         
         pages = '',
         volume ='',
         number ='',
         isbn = '')

original_search_robot_asysd <-  original_search%>%
  filter(!doi_clean %in% robot_doi_match$doi_clean)%>%
  select(author,
                                       year,
                                       journal,
                                       doi,
                                       title,
                                       pages,
                                       volume,
                                       number,
                                       abstract,
                                       record_id,
                                       isbn,
                                       label,
                                       source)

nkrobottodedup <- rbind(nk_robot_asysd_format, original_search_robot_asysd)

# use asysd to identify overlaps ----
robot_results <-dedup_citations(nkrobottodedup, merge_citations = TRUE, keep_source = 'nk')
#asysd have deemed these unique - with deduplicates collapsed into one row each with record_ids concatenated
robot_unique <- robot_results$unique

robot_manual_dedup<- robot_results$manual_dedup

#write csv file for record keeping
write.csv(robot_manual_dedup, paste0('output/', folder_name, 'nk_robot_manual_dedup_', evaluation_name, '.csv'))

# use shiny interface to identify if these overlap
robot_manual_review <- manual_dedup_shiny(robot_results$manual_dedup)

# Complete deduplication
robot_final_result <- dedup_citations_add_manual(robot_results$unique, additional_pairs = robot_manual_review)
#clean labels and source
robot_final_result$label <- sapply(strsplit(robot_final_result$label, ", "), function(z) {
  paste(sort(z), collapse = ",")
})
robot_final_result$source <- sapply(strsplit(robot_final_result$source, ", "), function(z) {
  paste(sort(z), collapse = ",")
})

robot_doi_match<-robot_doi_match%>%
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

nk_robot_labels <- nk_robot%>%
  select(doi = DOI,
         nkrecord_id = record_id,
         nklabel = label)%>%
  mutate(nksource = 'nk')

robot_doi_match_joinlabels <- left_join(robot_doi_match, nk_robot_labels, by = 'doi')%>%unique()
robot_doi_match_joinlabels <- robot_doi_match_joinlabels%>%
  mutate(label = paste(label1, nklabel, sep = ','),
         record_ids = paste(record_id, nkrecord_id, sep = ','),
         source = paste(source, nksource, sep = ','),
         duplicate_id = record_id)%>%
  select(names(robot_final_result))
robot_doi_match_joinlabels <- distinct (robot_doi_match_joinlabels, duplicate_id, .keep_all = TRUE)


robot_all_citations <- rbind(robot_doi_match_joinlabels, robot_final_result)%>%unique()

#clean data
robot_all_citations[robot_all_citations$label == 'nk_robot_include,nk_robot_include',]$label = 'nk_robot_include'
robot_all_citations[robot_all_citations$label == 'nk_robot_exclude,nk_robot_exclude',]$label = 'nk_robot_exclude'

robot_breakdown <- robot_all_citations%>%
  group_by(label)%>%
  summarise(count = length(duplicate_id))
write.csv(robot_breakdown, paste0('output/', folder_name, evaluation_name, '_robot_search_screen_performance.csv'), row.names=FALSE)
write.csv(robot_all_citations, paste0('output/' ,folder_name, evaluation_name, '_robot_all_citations.csv'), row.names=FALSE)


