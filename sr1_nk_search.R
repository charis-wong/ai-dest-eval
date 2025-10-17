#load packages----
library(ASySD)
library(tidyverse)
library(rcrossref)
library(stringr)

# add evaluation name here
sr <- "SR1"
srp <- "NK"

evaluation_name <- paste0(sr, '_', srp)
folder_name <- paste0(sr, '/', srp, '/')

original_search <- read.csv('data/SR1/original/all_studies_0224.csv')
original_Tiab_include <- read.csv('data/SR1/original/screen_include_0224.csv')
original_ft_exclude <- read.csv('data/SR1/original/screened_excluded_0224.csv')
original_ft_include <- original_Tiab_include%>%filter(!Doi %in% original_ft_exclude$Doi)
nk <- read.csv('data/SR1/NK/SR1_NK_all_screening.csv')%>%
  mutate(record_id = paste0("sr1_nk_", row_number()))
nk_ft_exclude <- filter(nk, grepl('Excluded', Full.Text.Screening.Status))
nk <- nk %>% mutate(label = ifelse(Full.Text.Screening.Status == "Included", 'nk_ft_include', 
                                           ifelse(record_id %in% nk_ft_exclude$record_id, 'nk_ft_exclude', 'nk_tiab_exclude')))


# wrangle to asysd compatible format----
nk_asysd_format <-nk %>% 
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


#review overlap between original search and elicit
original_search_asysd <- original_search %>%
  mutate(
    pages = '',
    volume = '',
    number = '',
    isbn = '',
    label = ifelse(StudyId %in% original_ft_include$StudyId, 'original_ft_include', 
                   ifelse(StudyId %in% original_ft_exclude$StudyId, 'original_ft_exclude',
                          'original_Tiab_exclude')
    ),
    source = 'original')%>%
  select(author = Authors,
         year = Year,
         journal = PublicationName,
         doi = Doi,
         title = Title,
         pages,
         volume,
         number,
         abstract = Abstract,
         record_id = StudyId,
         isbn,
         label,
         source)

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

#write results out
write.csv(overlap, paste0('output/', folder_name, 'overlapping_citations_', evaluation_name, '.csv'), row.names=FALSE)

breakdown <- final_result%>%
  group_by(label)%>%
  summarise(count = length(duplicate_id))
write.csv(breakdown, paste0('output/', folder_name, evaluation_name, '_search_screen_performance.csv'), row.names=FALSE)


### check robot screener performance

nk_robot_screener1 <- nk %>% 
  filter(Abstract.Screening.Reviewer.1 == "Robot")%>%
  mutate(label = ifelse(Abstract.Screening.Status..Reviewer.1.== "Advanced", 'nk_robot_include', 'nk_robot_exclude'))
  
nk_robot_screener2 <- nk %>% 
  filter(Abstract.Screening.Reviewer.2 == "Robot")%>%
  mutate(label = ifelse(Abstract.Screening.Status..Reviewer.2.== "Advanced", 'nk_robot_include', 'nk_robot_exclude'))
  
nk_robot <- rbind(nk_robot_screener1, nk_robot_screener2)  

nk_robot_asysd_format <- nk_robot%>% 
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


nkrobottodedup <- rbind(nk_robot_asysd_format, original_search_asysd)

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
robot_overlap <- filter(robot_final_result, grepl(',', record_ids))

write_citations(robot_overlap, type = 'csv', paste0('output/', folder_name, evaluation_name, 'robot_search_screen.csv'))

#write results out
write.csv(robot_overlap, paste0('output/', folder_name, 'overlapping_citations_robot', evaluation_name, '.csv'), row.names=FALSE)

robot_breakdown <- robot_final_result%>%
  group_by(label)%>%
  summarise(count = length(duplicate_id))
write.csv(robot_breakdown, paste0('output/', folder_name, evaluation_name, 'robot_search_screen_performance.csv'), row.names=FALSE)
