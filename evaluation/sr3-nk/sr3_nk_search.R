#load packages----
library(ASySD)
library(tidyverse)
library(rcrossref)
library(stringr)

# add evaluation name here
sr <- "SR3"
srp <- "nk"

evaluation_name <- paste0(sr, '_', srp)
folder_name <- paste0(sr, '/', srp, '/')

#original review: DOI 10.1002/14651858.CD014713.pub2

#1. load files----

#1.1 original review -----
# original review provides citation data for publications screened at FT - 21 included, 160 excluded

original_include <- read.csv('data/SR3/original/SR3_original_included.csv')%>%
  mutate(label = 'original_include',
         source = 'original')
original_exclude <- read.csv('data/SR3/original/SR3_original_excluded.csv')%>%
  mutate(label = 'original_exclude',
         source = 'original')

original_search <- rbind(original_include, select(original_exclude, names(original_include)))
original_search <- original_search%>%
  mutate(record_id = paste0('sr3_original_', row_number()),
         isbn = '')%>%
  mutate(doi_clean = tolower(enc2utf8(DOI)))


original_asysd_format<-original_search%>%
  select(author = Authors,
         year = Publication.year,
         journal = Journal,
         doi = DOI,
         title = Title,
         pages = Pages,
         volume = Volume,
         number = Issue,
         record_id,
         label,
         source)%>%
  mutate(abstract = "",
         isbn = "")




nk <- read.csv('data/SR3/nk/SR3_NK_all_screening.csv')%>%
  mutate(record_id = paste0("sr3_nk_", row_number()))

nk_ft_exclude <- filter(nk, grepl('Excluded', Full.Text.Screening.Status))
nk <- nk %>% mutate(label = ifelse(Full.Text.Screening.Status == "Included", 'nk_ft_include', 
                                   ifelse(record_id %in% nk_ft_exclude$record_id, 'nk_ft_exclude', 'nk_tiab_exclude')))
nk <- nk%>%
  mutate(DOI = tolower(enc2utf8(DOI)))


# first map matching dois - no abstract supplied by original article

doi_match <- original_search%>%
  filter(doi_clean != '')%>%
  filter(doi_clean %in% nk$DOI)


#filter out dois appearing multiple times - typically either duplicates or conference abstracts with multiple title/abstracts
count_doi <- doi_match%>%group_by(doi_clean)%>%summarise(n=length(record_id))%>%filter(n>1)
nk_count_doi <- nk%>%filter(DOI!='')%>%group_by(DOI)%>%summarise(n=length(record_id))%>%filter(n>1)%>%rename(doi_clean = DOI)
multiple_doi <- rbind(count_doi, nk_count_doi)

doi_match <- doi_match%>%filter(!doi_clean %in% multiple_doi$doi_clean)


nktomatch <- nk%>%
  filter(!DOI %in% doi_match$doi_clean)%>%
  filter(!DOI %in% multiple_doi)

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
  filter(!doi_clean %in% doi_match$doi_clean)


#review overlap between original search and nk
original_search_asysd <- originaltomatch %>%
  select(author = Authors,
         year = Publication.year,
         journal = Journal,
         doi = DOI,
         title = Title,
         pages = Pages,
         volume = Volume,
         number = Issue,
         record_id,
         label,
         source)%>%
  mutate(abstract = "",
         isbn = "")%>%
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
# manual_review <- manual_dedup_shiny(results$manual_dedup)

manual_review <- read.csv('output/SR3/nk/SR3_nk_dedup_manual_reviewed.csv')

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


#add record_ids of other matches manually picked up during data cleaning - lack of data/metadata meant these were not picked up as matches by asysd


manual_match1<-data.frame(
  original_ids = c(
    'sr3_original_3', #sr3_nk_643
    'sr3_original_5', #sr3_nk_576
    'sr3_original_7', #sr3_nk_23
    'sr3_original_9',#sr3_nk_1857
    'sr3_original_13', #sr3_nk_238
    'sr_original_117', #sr3_nk_7896
    'sr3_original_130',#sr3_nk_8007
    'sr3_original_171', #sr3_nk_7995
    'sr3_original_22', #sr3_nk_2169
    'sr3_original_59' #'sr3_nk_10357'
  ),
  nk_ids=c( 'sr3_nk_643',
            'sr3_nk_576',
            'sr3_nk_23',
            'sr3_nk_1857',
            'sr3_nk_238',
            'sr3_nk_7896',
            'sr3_nk_8007',
            'sr3_nk_7995',
            'sr3_nk_2169',
            'sr3_nk_10357')
  
)

#identified by matching nk titles to original titles and manually checking
other_match <- read.csv('output/SR3/nk/SR3_nk_matched_records.csv')

manual_match <- rbind(manual_match1, other_match)

manual_matched <- original_search%>%filter(record_id%in%manual_match$original_ids)
manual_matched <- left_join(manual_matched, manual_match, by = c("record_id"="original_ids"))

nk_labels <- nk%>%
  select(doi = DOI,
         nkrecord_id = record_id,
         nklabel = label)%>%
  mutate(nksource = 'nk')


manual_matched_joinlabels <- left_join(manual_matched, select(nk_labels,-doi), by = c('nk_ids' = 'nkrecord_id'))
manual_matched_joinlabels <- manual_matched_joinlabels%>%
  select(author = Authors,
         year = Publication.year,
         journal = Journal,
         doi = doi_clean,
         title = Title,
         pages = Pages,
         volume = Volume,
         number = Issue,
         record_id,
         label,
         nklabel,
         nk_ids,
         isbn,
         source, 
         nksource)%>%
  mutate(abstract = '')%>%
  mutate(label = paste(label, nklabel, sep = ', '),
         record_ids = paste(record_id, nk_ids, sep = ','),
         source = paste(source, nksource, sep = ','),
         duplicate_id = record_id)%>%
  select(names(final_result))



doi_match<-doi_match%>%
  select(-DOI)%>%
  select(author = Authors,
         year = Publication.year,
         journal = Journal,
         doi = doi_clean,
         title = Title,
         pages = Pages,
         volume = Volume,
         number = Issue,
         record_id,
         label1=label,
         source)%>%
  mutate(abstract = '', isbn = '')%>%
  filter(!record_id%in%manual_match$original_ids)




doi_match_joinlabels <- left_join(doi_match, nk_labels, by = 'doi')%>%unique()
doi_match_joinlabels <- doi_match_joinlabels%>%
  mutate(label = paste(label1, nklabel, sep = ', '),
         record_ids = paste(record_id, nkrecord_id, sep = ','),
         source = paste(source, nksource, sep = ','),
         duplicate_id = record_id)%>%
  select(names(final_result))

final_result <- final_result%>%filter(!duplicate_id %in% manual_matched_joinlabels$duplicate_id) 

all_citations <- rbind(doi_match_joinlabels, final_result, manual_matched_joinlabels)

all_citations <- all_citations%>%
  mutate(
    label = map_chr(label, ~ .x %>%
                  str_split(",") %>%
                  unlist() %>%
                  str_trim() %>%
                  unique() %>%
                  sort() %>%              
                  paste(collapse = ", ")
    )
  )


all_citations_record_ids <- all_citations%>%
  group_by(duplicate_id)%>%
  summarise(
    record_ids = paste(record_ids, collapse = ','))

all_citations <- all_citations%>%
  select(-record_ids)%>%unique()
all_citations <- all_citations[!duplicated(all_citations$duplicate_id),]

all_citations <-right_join(all_citations, all_citations_record_ids, by = "duplicate_id")%>%unique()



breakdown <- all_citations%>%
  group_by(label)%>%
  summarise(count = length(unique(duplicate_id)))

#write results out
write.csv(all_citations, paste0('output/SR3/nk/', evaluation_name, '_all_citations.csv'), row.names=FALSE)
write.csv(breakdown,  paste0('output/SR3/nk/', evaluation_name, '_breakdown.csv'), row.names = FALSE)




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
  filter(!doi_clean == '')%>%
  filter(!doi_clean %in% multiple_doi$doi_clean)%>%
  filter(doi_clean %in% nk_robot$DOI)

nkrobottomatch <- nk_robot%>%
  filter(!DOI %in% multiple_doi$doi_clean)%>%
  filter(!DOI %in% robot_doi_match$doi_clean)%>%
  filter(!DOI == "")


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
  select(author = Authors,
         year = Publication.year,
         journal = Journal,
         doi = DOI,
         title = Title,
         pages = Pages,
         volume = Volume,
         number = Issue,
         record_id,
         label,
         source)%>%
  mutate(abstract = "",
         isbn = "")%>%
  mutate(across(everything(), as.character))

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
  select(-DOI)%>%
  select(author = Authors,
         year = Publication.year,
         journal = Journal,
         doi = doi_clean,
         title = Title,
         pages = Pages,
         volume = Volume,
         number = Issue,
         record_id,
         label1=label,
         isbn,
         source)%>%
  mutate(abstract = '')%>%
  filter(!doi%in% multiple_doi$doi_clean)
  
  
  
  # select(-doi)%>%
  # select(author,
  #        year,
  #        journal,
  #        doi = doi_clean,
  #        title,
  #        pages,
  #        volume,
  #        number,
  #        abstract,
  #        record_id,
  #        isbn,
  #        label1=label,
  #        source)

nk_robot_labels <- nk_robot%>%
  select(doi = DOI,
         nkrecord_id = record_id,
         nklabel = label)%>%
  mutate(nksource = 'nk')



robot_doi_match_joinlabels <- left_join(robot_doi_match, filter(nk_robot_labels, doi!=""), by = 'doi')%>%unique()
robot_doi_match_joinlabels <- robot_doi_match_joinlabels%>%
  mutate(label = paste(label1, nklabel, sep = ','),
         record_ids = paste(record_id, nkrecord_id, sep = ','),
         source = paste(source, nksource, sep = ','),
         duplicate_id = record_id)%>%
  select(names(robot_final_result))
robot_doi_match_joinlabels <- distinct (robot_doi_match_joinlabels, duplicate_id, .keep_all = TRUE)


robot_all_citations <- rbind(robot_doi_match_joinlabels, robot_final_result)%>%unique()

robot_all_citations <- robot_all_citations%>%
  mutate(
    label = map_chr(label, ~ .x %>%
                      str_split(",") %>%
                      unlist() %>%
                      str_trim() %>%
                      unique() %>%
                      sort() %>%              
                      paste(collapse = ", ")
    )
  )


robot_breakdown <- robot_all_citations%>%
  group_by(label)%>%
  summarise(count = length(duplicate_id))

write.csv(robot_breakdown, paste0('output/', folder_name, evaluation_name, '_robot_search_screen_performance.csv'), row.names=FALSE)
write.csv(robot_all_citations, paste0('output/' ,folder_name, evaluation_name, '_robot_all_citations.csv'), row.names=FALSE)


