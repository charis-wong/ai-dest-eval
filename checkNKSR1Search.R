#to check if current search done manually similar to original search

library(ASySD)
library(tidyverse)
library(rcrossref)
library(stringr)

evaluation_name <- "SR1_NK"

original_search <- read.csv('data/SR1/original/all_studies_0224.csv')
NK_Dedup_search <- read.csv('data/SR1/NK/TAAR1 Nested Knowledge search results.csv')


NK_asysd_format <-NK_Dedup_search %>% 
  mutate(pages = '',
         volume = '',
         number = '',
         DOI = tolower(DOI),
         record_id = paste0('sr1_NK_', row_number()),
         isbn = '',
         label = 'NK')%>%
  select(author = Author,
         year = Year,
         journal = Journal,
         doi = DOI,
         title = Title,
         pages,
         volume,
         number,
         abstract = Abstract,
         record_id,
         isbn,
         label)%>%
  mutate(source = 'NK')

original_search_asysd <- original_search %>%
  mutate(
    pages = '',
    volume = '',
    number = '',
    isbn = '',
    Doi = tolower(Doi),
    label = 'original',
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

doi_match <- original_search_asysd%>%
  filter(doi %in% NK_asysd_format$doi)



todedup <- rbind(NK_asysd_format, original_search_asysd)%>%
  filter(!doi %in% doi_match$doi)


# use asysd to identify overlaps ----
results <-dedup_citations(todedup, merge_citations = TRUE, keep_source = 'NK')
#asysd have deemed these unique - with deduplicates collapsed into one row each with record_ids concatenated
unique <- results$unique

manual_dedup<- results$manual_dedup

#write csv file for record keeping
write.csv(manual_dedup, paste0('output/manual_dedup_', evaluation_name, '.csv'))

# use shiny interface to identify if these overlap
manual_review <- manual_dedup_shiny(results$manual_dedup)

# Complete deduplication
final_result <- dedup_citations_add_manual(results$unique, additional_pairs = manual_review)

tocheck <- final_result %>% 
  filter(label == 'original' | label == 'NK')%>%
  arrange(title)


write.csv(tocheck, 'output/SR1/NK/citationstocheck.csv', row.names = FALSE)
