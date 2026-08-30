#we initially checked additional inclusions in july 2025; but elicit search in oct 2025 returned different results. check current additional inclusions (n = 127) to see if they have already been screened in our syrf project

#load allcitations from sr3_elicit_search_v2.R

library(dplyr)
library(ASySD)

all_citations <- read.csv("output/SR3/elicit/all_citations_SR3_elicit.csv")

additional_inclusions <-all_citations%>%
  filter(label == "elicit_ft_include")

#load syrf file
syrf_screening <- read.csv('data/SR3/elicit/Screening_data_-_2026_04_25_-_Wide_format_-_4e2a7337-5b85-41b0-9abb-831a89c6d8fd_-_Investigators_Unblinded.csv')

syrf_screening_asysd <-syrf_screening%>%
  select(author = Authors,
         year = Year,
         abstract = Abstract,
         doi = Doi,
         journal = PublicationName,
         title = Title,
         label = ScreeningStatus)%>%
  mutate(pages = "",
         isbn = "",
         volume = "",
         number = "",
         source = "syrf",
         record_id = paste0('syrf_', row_number()))

additional_inclusions_asysd_format <-additional_inclusions %>% 
  mutate(record_id = record_ids, source = 'elicit')

todedup <- rbind(syrf_screening_asysd, select(additional_inclusions_asysd_format,  names(syrf_screening_asysd)))

results <-dedup_citations(todedup, merge_citations = TRUE, keep_source = 'elicit')

unique <- results$unique

manual_dedup<- results$manual_dedup

#no manual dedup returned

# #write csv file for record keeping
# write.csv(manual_dedup, paste0('output/SR3/elicit/sr3_elicit_additional_incl_manual_dedup.csv'), row.names = FALSE)
# 
# # use shiny interface to identify if these overlap
# manual_review <- manual_dedup_shiny(results$manual_dedup)
# 
# 
# # Complete deduplication
# final_result <- dedup_citations_add_manual(results$unique, additional_pairs = manual_review)

final_result <- results$unique
overlap<-final_result%>%filter(source == "elicit, syrf")
to_screen <- final_result%>%filter(source == "elicit")

elicit_biblio<-read.csv('data/SR3/elicit/Elicit - screen-results-review-c8133957-5150-4f26-8ee6-f57b0df25f31.csv')%>%
  select(Title, 
         Abstract,
         Authors,
         DOI, 
         Venue,
         Year)%>%
  mutate(across(everything(), as.character))


to_screen_reformat <- to_screen%>%
  select(Title = title,
         Authors = author,
         Year = year,
         DOI = doi,
         Venue = journal, 
         source,
         record_ids,
         label)%>%
  mutate(across(everything(), as.character))
  

to_screen_with_biblio <- left_join(to_screen_reformat, elicit_biblio, 
                                   by = c('Title', "Authors", 'DOI', 'Venue', 'Year'))


to_screen_syrf_format<-to_screen_with_biblio%>%
  rename(PublicationName = Venue, 
         Doi = DOI,
         CustomId = record_ids)%>%
  mutate(AlternateName = '',
         Url = Doi,
         AuthorAddress = "",
         ReferenceType ='',
         Keywords = '',
         PdfRelativePath = ''
         )%>%
  select(-source, -label)
write.csv(to_screen_syrf_format, 'output/SR3/elicit/sr3_elicit_additional_inclusions_to_screen.csv', row.names = FALSE)


#analyse additional inclusions
additional_screened <- read.csv("data/SR3/elicit/sr3_elicit_additional_inclusions_screened.csv")
additional_screened <- additional_screened%>%
  select(syrflabel = ScreeningDecisions, 
         duplicate_id = CustomId)

additional_screened_joined <- left_join(additional_screened, all_citations, by = "duplicate_id")%>%
  mutate(label = paste0(label, ", ", syrflabel))%>%
  select(-syrflabel)

all_additional_inclusions <- rbind(select(overlap, names(additional_screened_joined)), additional_screened_joined)
all_additional_inclusions <- all_additional_inclusions%>%
  select(
    duplicate_id,
    author,
    year,
    journal,
    doi,
    title,
    label)

write.csv(all_additional_inclusions, 'output/SR3/elicit/SR3-elicit-additional_inclusions_analysed.csv', row.names = FALSE)
