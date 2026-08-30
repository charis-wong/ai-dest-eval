
#we did our first elicit search in july 2025; second in oct 2025; yielded different publications. this is to check performance on search oct 2025
# on re-export, elicit now uses abstract then ft screening. 


#load packages----
library(ASySD)
library(tidyverse)
library(stringr)

# add evaluation name here
evaluation_name <- "SR3_elicit"

#original review: DOI 10.1002/14651858.CD014713.pub2

#1. load and wrangle files----

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
         isbn = '')

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


#1.2 elicit
elicitTiAb <- read.csv('data/SR3/elicit/Elicit - screen-results-review-c8133957-5150-4f26-8ee6-f57b0df25f31.csv')

elicitFTScreen <- read.csv('data/SR3/elicit/Elicit - screen-results-review-c8133957-5150-4f26-8ee6-f57b0df25f31 (1).csv')

elicitTiAb <- elicitTiAb%>%
  select(Title,
         Abstract,
         Authors, 
         DOI,
         Year,
         Venue,
         Ab.Screening.judgement = Screening.judgement,
         Ab.Exclusion.reason = Exclusion.reason
        )
elicitFT_ToJoin <- elicitFTScreen%>%
  select(Title,
         Abstract,
         Authors, 
         DOI,
         Year,
         FT.Screening.judgement = Screening.judgement,
         FT.Exclusion.reason = Exclusion.reason
  )

elicitAllScreening <- left_join(elicitTiAb, elicitFT_ToJoin, by = c('Title' = 'Title',
                                                                    'Abstract' = 'Abstract',
                                                                    'Authors' = 'Authors',
                                                                    'DOI' = 'DOI',
                                                                    "Year" = "Year"))
  

elicitAllScreening <- elicitAllScreening%>%
  mutate(record_id = paste0("sr3_elicit_", row_number()),
         label = ifelse(Ab.Screening.judgement == "Exclude", 'elicit_tiab_exclude',
                        ifelse(FT.Screening.judgement == "Include", "elicit_ft_include", "elicit_ft_exclude")))

# wrangle to asysd compatible format----
elicit_asysd_format <-elicitAllScreening %>% 
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
         isbn = "")


#use asysd to find overlap
todedup <- rbind(elicit_asysd_format, original_asysd_format)


# use asysd to identify overlaps ----
results <-dedup_citations(todedup, merge_citations = TRUE, keep_source = 'elicit')
#asysd have deemed these unique - with deduplicates collapsed into one row each with record_ids concatenated
unique <- results$unique

manual_dedup<- results$manual_dedup

#write csv file for record keeping
write.csv(manual_dedup, paste0('output/SR3/elicit/manual_dedup_', evaluation_name, '.csv'))

# use shiny interface to identify if these overlap
manual_review <- manual_dedup_shiny(results$manual_dedup)


# Complete deduplication
final_result <- dedup_citations_add_manual(results$unique, additional_pairs = manual_review)


#note duplicate in original excluded studies 
originalduplicates<- final_result%>%filter(label == "original_exclude, original_exclude")
duplicate_ids <-unlist(strsplit(originalduplicates$record_ids, ", "))

duplicate <- original_search%>%filter(record_id %in% duplicate_ids)
write.csv(duplicate, 'output/SR3/original_excluded_duplicates.csv', row.names = FALSE)

#3 duplicates in original_excluded. update prisma


#manual check - matches missed

#manual clean
#sr3_original_36 = sr3_elicit_337

final_result[final_result$duplicate_id == "sr3_elicit_337", ]$record_ids = "sr3_elicit_337, sr3_original_36"
final_result[final_result$duplicate_id == "sr3_elicit_337", ]$label = "elicit_ft_include, original_exclude"
final_result[final_result$duplicate_id == "sr3_elicit_337", ]$source = "elicit, original"
# 
# final_result[final_result$duplicate_id == "sr3_elicit_148", ]$record_ids = "sr3_elicit_148, sr3_original_147"
# final_result[final_result$duplicate_id == "sr3_elicit_148", ]$label = "elicit_include, original_exclude"
# final_result[final_result$duplicate_id == "sr3_elicit_148", ]$source = "elicit, original"



# final_result <- final_result %>%filter(!duplicate_id %in% c("sr3_original_36", "sr3_original_147"))


final_result <- final_result%>%
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

breakdown <- final_result%>%
  group_by(label)%>%
  summarise(count = length(duplicate_id))

write.csv(final_result, paste0('output/SR3/elicit/all_citations_', evaluation_name, '.csv'), row.names=FALSE)
write.csv(breakdown,  paste0('output/SR3/elicit/breakdown_', evaluation_name, '.csv'), row.names = FALSE)
