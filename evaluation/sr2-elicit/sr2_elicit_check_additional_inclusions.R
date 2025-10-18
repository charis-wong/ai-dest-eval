#check sr2 additional inclusions from elicit that were not included in original review for preprints and linked publications
library(ASySD)
library(tidyverse)
library(rcrossref)
library(stringr)
library(httr)


#original review: DOI 10.11124/JBIES-23-00291

#load functions to check if preprint and to get published dois for preprints---- 


is_preprint_doi <- function(doi) {
  url <- paste0("https://api.crossref.org/works/", doi)
  res <- try(GET(url), silent = TRUE)
  
  if (inherits(res, "try-error") || status_code(res) != 200) {
    return(NA)  # or FALSE if you prefer
  }
  
  data <- content(res, as = "parsed", type = "application/json")
  type <- data$message$type
  
  # Crossref preprints usually have type = "posted-content"
  return(type == "posted-content")
}

get_published_doi_crossref <- function(preprint_doi) {
  url <- paste0("https://api.crossref.org/works/", preprint_doi)
  res <- GET(url)
  
  if (status_code(res) != 200) return(NA)
  
  data <- content(res, as = "parsed", type = "application/json")
  
  if (!is.null(data$message$relation$`is-preprint-of`)) {
    dois <- sapply(data$message$relation$`is-preprint-of`, function(x) x$id)
    return(paste(dois, collapse = "; "))
  }
  
  return(NA)
}



#2. load files----

#2.1 original review -----

#original review studies cleaned using 'clean_SR2_original_studies.R'. Load output from that script; duplicates removed, doi format cleaned. 

original_search <- read.csv('output/SR2/original/cleaned_SR2_original_studies.csv',fileEncoding="latin1")
original_search$doi_clean <- tolower(original_search$doi_clean)


#2.2 additional inclusions within elicit not in original review. we dual screened these in syrf for appropriateness of inclusion
#original review last search was 12 July 2022. Column PublishedBeforeSearchDate reflects this; use year for years other than 2022; manual check for citations with publication year 2022

additional_inclusions_syrf_file<-list.files(path = 'data/SR2/elicit/additional_inclusions_screening', pattern = "\\.csv$")

# Read each file
for (file in additional_inclusions_syrf_file) {
  assign('additional_inclusions', read.csv(file.path('data/SR2/elicit/additional_inclusions_screening', file)), envir = .GlobalEnv)
}

#3. check if additional inclusions are preprints and retrieve their published DOIs

additional_inclusions <- additional_inclusions %>%
  rowwise() %>%
  mutate(is_preprint = is_preprint_doi(Doi),
         published_DOI = get_published_doi_crossref(Doi))%>%
  ungroup()

additional_inclusions$published_DOI <- tolower(additional_inclusions$published_DOI)


additional_inclusions <- additional_inclusions%>%
  mutate(source = "elicit",
         label = ifelse(ScreeningStatus == "Included", 
                        ifelse(PublishedBeforeOriginalSearch == TRUE, "additional_included_before", "additional_included_after"), 
                        ifelse(PublishedBeforeOriginalSearch == TRUE, "additional_excluded_before", "additional_excluded_after")
                        ))

additional_inclusions_with_published_dois <- additional_inclusions%>%
  filter(!is.na(published_DOI))

#check for duplicate dois within additional inclusions list
additional_inclusions$Doi <- tolower(additional_inclusions$Doi)
additional_inclusions_duplicate_dois <- additional_inclusions%>%filter(Doi %in% additional_inclusions_with_published_dois$published_DOI)

additional_inclusions_unique <- additional_inclusions%>%
  filter(!published_DOI%in%additional_inclusions_duplicate_dois$Doi)

#match retrieved published DOIs with DOIs within original search

additional_inclusions_preprint_match_original <- additional_inclusions_with_published_dois%>%
  filter(!published_DOI%in%additional_inclusions_duplicate_dois$Doi)%>%
  filter(published_DOI %in% original_search$doi_clean)


#for unmatched - get metadata using published data from crossref
additional_inclusions_with_published_dois_to_match <- additional_inclusions_with_published_dois%>%
  filter(!StudyId %in% additional_inclusions_preprint_match_original$StudyId)

# get metadata from crossref----
for(i in 1:nrow(additional_inclusions_with_published_dois_to_match)){
  doi_i <- additional_inclusions_with_published_dois_to_match[i, ]$published_DOI
  if(!is.na(doi_i)){
    bib <- cr_cn(dois = doi_i, format = "bibentry")
    try(dat <- data.frame(
      author = bib$author,
      year = bib$year,
      journal = bib$journal,
      doi = doi_i,
      title = bib$title,
      pages = bib$pages,
      volume = bib$volume,
      number = bib$number,
      isbn = bib$ISSN))
    if(!exists("dat")) dat <- data.frame(
      author = NA,
      year = NA,
      journal = NA,
      doi = doi_i,
      title = NA,
      pages = NA,
      volume = NA,
      number = NA,
      isbn = NA)
    try(abst <- cr_abstract(doi_i))
    if(!exists('abst')) dat <- dat%>%mutate(abstract = NA) else dat <- dat%>%mutate(abstract = abst)
    if(!exists('crossrefdata')) crossrefdata <- dat else crossrefdata <- rbind(crossrefdata, dat)

    try(rm(bib, dat))
    try(rm(abst))

  }
}

additional_inclusions_with_published_dois_to_match_syrf_id <- additional_inclusions_with_published_dois_to_match%>%
  select(doi = published_DOI,
         previous_doi = Doi,
         record_id = StudyId, 
         label, 
         source,
         )

additional_inclusions_to_match <- left_join(additional_inclusions_with_published_dois_to_match_syrf_id, crossrefdata, by = "doi")

# wrangle to asysd compatible format----

original_search_asysd <- original_search %>%
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

additional_inclusions_to_match_asysd <- additional_inclusions_to_match%>%
  select(names(original_search_asysd))


#check for overlaps using asysd


todedup <- rbind(additional_inclusions_to_match_asysd, original_search_asysd)

results <-dedup_citations(todedup, merge_citations = TRUE, keep_source = 'elicit')

unique <- results$unique

#no duplicates found


# update additional inclusions - remove those with matching dois with original

additional_inclusions_new <- additional_inclusions_unique%>%
  filter(!StudyId %in% additional_inclusions_preprint_match_original$StudyId)

additional_inclusions_summary <- additional_inclusions_new%>%
  group_by(label)%>%summarise(n=length(StudyId))

write.csv(additional_inclusions_summary, 'output/SR2/elicit/SR2_elicit_additional_inclusions_summary.csv', row.names = FALSE)
write.csv(additional_inclusions_new, 'output/SR2/elicit/SR2_elicit_additional_inclusions.csv', row.names = FALSE)


