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
#1.2 elicit file----

elicit_file<-list.files(path = 'data/SR2/elicit', pattern = "\\.csv$")

# Read each file
for (file in elicit_file) {
  assign('elicit_original', read.csv(file.path('data/SR2/elicit', file)), envir = .GlobalEnv)
}

elicit <- elicit_original %>% mutate(record_id = paste0("sr2_elicit_", row_number()),
                            label = ifelse(Screening.judgement == "Include", 'elicit_include', 'elicit_exclude')
)

elicitdoiduplicates <- elicit%>%
  filter(DOI!="-")%>%
  group_by(DOI)%>%
  summarise(n = length(record_id))%>%
  filter(n>1)

elicitduplicates <-elicit%>%
  filter(DOI %in% elicitdoiduplicates$DOI)

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

elicit_check_preprint_duplicate <- elicit %>%
  rowwise() %>%
  mutate(is_preprint = is_preprint_doi(DOI),
         published_DOI = get_published_doi_crossref(DOI))%>%
  ungroup()

elicit_check_preprint_duplicate$DOI <- tolower(elicit_check_preprint_duplicate$DOI)
elicit_check_preprint_duplicate$published_DOI <- tolower(elicit_check_preprint_duplicate$published_DOI)

elicit_preprint_duplicates <- elicit_check_preprint_duplicate%>%
  filter(DOI %in% elicit_check_preprint_duplicate$published_DOI)

elicit_dedup1<-elicit_check_preprint_duplicate%>%filter(!published_DOI %in%elicit_preprint_duplicates$DOI)

elicit_asysd_format <-elicit_dedup1 %>% 
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
dedup2 <- dedup_citations(elicit_asysd_format, merge_citations = TRUE, keep_source = 'elicit')
