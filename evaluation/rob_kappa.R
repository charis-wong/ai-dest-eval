# Load necessary libraries
library(dplyr)
library(tidyr)
library(irr)

df <- read.csv("evaluation/rob-overall.csv")

# Split the Row.Labels into systematic review evaluation name and tags
df <- df %>%
  mutate(
    SRX_tool = ifelse(grepl("SR", Row.Labels), Row.Labels, NA),
    Tag.org = ifelse(!grepl("SR", Row.Labels), gsub("-.*", "", Row.Labels), NA),
    Tag.tool = ifelse(!grepl("SR", Row.Labels), gsub(".*-", "", Row.Labels), NA)
  )

# Fill the SRX_tool column
df$SRX_tool <- ifelse(is.na(df$SRX_tool), NA, df$SRX_tool)
for (i in 1:nrow(df)) {
  if (is.na(df$SRX_tool[i])) {
    for (j in (i - 1):1) {
      if (!is.na(df$SRX_tool[j])) {
        df$SRX_tool[i] <- df$SRX_tool[j]
        break
      }
    }
  }
}

# Select relevant columns and remove rows with missing values
df <- df %>%
  select(SRX_tool, Tag.org, Tag.tool, Count.of.row) %>%
  filter(!is.na(Count.of.row))

df <- df%>%
  filter(!is.na(Tag.org))

df_expanded <- df %>%
  ungroup() %>%
  split(.$SRX_tool) %>%
  map(function(x) {
    x$Count.of.row <- as.integer(x$Count.of.row)
    x %>% 
      uncount(Count.of.row, .id = NULL) %>% 
      select(SRX_tool, Tag.org, Tag.tool)
  }) %>%
  bind_rows()


# Create a matrix for the kappa calculation
df_list <- split(df, df$SRX_tool)

# Initialize an empty list to store the results
df_kappa_list <- list()

# Loop through each SRX_tool
for (x in names(df_list)) {
  rating_matrix <- matrix(nrow = sum(df_list[[x]]$Count.of.row), ncol = 2, byrow = TRUE)
  count <- 0
  for (i in 1:nrow(df_list[[x]])) {
    for (j in 1:df_list[[x]]$Count.of.row[i]) {
      rating_matrix[count + j, ] <- c(df_list[[x]]$Tag.org[i], df_list[[x]]$Tag.tool[i])
    }
    count <- count + df_list[[x]]$Count.of.row[i]
  }
  kappa <- kappa2(rating_matrix)$value
  df_kappa_list[[x]] <- kappa
}

# Create a dataframe for the results
df_kappa <- data.frame(
  SRX_tool = names(df_kappa_list),
  kappa = unlist(df_kappa_list)
)

# View the result
print(df_kappa)




######
# Create a matrix for the kappa calculation
df_list <- split(df, df$SRX_tool)
df_kappa <- lapply(df_list, function(x) {
  table_matrix <- table(x$Tag.org, x$Tag.tool)
  kappa <- kappam.fleiss(table_matrix)$kappam
  return(kappa)
})

# Create a dataframe for the results
df_kappa <- data.frame(
  SRX_tool = names(df_kappa),
  kappa = unlist(df_kappa)
)

# View the result
print(df_kappa)












# Expand the dataframe to repeat rows based on the Count.of.row
df_expanded <- df %>%
  ungroup() %>%
  split(.$SRX_tool) %>%
  map(function(x) {
    x$Count.of.row <- as.integer(x$Count.of.row)
    x %>% 
      uncount(Count.of.row, .id = NULL) %>% 
      select(SRX_tool, Tag.org, Tag.tool)
  }) %>%
  bind_rows()

calculate_kappa <- function(df) {
  # Create a table for the kappa calculation
  table_matrix <- table(df$Tag.org, df$Tag.tool)
  
  # Calculate the kappa statistic
  kappa <- kappa2(table_matrix, weight = "unweighted")$value
  
  return(kappa)
}

# Group by SRX_tool and calculate kappa for each group
df_kappa <- df_expanded %>%
  group_by(SRX_tool) %>%
  summarise(kappa = calculate_kappa(cur_data()))


df_test <-df_expanded%>%
  filter(SRX_tool == "SR1_Elicit")

table_matrix <- table(df_test$Tag.org, df_test$Tag.tool)



calculate_kappa(df_test)
# View the result
print(df_kappa)