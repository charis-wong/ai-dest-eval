library(dplyr)

data <- read.csv("evaluation/annotation_analysis/annotation_comparison_all.csv")

summary_table <- data %>%
  group_by(Evaluation, Category) %>%
  summarise(
    Percentage_Match = round(mean(Match_original) * 100, 1),
    Number_of_Comparisons = n()
  )

# Pivot the data
pivoted_table <- summary_table %>%
  pivot_wider(
    id_cols = Evaluation,
    names_from = Category,
    values_from = c(Percentage_Match, Number_of_Comparisons),
    names_sep = "_"
  )



# Calculate overall percentage match
overall_percentage_match <- summary_table %>%
  group_by(Evaluation) %>%
  summarise(
    Overall_Percentage_Match = round(mean(Percentage_Match), 1),
    Overall_Number_of_Comparisons = sum(Number_of_Comparisons)
  )

# Merge the pivoted table with the overall percentage match
merged_table <- merge(pivoted_table, overall_percentage_match, by = "Evaluation")

# Create a new dataframe with the desired format
correctness_table <- data.frame(
  Evaluation = merged_table$Evaluation,
  Population = paste0(merged_table$Percentage_Match_Population, "% (", merged_table$Number_of_Comparisons_Population, ")"),
  Intervention = paste0(merged_table$Percentage_Match_Intervention, "% (", merged_table$Number_of_Comparisons_Intervention, ")"),
  Comparison = paste0(merged_table$Percentage_Match_Comparison, "% (", merged_table$Number_of_Comparisons_Comparison, ")"),
  Outcome = paste0(merged_table$Percentage_Match_Outcome, "% (", merged_table$Number_of_Comparisons_Outcome, ")"),
  Method = paste0(merged_table$Percentage_Match_Method, "% (", merged_table$Number_of_Comparisons_Method, ")"),
  Result = paste0(merged_table$Percentage_Match_Result, "% (", merged_table$Number_of_Comparisons_Result, ")"),
  Overall = paste0(merged_table$Overall_Percentage_Match, "% (", merged_table$Overall_Number_of_Comparisons, ")")
)


write.csv(correctness_table, "output/overall_annotation_comparison_analysis.csv", row.names = FALSE)

completeness <- data %>%
  group_by(Study, Evaluation, Category)%>%
  summarise(all_match = all(Match_original))

completeness_table <- completeness %>%
  group_by(Evaluation, Category) %>%
  summarise(
    Percentage_Match = round(mean(all_match) * 100, 1),
    Number_of_Studies = n()
  )

pivoted_completeness_table <- completeness_table %>%
  pivot_wider(
    id_cols = Evaluation,
    names_from = Category,
    values_from = c(Percentage_Match, Number_of_Studies),
    names_sep = "_"
  )


# Create a new dataframe with the desired format
final_completeness_table <- data.frame(
  Evaluation = pivoted_completeness_table$Evaluation,
  Population = paste0(pivoted_completeness_table$Percentage_Match_Population, "% (", pivoted_completeness_table$Number_of_Studies_Population, ")"),
  Intervention = paste0(pivoted_completeness_table$Percentage_Match_Intervention, "% (", pivoted_completeness_table$Number_of_Studies_Intervention, ")"),
  Comparison = paste0(pivoted_completeness_table$Percentage_Match_Comparison, "% (", pivoted_completeness_table$Number_of_Studies_Comparison, ")"),
  Outcome = paste0(pivoted_completeness_table$Percentage_Match_Outcome, "% (", pivoted_completeness_table$Number_of_Studies_Outcome, ")"),
  Method = paste0(pivoted_completeness_table$Percentage_Match_Method, "% (", pivoted_completeness_table$Number_of_Studies_Method, ")"),
  Result = paste0(pivoted_completeness_table$Percentage_Match_Result, "% (", pivoted_completeness_table$Number_of_Studies_Result, ")")

  
)
write.csv(final_completeness_table, "output/overall_annotation_completeness_analysis.csv", row.names = FALSE)





additional_tags <- data%>%
  group_by(Evaluation, Category, Additional_tags)%>%
  summarise(n = n())

# Calculate the total number of studies for each evaluation and category
total_studies <- additional_tags %>%
  group_by(Evaluation, Category) %>%
  summarise(Total_n = sum(n))

# Calculate the number of studies with additional tags for each evaluation and category
additional_tags_studies <- additional_tags %>%
  filter(Additional_tags != "No") %>%
  group_by(Evaluation, Category) %>%
  summarise(Additional_tags_n = sum(n))

# Merge the total studies and additional tags studies data frames
merged_additional_tags_data <- merge(total_studies, additional_tags_studies, by = c("Evaluation", "Category"), all.x = TRUE)

# Calculate the percentage of studies with additional tags for each evaluation and category
merged_additional_tags_data$Percentage_Additional_tags <- (merged_additional_tags_data$Additional_tags_n / merged_additional_tags_data$Total_n) * 100

# Calculate the number of studies with appropriate additional tags for each evaluation and category
appropriate_additional_tags_studies <- additional_tags %>%
  filter(Additional_tags == "Yes and appropriate") %>%
  group_by(Evaluation, Category) %>%
  summarise(Appropriate_Additional_Tags_n = sum(n))

# Merge the merged data frame with the appropriate additional tags studies data frame
final_additional_tags_data <- merge(merged_additional_tags_data, appropriate_additional_tags_studies, by = c("Evaluation", "Category"), all.x = TRUE)

# Calculate the percentage of studies with appropriate additional tags for each evaluation and category
final_additional_tags_data$Percentage_Appropriate_Additional_Tags <- (final_additional_tags_data$Appropriate_Additional_Tags_n / final_additional_tags_data$Additional_tags_n) * 100

final_additional_tags_data$Percentage_Appropriate_Additional_Tags[is.na(final_additional_tags_data$Percentage_Appropriate_Additional_Tags)]<-0

# Pivot the data to get the desired format
final_additional_tags_table <- final_additional_tags_data %>%
  select(Evaluation, Category, Percentage_Additional_tags, Percentage_Appropriate_Additional_Tags) %>%
  pivot_wider(
    id_cols = Evaluation,
    names_from = Category,
    values_from = c(Percentage_Additional_tags, Percentage_Appropriate_Additional_Tags),
    names_sep = "_"
  )

# Create a new data frame with the desired format
final_additional_tags_summary_table <- data.frame(
  Evaluation = final_additional_tags_table$Evaluation,
  Population = paste0(round(final_additional_tags_table$Percentage_Additional_tags_Population, 1), "% (", round(final_additional_tags_table$Percentage_Appropriate_Additional_Tags_Population, 1), "%)"),
  Intervention = paste0(round(final_additional_tags_table$Percentage_Additional_tags_Intervention, 1), "% (", round(final_additional_tags_table$Percentage_Appropriate_Additional_Tags_Intervention, 1), "%)"),
  Comparison = paste0(round(final_additional_tags_table$Percentage_Additional_tags_Comparison, 1), "% (", round(final_additional_tags_table$Percentage_Appropriate_Additional_Tags_Comparison, 1), "%)"),
  Outcome = paste0(round(final_additional_tags_table$Percentage_Additional_tags_Outcome, 1), "% (", round(final_additional_tags_table$Percentage_Appropriate_Additional_Tags_Outcome, 1), "%)"),
  Method = paste0(round(final_additional_tags_table$Percentage_Additional_tags_Method, 1), "% (", round(final_additional_tags_table$Percentage_Appropriate_Additional_Tags_Method, 1), "%)"),
  Result = paste0(round(final_additional_tags_table$Percentage_Additional_tags_Result, 1), "% (", round(final_additional_tags_table$Percentage_Appropriate_Additional_Tags_Result, 1), "%)")

)

write.csv(final_additional_tags_summary_table, "output/overall_additional_tags_analysis.csv", row.names = FALSE)

