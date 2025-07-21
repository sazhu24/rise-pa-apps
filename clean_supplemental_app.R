
# Parse Supplemental Application Section of RISE PA Application

clean_supplemental_app <- function(lines, remove_rows) {
  
  # === 1. Extract table ===
  supp_df <- extract_large_table(
    lines,
    start_pattern = "SUPPLEMENTAL APPLICATION",
    end_pattern = "Signed Consent Form Upload"
  )
  
  # === 2. Remove date and URL rows ===
  filter_patterns <- c("^https://dep.grants.pa.gov", "^[0-9]{1,2}/[0-9]{1,2}/25") %>%
    paste(collapse = "|")
  
  supp_df <- supp_df %>%
    filter(!str_detect(V1, filter_patterns))
  
  # === 3. Identify section headers ===
  section_headers <- c(
    "Project Overview", "Company Overview", "Project Scope", "Project Team",
    "Project Benefits and Impact", "Permitting", "Project Innovation/Transformative Impact",
    "Stakeholder Engagement", "Project-Specific Questions", "As-a-Service Provider Questions",
    "Technical Appendix", "Detailed Budget Information", "Fair Labor Bonus",
    "Greenhouse Gas Emissions Reduction Bonus", "Additional Information"
  ) %>%
    paste(collapse = "|")
  
  split_rows <- which(str_detect(supp_df[[1]], section_headers))
  split_indices <- c(split_rows, nrow(supp_df) + 1)
  
  # === 4. Split and clean each section ===
  supp_tables <- map2(split_indices[-length(split_indices)], split_indices[-1] - 1, function(start, end) {
    sub_df <- supp_df[start:end, ] %>%
      filter(!if_all(everything(), ~ is.na(.x) | .x == ""))
    
    title <- sub_df[1, 1]
    sub_df <- sub_df[-1, ] %>%
      mutate(
        Question = if_else(str_detect(V1, "^\\s*\\d{1,2}\\."), V1, NA_character_),
        Subquestion = if_else(str_detect(V1, "^\\s*[a-z]\\."), V1, NA_character_),
        V1 = if_else(str_detect(V1, "^\\s*(\\d{1,2}|[a-z])\\."), NA_character_, V1),
        Number = str_extract(Question, "^\\s*\\d{1,2}")
      ) %>%
      fill(Number, Question, .direction = "down") %>%
      group_by(Question) %>%
      fill(Subquestion, .direction = "down") %>%
      ungroup() %>%
      filter(!(is.na(V1) & !is.na(Question) & is.na(Subquestion))) %>%
      mutate(Section = title) %>%
      relocate(Section, Number, Question, Subquestion)
  })
  
  supp_tables <- supp_tables[c(-1, -2)]
  
  # === 5. Assign cleaned names ===
  section_names <- c(
    "project_overview", "company_overview", "project_scope", "project_team",
    "project_benefits", "permitting", "project_innovation", "stakeholder_engagement",
    "project_specific_questions", "as_a_service", "technical_appendix", "budget_info",
    "fair_labor_bonus", "ghg_reduction_bonus", "additional_info"
  )
  
  names(supp_tables) <- section_names
  
  # === 6. Add Subsection tags ===
  subsections <- c(
    "Energy Efficiency", "Electrification", "Industrial Process Emissions Reduction",
    "Low-carbon Fuel Switching Technologies", "On-site Renewable Energy",
    "Carbon Capture, Utilization, and Storage",
    "Carbon Capture and Storage", "Carbon Capture and Utilization",
    "Methane Capture", "Methane Destruction", "Methane Utilization"
  ) %>% paste(collapse = "|")
  
  supp_tables$project_specific_questions <- supp_tables$project_specific_questions %>%
    mutate(Subsection = if_else(str_detect(V1, paste0("^(", subsections, ")")), V1, NA_character_)) %>%
    fill(Subsection, .direction = "down") %>%
    relocate(Subsection, .after = Section)
  
  # === 8. Combine all sections ===
  df_combined <- bind_rows(supp_tables)
  
  # === 9. Final cleanup (optional: remove flagged rows)
  supplemental_app_cleaned <- df_combined %>%
    relocate(Subsection, .after = Section)
  
  if (exists("remove_rows")) {
    supplemental_app_cleaned <- anti_join(
      supplemental_app_cleaned,
      remove_rows,
      by = join_by(Section, Subsection, Number, Question, Subquestion, V1)
    )
  }
  
  return(supplemental_app_cleaned)
}
