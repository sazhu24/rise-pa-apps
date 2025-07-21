
# Parse Introduction Section of RISE PA Application

# === Extract table from list of text lines ===
extract_large_table <- function(lines, start_pattern, end_pattern, max_cols = 10) {
  start_idx <- grep(start_pattern, lines)
  end_idx <- grep(end_pattern, lines)
  
  if (length(start_idx) == 0 | length(end_idx) == 0) {
    warning("Start or end pattern not found.")
    return(NULL)
  }
  
  section <- lines[start_idx[1]:end_idx[length(end_idx)]]
  
  str_split_fixed(section, " {2,}", max_cols) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    select(where(~ !all(.x == ""))) %>%
    filter(!if_all(everything(), ~ is.na(.x) | .x == ""))
}

# === Split introduction block into section tables === 
split_intro_tables <- function(lines) {
  # Extract full intro table
  intro_df <- extract_large_table(lines, start_pattern = "Application #:", end_pattern = "PROJECT ADDENDA")
  
  # Extract Application ID
  app_id <- intro_df[1, ] %>%
    select(V1) %>%
    mutate(V1 = as.numeric(str_extract(V1, "[0-9.]+"))) %>%
    rename("Application Number" = V1)
  
  # Find section boundaries (I., II., etc.)
  split_rows <- which(str_detect(intro_df[[1]], "^(I|V)([A-Z]*)\\."))
  split_indices <- c(split_rows, nrow(intro_df))
  
  # Split into subtables and drop header row from each
  tables <- map2(split_indices[-length(split_indices)], split_indices[-1] - 1, function(start, end) {
    sub_df <- intro_df[start:end, ] %>%
      filter(!if_all(everything(), ~ is.na(.x) | .x == ""))
    sub_df[-1, ]  # remove the title row
  })
  
  # Add app_id as named element to the list
  names(tables) <- paste0("section_", seq_along(tables))
  tables$app_id <- app_id
  
  return(tables)
}

# === Reshape key-value table to wide ===
reshape_table <- function(df) {
  long <- df %>%
    pivot_longer(cols = everything(), values_to = "value") %>%
    filter(!is.na(value)) %>%
    pull(value)
  
  tibble(
    key = ifelse(str_detect(long, ":$"), str_remove(long, ":$"), NA),
    value = ifelse(!str_detect(long, ":$"), long, NA)
  ) %>%
    fill(key, .direction = "down") %>%
    filter(!is.na(value)) %>%
    mutate(record_id = cumsum(key == "Name")) %>%
    pivot_wider(
      id_cols = record_id,
      names_from = key,
      values_from = value,
      values_fn = ~ .x[1]
    ) %>%
    select(-record_id)
}

# === Section I: Profiles ===
parse_section_profiles <- function(table1) {
  name_rows <- which(table1$V1 == "Name:")
  if (length(name_rows) >= 2) {
    table1 <- table1[1:(name_rows[2] - 1), ]
  }
  reshape_table(table1)
}

# === Section II: Site Locations ===
parse_section_site <- function(table2) {
  site_info <- str_extract(table2[1, 1], "(?<=Site Information:).*") %>% str_trim()
  table2_clean <- table2[-1, ] %>%
    reshape_table() %>%
    setNames(str_glue("Site {site_info} - {names(.)}"))
  table2_clean
}

# === Section III: Project Info ===
parse_section_projectinfo <- function(table3) {
  table3_clean <- reshape_table(table3)[, -1]
  names(table3_clean) <- c("Talked to Agency Contact", "Project Name")
  table3_clean
}

# === Section VI: GAT Data ===
parse_section_gat <- function(table6) {
  reshape_table(table6)
}

# === Section VII: Budget ===
parse_section_budget <- function(table7) {
  start_row <- which(table7$V1 == "RISE PA Program")
  end_row <- which(table7$V1 == "Total")
  
  table7a <- table7[(start_row + 1):(end_row - 1), ] %>%
    select(where(~ !all(is.na(.) | str_trim(.) == ""))) %>%
    select(-ncol(.)) %>%
    mutate(
      V1 = str_trim(gsub("\\s*\\([^\\)]+\\)", "", V1)),
      across(!V1, ~ as.numeric(str_extract(.x, "[0-9.]+")))
    )
  names(table7a) <- c("Item", "Budget RISE PA", "Budget Match Private")
  
  table7a_clean <- table7a %>%
    pivot_wider(names_from = "Item", values_from = c("Budget RISE PA", "Budget Match Private"))
  
  table7b <- data.frame(
    "Budget Total" = as.numeric(str_extract(table7[which(table7$V1 == "Budget Total:"), 2], "[0-9.]+")),
    check.names = FALSE
  )
  
  table7c <- data.frame(
    "Budget Narrative" = table7[nrow(table7), 1],
    check.names = FALSE
  )
  
  bind_cols(table7a_clean, table7b, table7c)
}

# === Section IX: Narrative Summary ===
parse_section_narrative <- function(table9) {
  start_row <- which(table9$V1 == "MANAGEMENT SECTION")
  end_row <- which(str_detect(table9$V1, "^PROGRAM NAME"))
  
  table9a <- data.frame("Management Summary" = table9[2, 1])
  
  table9b <- table9[(start_row + 1):(end_row - 1), ] %>%
    reshape_table()
  
  table9c <- table9[(end_row + 1):nrow(table9), ]
  colnames(table9c) <- table9c[1, ]
  table9c <- table9c[-1, ]
  
  bind_cols(table9a, table9b, table9c)
}

# === Master function ===
parse_intro_sections <- function(tables_list) {
  
  app_id <- tables_list[[10]]
  table1_clean <- parse_section_profiles(tables_list[[1]])
  table2_clean <- parse_section_site(tables_list[[2]])
  table3_clean <- parse_section_projectinfo(tables_list[[3]])
  table6_clean <- parse_section_gat(tables_list[[6]])
  table7_clean <- parse_section_budget(tables_list[[7]])
  table9_clean <- parse_section_narrative(tables_list[[9]])
  
  bind_cols(
    app_id,
    table1_clean,
    table2_clean,
    table3_clean,
    table6_clean,
    table7_clean,
    table9_clean
  )
}
