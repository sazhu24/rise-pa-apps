
# === RISE PA Application Parser ===
# This script extracts and cleans the Introduction and Supplemental sections
# of a RISE PA grant application PDF and exports a cleaned Excel workbook.

# USAGE:
# - Place the target PDF file in the working directory
# - Run the script

# === Libraries ===
library(tidyverse)
library(openxlsx)
library(pdftools)

# === Set Working Directory ===
setwd("C:/Users/63526/OneDrive - ICF/RISE-PA")

# === Load Functions ===
source("clean_intro_app.R")
source("clean_supplemental_app.R")

# === Define Inputs ===
pdf_path <- "inputs/ICF Test Application.pdf"
if (!file.exists(pdf_path)) stop("PDF file not found: ", pdf_path)

# === Read PDF and Parse Lines ===
pdf_text_raw <- pdf_text(pdf_path)
all_lines <- pdf_text_raw %>%
  paste(collapse = "\n") %>%
  str_split("\n") %>%
  unlist() %>%
  trimws()

# === Clean Sections ===
intro_sections <- split_intro_tables(all_lines)
intro_cleaned <- parse_intro_sections(intro_sections)

# Load remove_rows (optional)
remove_rows <- if (file.exists("inputs/remove_rows.xlsx")) {
  read.xlsx("inputs/remove_rows.xlsx") %>% filter(remove == "x")
} else {
  NULL
}

supplemental_cleaned <- clean_supplemental_app(all_lines, remove_rows)

# === Export ===
output_file <- str_glue("outputs/cleaned_application_{Sys.Date()}.xlsx")
write.xlsx(
  list(
    "Introduction" = intro_cleaned,
    "Supplemental Application" = supplemental_cleaned
  ),
  output_file
)

cat("Export complete:", output_file, "\n")
