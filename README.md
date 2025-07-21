# RISE PA Application Parser

This repository contains scripts to extract and clean data from RISE PA grant application PDFs.

## Contents
- `process_app.R`: Main script to process intro and supplemental sections
- `clean_intro_app.R`: Functions for parsing the introduction section
- `clean_supplemental_app.R`: Functions for parsing the supplemental section

## Requirements
- Packages: tidyverse, pdftools, openxlsx

## Usage
1. Open `process_app.R` and update the file path to set this folder as your working directory
2. Then run the script to extract, clean, and export the application data.

## Notes & Disclaimers
- Scripts are currently not able to detect check boxes in the PDF.
- Output may contain errors. Please review carefully and revise code as needed.
- The supplemental application script is under development, but may serve as a starting point for extracting additional content.