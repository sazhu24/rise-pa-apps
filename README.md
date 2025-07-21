# RISE PA Application Parser

This repository contains R scripts for extracting and cleaning data from RISE PA grant application PDFs.

## Contents
- `process_app.R`: Main script to run the parser and export cleaned data
- `clean_intro_app.R`: Functions to extract fields from the introduction section
- `clean_supplemental_app.R`: Functions to extract fields from the supplemental section (work in progress)

## Requirements
- Packages: tidyverse, pdftools, openxlsx

## Usage
1. Open `process_app.R` and set your working directory to this folder.
2. Run the script to extract, clean, and export the cleaned application data the outputs folder.

## Notes & Disclaimers
- **Parsed output may contain errors.** Please review results carefully and revise as needed.
- **Checkboxes in the PDF are not currently detected by the parser.** The current application PDF appears to be a secured document that restricts editing and data extraction, making it incompatible with tools like Python's PyPDF2. As a result, the parser cannot detect checkbox values or access structured form fields. Consider investigating whether an alternative export format is available to improve functionality and reliability.
- The supplemental app parser is a work in progress but may serve as a helpful starting point for additional development.
- The scripts have only been tested on a single PDF, in which many answers were left blank. Because blank responses affect spacing, the parser may not produce consistent results when run on PDFs with fully completed responses. Testing on additional PDFs is strongly recommended before broader deployment.

