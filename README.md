# BSA

[R](https://cran.r-project.org/) code to analyse complaint/decision data from the New Zealand Broadcasting Standards Authority.

## Data

Data was requested from the BSA on 5 August 2023. The original request was for aggregated decision data appearing in the public [Decisions database](https://www.bsa.govt.nz/decisions/all-decisions/). The BSA sent me a dataset containing decision data created under their current [codebook](https://www.bsa.govt.nz/broadcasting-standards/broadcasting-code-book-2022/) which came into effect 1 July 2022 (`20230807 AllClosedFormalComplaints.xlsx`). I then requested the equivalent data created under the previous BSA codebook, in effect between 1 April 2016 to 30 June 2022, and the BSA provided this to me (`20230810 AllClosedFormalComplaints.xlsx`).

Because the datasets were created under different codebooks, they vary slightly in their contents and structure. As part of my analyses, I have restructured and merged the two datasets into a single spreadsheet following conventions used in the dataset created from the current codebook.

The BSA is an independent Crown entity not subject to the NZGOAL framework, but agreed to allow reuse of the data on terms equivalent to NZGOAL (Creative Commons Attribution 4.0). The raw data will be published on Figshare once analyses are concluded.

## Reproducible analysis with Quarto

R code is embedded within executable chunks in `bsa.qmd`, a [Quarto](https://quarto.org/) Markdown document. Quarto is an open-source scientific and technical publishing system built on Pandoc. To run analyses and generate figures:

1. Clone this repository.*
2. Open `bsa.qmd` in RStudio and 'Run all' (`ctrl`+`alt`+`R`).

*see [here](https://happygitwithr.com/) for instructions for setting up Git/GitHub with RStudio.

## Licence

All code in this repository is licensed under the [MIT license](LICENSE).