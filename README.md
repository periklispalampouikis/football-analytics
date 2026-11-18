# Football analytics

Opponent analysis and match reports, built with R on event and tracking data.
Each folder is one self-contained piece: a script that reproduces every figure, and the report.

Author: **Periklis Palampouikis**, statistician · [LinkedIn](https://www.linkedin.com/in/periklis-palampouikis-b2955538a)

## Contents

| Folder | Question | Data |
| --- | --- | --- |
| [`debrief-04-width`](debrief-04-width/) | Which side do Kestrel Bay attack down, and what does the first chart leave out? | Fictional practice match (The Portal) |

## How to run

1. Install R and the packages:
   ```r
   install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "scales", "ggsoccer"),
                    repos = "https://cloud.r-project.org")
   ```
2. Put the data CSVs in `data/raw/portal/` (data is not shared in this repo).
3. Open the project folder in RStudio and run the script with **Source** (Ctrl+Shift+S).
   Charts are saved to `outputs/`.

## Principles

- Every claim in a report points to a figure or a number.
- Definitions are stated, and results are checked for how much they depend on them.
- Uncertainty is shown, not hidden: small samples get intervals and cautious wording.
