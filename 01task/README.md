# IMDb Project 2 - Tasks 0 and 1

This folder contains two R scripts and the seed registry configured with the four seed IDs you sent. The contributor names are temporary labels (`member_1` to `member_4`); replace them with your team's agreed identifiers if needed.

## Files

- `0_Get data.R` crawls IMDb from each seed using `rvest`, collects movie details, up to four cast members in page order, vote histogram rows, box office text, production company names, and recommendation links.
- `1_Data clean and transform.R` cleans the raw files and exports fact, dimension, bridge, lineage, coverage, and data quality tables for Power BI.
- `seed_registry.csv` contains one different seed per temporary member.

## Run in RStudio

1. Copy these files into your project folder. Keep `seed_registry.csv` in the project root. The scripts use the current R working directory as the project folder.
2. In the RStudio Console, install packages once if needed:

   ```r
   install.packages(c("rvest", "chromote", "jsonlite", "dplyr", "stringr",
                      "tidyr", "readr", "tibble", "purrr"))
   ```

3. Open `0_Get data.R`, confirm the working directory and seed registry, then run it. It is set to target 500 unique titles per member. The crawl can take a long time because it pauses between requests and visits each movie, credits, and ratings page.
4. If the session stops, rerun Task 0 with the same registry; it resumes from `data_raw/crawl_checkpoint.rds`. Do not edit the seed registry during a run. To start a fresh crawl, move the old `data_raw/` folder out of the project before rerunning.
5. Check `data_raw/scrape_failures_raw.csv`, `data_raw/crawl_queue_raw.csv`, and the per-member coverage. IMDb can change its page markup or limit requests; the scripts record missing fields and failed pages instead of replacing missing values with zero.
6. After Task 0 has produced `movies_raw.csv`, open and run `1_Data clean and transform.R`.

## Outputs

Task 0 writes its raw CSVs and checkpoint under `data_raw/`. Task 1 writes `FactMovie`, `DimGenre`, `BridgeMovieGenre`, `DimActor`, `BridgeMovieActor`, `DimCompany`, `BridgeMovieCompany`, `DimContributor`, `BridgeMovieContributor`, `FactVoteDistribution`, `MovieDiscoveryEdges`, `CoverageByMember`, `DataQualitySummary`, and `ScrapeFailures` under `data_clean/`, plus `imdb_project_clean.RData`.

The bridge tables keep many-to-many relationships. `BridgeMovieContributor` records every member/title contribution, and `MovieDiscoveryEdges` retains the parent-to-recommendation trail. Budget and gross source text is preserved; parsed amounts do not imply a verified currency when IMDb does not state one.

## Before submitting

Run a small check first in a separate copy of the project folder: keep only one row in `seed_registry.csv`, temporarily set `target_per_member <- 1L`, and make sure that copy has no old `data_raw/crawl_checkpoint.rds`. Compare the title, rating, recommendation IDs, cast order, and vote rows with the visible IMDb pages. If expected rows are missing, inspect the relevant selector in Task 0 before starting the larger crawl. This bundle has not been run against IMDb, so live selectors and coverage still need that check in RStudio.

