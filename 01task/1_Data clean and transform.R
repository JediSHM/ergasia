# =============================================================================
# PROJECT 2 - TASK 1: Clean and transform raw IMDb tables
#
# Run this after 0_Get_data.R. It preserves source text, keeps missing values as
# NA (never zero-fills missing money/ratings), and writes CSVs for Power BI.
# =============================================================================

required_packages <- c("dplyr", "stringr", "tidyr", "readr", "tibble", "purrr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Install these packages in RStudio first: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(purrr)
})

project_dir <- getwd()
raw_dir <- file.path(project_dir, "data_raw")
clean_dir <- file.path(project_dir, "data_clean")
dir.create(clean_dir, showWarnings = FALSE, recursive = TRUE)

read_raw <- function(file_name) {
  path <- file.path(raw_dir, file_name)
  if (!file.exists(path)) stop("Missing raw file: ", path, ". Run Task 0 first.")
  readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
}

clean_missing_text <- function(x) {
  x <- stringr::str_squish(as.character(x))
  dplyr::na_if(x, "")
}

parse_count <- function(x) {
  x <- clean_missing_text(x)
  upper <- stringr::str_to_upper(x)
  multiplier <- dplyr::case_when(
    stringr::str_detect(upper, "K\\s*$") ~ 1000,
    stringr::str_detect(upper, "M\\s*$") ~ 1000000,
    stringr::str_detect(upper, "B\\s*$") ~ 1000000000,
    TRUE ~ 1
  )
  number_text <- stringr::str_remove(upper, "[KMB]\\s*$")
  number <- suppressWarnings(readr::parse_number(number_text))
  number * multiplier
}

parse_numeric <- function(x) {
  suppressWarnings(readr::parse_number(clean_missing_text(x)))
}

parse_money_marker <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    stringr::str_detect(x, fixed("€")) ~ "EUR symbol",
    stringr::str_detect(x, fixed("£")) ~ "GBP symbol",
    stringr::str_detect(x, fixed("$")) ~ "Dollar symbol (currency not confirmed)",
    stringr::str_detect(stringr::str_to_upper(x), "\\b(EUR|EURO)\\b") ~ "EUR label",
    stringr::str_detect(stringr::str_to_upper(x), "\\b(GBP|POUND)\\b") ~ "GBP label",
    stringr::str_detect(stringr::str_to_upper(x), "\\b(USD|DOLLAR)\\b") ~ "USD label",
    TRUE ~ "Currency not stated"
  )
}

split_values <- function(data, source_column, value_column) {
  result <- data %>%
    transmute(
      tconst,
      .value = stringr::str_split(
        dplyr::coalesce(.data[[source_column]], ""), stringr::fixed("|")
      )
    ) %>%
    tidyr::unnest_longer(.value, indices_to = ".position") %>%
    mutate(.value = clean_missing_text(.value)) %>%
    filter(!is.na(.value)) %>%
    distinct(tconst, .value, .keep_all = TRUE)
  names(result)[names(result) == ".value"] <- value_column
  names(result)[names(result) == ".position"] <- "item_order"
  result
}

# ------------------------------- Read raw data -------------------------------
movies_raw <- read_raw("movies_raw.csv")
cast_raw <- read_raw("cast_raw.csv")
votes_raw <- read_raw("vote_distribution_raw.csv")
contributors_raw <- read_raw("movie_contributors_raw.csv")
lineage_raw <- read_raw("movie_lineage_raw.csv")
discovery_edges_raw <- read_raw("discovery_edges_raw.csv")
failures_raw <- read_raw("scrape_failures_raw.csv")
seed_registry <- readr::read_csv(
  file.path(project_dir, "seed_registry.csv"),
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

if (nrow(movies_raw) == 0) stop("movies_raw.csv is empty. Complete Task 0 first.")

# ------------------------- Main movie fact table -----------------------------
movies <- movies_raw %>%
  mutate(across(where(is.character), clean_missing_text)) %>%
  filter(!is.na(tconst)) %>%
  arrange(tconst) %>%
  distinct(tconst, .keep_all = TRUE)

FactMovie <- movies %>%
  transmute(
    tconst,
    movie_name,
    release_date_raw = date_published_raw,
    release_year = suppressWarnings(as.integer(stringr::str_extract(date_published_raw, "[0-9]{4}"))),
    imdb_rating = parse_numeric(imdb_rating_raw),
    total_votes = parse_count(imdb_votes_raw),
    genres_raw,
    production_companies_raw,
    budget_raw,
    budget_amount = parse_count(budget_raw),
    budget_currency_marker = parse_money_marker(budget_raw),
    gross_us_canada_raw,
    gross_us_canada_amount = parse_count(gross_us_canada_raw),
    gross_us_canada_currency_marker = parse_money_marker(gross_us_canada_raw),
    gross_worldwide_raw,
    gross_worldwide_amount = parse_count(gross_worldwide_raw),
    gross_worldwide_currency_marker = parse_money_marker(gross_worldwide_raw),
    description_raw,
    content_rating = content_rating_raw
  )

# ---------------------- Genre and company dimensions -------------------------
genre_values <- split_values(FactMovie, "genres_raw", "genre_name")
DimGenre <- genre_values %>%
  distinct(genre_name) %>%
  arrange(genre_name) %>%
  mutate(genre_id = paste0("G", sprintf("%03d", row_number())), .before = 1)
BridgeMovieGenre <- genre_values %>%
  left_join(DimGenre, by = "genre_name") %>%
  select(tconst, genre_id, genre_order = item_order) %>%
  distinct()

company_values <- split_values(FactMovie, "production_companies_raw", "company_name")
DimCompany <- company_values %>%
  distinct(company_name) %>%
  arrange(company_name) %>%
  mutate(company_id = paste0("C", sprintf("%03d", row_number())), .before = 1)
BridgeMovieCompany <- company_values %>%
  left_join(DimCompany, by = "company_name") %>%
  select(tconst, company_id, company_order = item_order) %>%
  distinct()

# ------------------------- Cast and actor dimension --------------------------
cast_clean <- cast_raw %>%
  mutate(
    actor_id = clean_missing_text(actor_id),
    actor_name = clean_missing_text(actor_name),
    actor_order = suppressWarnings(as.integer(actor_order))
  ) %>%
  filter(!is.na(tconst), !is.na(actor_id), !is.na(actor_name)) %>%
  distinct(tconst, actor_id, .keep_all = TRUE)

DimActor <- cast_clean %>%
  arrange(actor_id, actor_order) %>%
  distinct(actor_id, .keep_all = TRUE) %>%
  select(actor_id, actor_name)
BridgeMovieActor <- cast_clean %>%
  select(tconst, actor_id, actor_order) %>%
  distinct()

# ----------------------- Contributor and lineage -----------------------------
movie_contributors <- contributors_raw %>%
  mutate(
    member_id = clean_missing_text(member_id),
    seed_movie_id = clean_missing_text(seed_movie_id),
    first_discovery_round = suppressWarnings(as.integer(first_discovery_round))
  ) %>%
  filter(!is.na(member_id), !is.na(tconst)) %>%
  distinct(member_id, tconst, .keep_all = TRUE)

DimContributor <- bind_rows(
  seed_registry %>% transmute(member_id, seed_movie_id),
  movie_contributors %>% select(member_id, seed_movie_id)
) %>%
  mutate(across(everything(), clean_missing_text)) %>%
  filter(!is.na(member_id)) %>%
  distinct(member_id, .keep_all = TRUE) %>%
  arrange(member_id)

BridgeMovieContributor <- movie_contributors %>%
  select(tconst, member_id, seed_movie_id, first_discovery_round) %>%
  distinct()

MovieFirstDiscovery <- lineage_raw %>%
  mutate(
    member_id = clean_missing_text(member_id),
    seed_movie_id = clean_missing_text(seed_movie_id),
    parent_tconst = clean_missing_text(parent_tconst),
    tconst = clean_missing_text(tconst),
    discovery_round = suppressWarnings(as.integer(discovery_round)),
    is_seed = as.logical(is_seed)
  ) %>%
  filter(!is.na(member_id), !is.na(tconst)) %>%
  distinct(member_id, seed_movie_id, parent_tconst, tconst, discovery_round, .keep_all = TRUE)

MovieDiscoveryEdges <- discovery_edges_raw %>%
  mutate(
    member_id = clean_missing_text(member_id),
    seed_movie_id = clean_missing_text(seed_movie_id),
    parent_tconst = clean_missing_text(parent_tconst),
    recommended_tconst = clean_missing_text(recommended_tconst),
    discovery_round = suppressWarnings(as.integer(discovery_round))
  ) %>%
  filter(!is.na(member_id), !is.na(parent_tconst), !is.na(recommended_tconst)) %>%
  distinct(member_id, seed_movie_id, parent_tconst, recommended_tconst, discovery_round)

# -------------------------- Vote distribution fact ----------------------------
FactVoteDistribution <- votes_raw %>%
  mutate(
    score = suppressWarnings(as.integer(score)),
    percentage = parse_numeric(stringr::str_remove(percentage_raw, "%")),
    vote_count = parse_count(vote_count_raw),
    extraction_status = clean_missing_text(extraction_status),
    raw_row_text = clean_missing_text(raw_row_text)
  ) %>%
  filter(!is.na(tconst)) %>%
  select(tconst, score, vote_count, percentage, extraction_status, raw_row_text) %>%
  distinct()

# ----------------------- Coverage and missingness ----------------------------
CoverageByMember <- DimContributor %>%
  left_join(
    BridgeMovieContributor %>% count(member_id, name = "unique_movies_collected"),
    by = "member_id"
  ) %>%
  mutate(unique_movies_collected = tidyr::replace_na(unique_movies_collected, 0L))

DataQualitySummary <- tibble::tibble(
  measure = c(
    "Unique movies", "Movies missing title", "Movies missing IMDb rating",
    "Movies missing total votes", "Movies missing budget text",
    "Movies missing worldwide gross text", "Movies with cast rows",
    "Movies with vote histogram rows", "Scrape failure rows",
    "Unique member/movie contributions", "Recommendation lineage edges"
  ),
  value = c(
    nrow(FactMovie),
    sum(is.na(FactMovie$movie_name)),
    sum(is.na(FactMovie$imdb_rating)),
    sum(is.na(FactMovie$total_votes)),
    sum(is.na(FactMovie$budget_raw)),
    sum(is.na(FactMovie$gross_worldwide_raw)),
    dplyr::n_distinct(BridgeMovieActor$tconst),
    dplyr::n_distinct(FactVoteDistribution$tconst[
      FactVoteDistribution$extraction_status %in% "row_found"
    ]),
    nrow(failures_raw),
    nrow(BridgeMovieContributor),
    nrow(MovieDiscoveryEdges)
  )
)

# --------------------------------- Export ------------------------------------
tables <- list(
  FactMovie = FactMovie,
  DimGenre = DimGenre,
  BridgeMovieGenre = BridgeMovieGenre,
  DimActor = DimActor,
  BridgeMovieActor = BridgeMovieActor,
  DimCompany = DimCompany,
  BridgeMovieCompany = BridgeMovieCompany,
  DimContributor = DimContributor,
  BridgeMovieContributor = BridgeMovieContributor,
  FactVoteDistribution = FactVoteDistribution,
  MovieFirstDiscovery = MovieFirstDiscovery,
  MovieDiscoveryEdges = MovieDiscoveryEdges,
  CoverageByMember = CoverageByMember,
  DataQualitySummary = DataQualitySummary,
  ScrapeFailures = failures_raw
)

purrr::iwalk(tables, function(table, table_name) {
  readr::write_csv(table, file.path(clean_dir, paste0(table_name, ".csv")), na = "")
})
clean_data <- tables
save(clean_data, file = file.path(clean_dir, "imdb_project_clean.RData"))

message("Task 1 finished. Clean CSV tables and imdb_project_clean.RData are in data_clean/.")
message("Review CoverageByMember.csv and DataQualitySummary.csv before Power BI.")
