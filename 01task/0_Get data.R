# =============================================================================
# PROJECT 2 - TASK 0: IMDb snowball scraper
#
# Run this from the project folder in RStudio. Put seed_registry.csv in that
# folder, then run this script. It saves raw tables under data_raw/ and a
# resumable checkpoint after every few titles.
#
# The team is crawled toward one global target of unique films. Member queues
# rotate fairly; metadata are fetched once and contributor lineage is retained.
# =============================================================================

# Required packages. Install once in the RStudio Console if needed:
# install.packages(c("rvest", "chromote", "jsonlite", "dplyr", "stringr",
#                    "tidyr", "readr", "tibble", "purrr"))
required_packages <- c(
  "rvest", "chromote", "jsonlite", "dplyr", "stringr", "tidyr",
  "readr", "tibble", "purrr"
)
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
  library(rvest)
  library(jsonlite)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(purrr)
})

# --------------------------- Student configuration ---------------------------
project_dir <- getwd()
seed_file <- file.path(project_dir, "seed_registry.csv")
raw_dir <- file.path(project_dir, "data_raw")
target_unique_movies <- 2000L
checkpoint_every <- 5L
request_delay_seconds <- c(3, 6)  # Random pause before each IMDb page request

if (!file.exists(seed_file)) {
  stop("Put seed_registry.csv in the current project folder: ", project_dir)
}
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

seed_registry <- readr::read_csv(
  seed_file,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
) %>%
  transmute(
    member_id = stringr::str_squish(member_id),
    seed_movie_id = stringr::str_extract(seed_movie_id, "tt[0-9]+")
  )

if (!all(c("member_id", "seed_movie_id") %in% names(seed_registry)) ||
    nrow(seed_registry) == 0 || any(is.na(seed_registry$member_id)) ||
    any(is.na(seed_registry$seed_movie_id))) {
  stop("seed_registry.csv needs non-empty member_id and IMDb seed_movie_id columns.")
}
if (anyDuplicated(seed_registry$member_id)) {
  stop("Use one seed row per member_id.")
}
if (anyDuplicated(seed_registry$seed_movie_id)) {
  stop("The project brief requires each member to start with a different seed.")
}

checkpoint_path <- file.path(raw_dir, "crawl_checkpoint.rds")

# ------------------------------ Empty datasets -------------------------------
empty_queue <- tibble(
  member_id = character(), seed_movie_id = character(), tconst = character(),
  parent_tconst = character(), discovery_round = integer(), status = character()
)
empty_movies <- tibble(
  tconst = character(), movie_name = character(), date_published_raw = character(),
  genres_raw = character(), imdb_rating_raw = character(), imdb_votes_raw = character(),
  description_raw = character(), content_rating_raw = character(),
  production_companies_raw = character(), budget_raw = character(),
  gross_us_canada_raw = character(), gross_worldwide_raw = character()
)
empty_cast <- tibble(
  tconst = character(), actor_order = integer(), actor_id = character(), actor_name = character()
)
empty_votes <- tibble(
  tconst = character(), score = integer(), percentage_raw = character(),
  vote_count_raw = character(), raw_row_text = character(), extraction_status = character()
)
empty_recommendations <- tibble(parent_tconst = character(), recommended_tconst = character())
empty_contributors <- tibble(
  member_id = character(), seed_movie_id = character(), tconst = character(),
  first_discovery_round = integer()
)
empty_lineage <- tibble(
  member_id = character(), seed_movie_id = character(), parent_tconst = character(),
  tconst = character(), discovery_round = integer(), is_seed = logical()
)
empty_edges <- tibble(
  member_id = character(), seed_movie_id = character(), parent_tconst = character(),
  recommended_tconst = character(), discovery_round = integer()
)
empty_failures <- tibble(
  member_id = character(), tconst = character(), stage = character(),
  url = character(), error_text = character()
)

same_registry <- function(a, b) {
  identical(
    paste(a$member_id, a$seed_movie_id, sep = "::"),
    paste(b$member_id, b$seed_movie_id, sep = "::")
  )
}

if (file.exists(checkpoint_path)) {
  state <- readRDS(checkpoint_path)
  if (!same_registry(state$seed_registry, seed_registry)) {
    stop(
      "The checkpoint belongs to a different seed_registry.csv. Move or remove ",
      "data_raw/crawl_checkpoint.rds before starting a new team run."
    )
  }
  message("Resuming from data_raw/crawl_checkpoint.rds")
} else {
  initial_queue <- seed_registry %>%
    transmute(
      member_id, seed_movie_id,
      tconst = seed_movie_id,
      parent_tconst = NA_character_,
      discovery_round = 0L,
      status = "pending"
    )
  state <- list(
    seed_registry = seed_registry,
    queue = initial_queue,
    movies = empty_movies,
    cast = empty_cast,
    votes = empty_votes,
    recommendations = empty_recommendations,
    contributors = empty_contributors,
    lineage = empty_lineage,
    discovery_edges = empty_edges,
    failures = empty_failures,
    processed_since_checkpoint = 0L
  )
}

# ------------------------------- Helper tools -------------------------------
clean_scalar <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(stringr::str_squish(x))]
  if (length(x) == 0) return(NA_character_)
  paste(unique(stringr::str_squish(x)), collapse = " | ")
}

find_movie_object <- function(x) {
  if (!is.list(x)) return(NULL)
  object_type <- unlist(x[["@type"]], recursive = TRUE, use.names = FALSE)
  if (!is.null(object_type) && "Movie" %in% object_type) return(x)
  for (item in x) {
    found <- find_movie_object(item)
    if (!is.null(found)) return(found)
  }
  NULL
}

read_page_responsibly <- function(url, attempts = 3L, use_live = FALSE) {
  last_error <- "Request failed without an error message."
  for (attempt in seq_len(attempts)) {
    Sys.sleep(stats::runif(1, request_delay_seconds[1], request_delay_seconds[2]))
    result <- tryCatch(
      list(
        page = if (use_live) rvest::read_html_live(url) else rvest::read_html(url),
        error = NULL
      ),
      error = function(e) list(page = NULL, error = conditionMessage(e))
    )
    if (!is.null(result$page)) return(result)
    last_error <- if (is.null(result$error) || !nzchar(result$error)) {
      "IMDb returned an empty page."
    } else {
      result$error
    }
    if (attempt < attempts) Sys.sleep(attempt * 3)
  }
  list(page = NULL, error = last_error)
}

page_text <- function(page, selectors) {
  for (selector in selectors) {
    nodes <- tryCatch(rvest::html_elements(page, selector), error = function(e) NULL)
    if (is.null(nodes) || length(nodes) == 0) next
    values <- rvest::html_text2(nodes)
    values <- stringr::str_squish(values[!is.na(values) & nzchar(stringr::str_squish(values))])
    if (length(values) > 0) return(paste(unique(values), collapse = " | "))
  }
  NA_character_
}

jsonld_movie <- function(page) {
  nodes <- rvest::html_elements(page, 'script[type="application/ld+json"]')
  if (length(nodes) == 0) return(NULL)
  texts <- rvest::html_text(nodes)
  for (txt in texts) {
    parsed <- tryCatch(
      jsonlite::fromJSON(txt, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) next
    found <- find_movie_object(parsed)
    if (!is.null(found)) return(found)
  }
  NULL
}

extract_recommendations <- function(page, current_id) {
  # The first selector is from the supplied starter script. IMDb may change
  # its page markup, so the fallback remains limited to the More Like This area.
  selectors <- c(
    "div.ipc-sub-grid.ipc-sub-grid--page-span-2.ipc-sub-grid--nowrap.ipc-shoveler__grid a[href*='/title/tt']",
    "[data-testid*='MoreLikeThis'] a[href*='/title/tt'], [data-testid*='more-like-this'] a[href*='/title/tt']"
  )
  nodes <- NULL
  for (selector in selectors) {
    nodes <- tryCatch(rvest::html_elements(page, selector), error = function(e) NULL)
    if (!is.null(nodes) && length(nodes) > 0) break
  }
  if (is.null(nodes) || length(nodes) == 0) return(character())
  ids <- rvest::html_attr(nodes, "href") %>%
    stringr::str_extract("tt[0-9]+") %>%
    stats::na.omit() %>%
    unique()
  ids[ids != current_id]
}

extract_cast <- function(page, current_id, top_n = 4L) {
  sections <- rvest::html_elements(page, "section")
  cast_scope <- page
  if (length(sections) > 0) {
    section_labels <- vapply(sections, function(section) {
      heading <- rvest::html_element(section, "h3")
      rvest::html_text2(heading)
    }, character(1))
    cast_index <- which(stringr::str_detect(
      section_labels, stringr::regex("^\\s*Cast\\b", ignore_case = TRUE)
    ))
    if (length(cast_index) > 0) cast_scope <- sections[[cast_index[1]]]
  }
  actor_links <- rvest::html_elements(cast_scope, "a[href*='/name/nm']")
  actor_ids <- rvest::html_attr(actor_links, "href") %>% stringr::str_extract("nm[0-9]+")
  actor_names <- rvest::html_text2(actor_links) %>% stringr::str_squish()
  cast <- tibble::tibble(actor_id = actor_ids, actor_name = actor_names) %>%
    filter(!is.na(actor_id), !is.na(actor_name), nzchar(actor_name)) %>%
    distinct(actor_id, .keep_all = TRUE) %>%
    slice_head(n = top_n) %>%
    mutate(tconst = current_id, actor_order = row_number(), .before = 1)
  cast %>% select(tconst, actor_order, actor_id, actor_name)
}

extract_votes <- function(page, current_id) {
  # Current IMDb ratings pages expose the chart as SVG paths with accessible
  # labels such as "10 - 29.9% (477K Ratings)". Keep the older div selectors
  # as a fallback in case IMDb serves a different page layout.
  bars <- rvest::html_elements(
    page, "[data-testid='histogram-container'] path[aria-label]"
  )
  labels <- rvest::html_attr(bars, "aria-label")
  labels <- labels[!is.na(labels) & nzchar(labels)]

  if (length(labels) > 0) {
    count_match <- stringr::str_match(
      labels, "\\(([0-9][0-9,.]*\\s*[KMB]?)\\s+Ratings?\\)"
    )
    return(tibble::tibble(
      tconst = current_id,
      score = suppressWarnings(as.integer(stringr::str_extract(
        labels, "(?<![0-9])(?:10|[1-9])(?![0-9])"
      ))),
      percentage_raw = stringr::str_extract(labels, "[0-9]+(?:\\.[0-9]+)?%"),
      vote_count_raw = stringr::str_squish(count_match[, 2]),
      raw_row_text = labels,
      extraction_status = "row_found"
    ))
  }

  rows <- rvest::html_elements(page, "div.ipc-rating-histogram__row")
  if (length(rows) == 0) {
    rows <- rvest::html_elements(page, "[data-testid*='rating-histogram-row']")
  }
  if (length(rows) == 0) {
    return(tibble::tibble(
      tconst = current_id, score = NA_integer_, percentage_raw = NA_character_,
      vote_count_raw = NA_character_, raw_row_text = NA_character_,
      extraction_status = "histogram_rows_not_found"
    ))
  }
  raw <- rvest::html_text2(rows) %>% stringr::str_squish()
  tibble::tibble(
    tconst = current_id,
    score = suppressWarnings(as.integer(stringr::str_extract(raw, "(?<![0-9])(?:10|[1-9])(?![0-9])"))),
    percentage_raw = stringr::str_extract(raw, "[0-9]+(?:\\.[0-9]+)?%"),
    vote_count_raw = vapply(raw, function(row_text) {
      tokens <- stringr::str_extract_all(
        row_text, "[0-9][0-9,]*(?:\\.[0-9]+)?\\s*[KMB]?"
      )[[1]]
      if (length(tokens) >= 2) stringr::str_squish(tokens[length(tokens)]) else NA_character_
    }, character(1)),
    raw_row_text = raw,
    extraction_status = "row_found"
  )
}

is_imdb_bot_challenge <- function(page) {
  if (is.null(page)) return(FALSE)
  body_nodes <- tryCatch(
    rvest::html_elements(page, "body"),
    error = function(e) NULL
  )
  if (is.null(body_nodes) || length(body_nodes) == 0) return(FALSE)
  body_text <- tryCatch(
    paste(rvest::html_text(body_nodes), collapse = " "),
    error = function(e) ""
  )
  stringr::str_detect(
    body_text,
    stringr::regex(
      "confirm you are human|security check before continuing|not a bot|captcha puzzle|awswafintegration|challengescript",
      ignore_case = TRUE
    )
  )
}

scrape_movie <- function(current_id) {
  base_url <- paste0("https://www.imdb.com/title/", current_id, "/")
  # Try the faster static HTML first. IMDb's recommendation carousel can be
  # JavaScript-rendered, so use the live browser only when static HTML lacks
  # the movie JSON-LD or the recommendation links.
  main_result <- read_page_responsibly(base_url, use_live = FALSE)
  main_page <- main_result$page
  movie <- if (is.null(main_page)) NULL else jsonld_movie(main_page)
  recommendations <- if (is.null(main_page)) {
    character()
  } else {
    extract_recommendations(main_page, current_id)
  }
  if (is.null(movie) || length(recommendations) == 0) {
    live_result <- read_page_responsibly(base_url, use_live = TRUE)
    if (!is.null(live_result$page)) {
      main_page <- live_result$page
      if (is.null(movie)) movie <- jsonld_movie(main_page)
      live_recommendations <- extract_recommendations(main_page, current_id)
      if (length(live_recommendations) > 0) recommendations <- live_recommendations
    }
  }
  if (is_imdb_bot_challenge(main_page)) {
    return(list(
      ok = FALSE,
      stage = "anti_bot_challenge",
      error = "IMDb served a human-verification CAPTCHA page. The title remains pending."
    ))
  }
  if (is.null(main_page)) {
    return(list(ok = FALSE, stage = "title_page", error = main_result$error))
  }
  if (is.null(movie)) return(list(ok = TRUE, is_movie = FALSE))

  aggregate_rating <- movie$aggregateRating
  movie_row <- tibble::tibble(
    tconst = current_id,
    movie_name = clean_scalar(movie$name),
    date_published_raw = clean_scalar(movie$datePublished),
    genres_raw = clean_scalar(movie$genre),
    imdb_rating_raw = clean_scalar(aggregate_rating$ratingValue),
    imdb_votes_raw = clean_scalar(aggregate_rating$ratingCount),
    description_raw = clean_scalar(movie$description),
    content_rating_raw = clean_scalar(movie$contentRating),
    production_companies_raw = {
      company_links <- rvest::html_elements(
        main_page, "[data-testid*='title-details-companies'] a[href*='/company/']"
      )
      company_names <- rvest::html_text2(company_links) %>% stringr::str_squish()
      company_names <- unique(company_names[!is.na(company_names) & nzchar(company_names)])
      if (length(company_names) == 0) NA_character_ else paste(company_names, collapse = " | ")
    },
    budget_raw = page_text(main_page, c(
      "[data-testid='title-boxoffice-budget']", "[data-testid*='title-boxoffice-budget']"
    )),
    gross_us_canada_raw = page_text(main_page, c(
      "[data-testid='title-boxoffice-grossdomestic']", "[data-testid*='grossdomestic']"
    )),
    gross_worldwide_raw = page_text(main_page, c(
      "[data-testid='title-boxoffice-cumulativeworldwidegross']",
      "[data-testid*='cumulativeworldwidegross']"
    ))
  )
  credits_url <- paste0(base_url, "fullcredits")
  credits_result <- read_page_responsibly(credits_url, use_live = FALSE)
  if (is_imdb_bot_challenge(credits_result$page)) {
    return(list(
      ok = FALSE,
      stage = "anti_bot_challenge",
      error = "IMDb served a human-verification CAPTCHA page on full credits. The title remains pending."
    ))
  }
  cast <- if (is.null(credits_result$page)) {
    empty_cast
  } else {
    extract_cast(credits_result$page, current_id, top_n = 4L)
  }
  credits_page_available <- !is.null(credits_result$page)
  if (nrow(cast) == 0) {
    live_credits_result <- read_page_responsibly(credits_url, use_live = TRUE)
    if (is_imdb_bot_challenge(live_credits_result$page)) {
      return(list(
        ok = FALSE,
        stage = "anti_bot_challenge",
        error = "IMDb served a human-verification CAPTCHA page on full credits. The title remains pending."
      ))
    }
    if (!is.null(live_credits_result$page)) {
      credits_page_available <- TRUE
      live_cast <- extract_cast(live_credits_result$page, current_id, top_n = 4L)
      if (nrow(live_cast) > 0) cast <- live_cast
    }
  }
  if (!credits_page_available) {
    cast <- empty_cast
    credits_failure <- tibble::tibble(
      member_id = NA_character_, tconst = current_id, stage = "fullcredits_page",
      url = credits_url,
      error_text = paste(
        na.omit(c(credits_result$error, live_credits_result$error)),
        collapse = " | "
      )
    )
  } else {
    credits_failure <- empty_failures
  }

  ratings_url <- paste0(base_url, "ratings")
  ratings_result <- read_page_responsibly(ratings_url, use_live = TRUE)
  if (is_imdb_bot_challenge(ratings_result$page)) {
    return(list(
      ok = FALSE,
      stage = "anti_bot_challenge",
      error = "IMDb served a human-verification CAPTCHA page on ratings. The title remains pending."
    ))
  }
  if (is.null(ratings_result$page)) {
    votes <- tibble::tibble(
      tconst = current_id, score = NA_integer_, percentage_raw = NA_character_,
      vote_count_raw = NA_character_, raw_row_text = NA_character_,
      extraction_status = "ratings_page_request_failed"
    )
    ratings_failure <- tibble::tibble(
      member_id = NA_character_, tconst = current_id, stage = "ratings_page",
      url = ratings_url, error_text = ratings_result$error
    )
  } else {
    votes <- extract_votes(ratings_result$page, current_id)
    ratings_failure <- empty_failures
  }

  list(
    ok = TRUE, is_movie = TRUE, movie = movie_row, cast = cast, votes = votes,
    recommendations = if (length(recommendations) == 0) {
      empty_recommendations
    } else {
      tibble::tibble(parent_tconst = current_id, recommended_tconst = recommendations)
    },
    failures = bind_rows(credits_failure, ratings_failure)
  )
}

append_unique <- function(old_data, new_data, key_columns) {
  if (nrow(new_data) == 0) return(old_data)
  bind_rows(old_data, new_data) %>%
    distinct(across(all_of(key_columns)), .keep_all = TRUE)
}

write_raw_tables <- function(state) {
  readr::write_csv(state$movies, file.path(raw_dir, "movies_raw.csv"), na = "")
  readr::write_csv(state$cast, file.path(raw_dir, "cast_raw.csv"), na = "")
  readr::write_csv(state$votes, file.path(raw_dir, "vote_distribution_raw.csv"), na = "")
  readr::write_csv(state$recommendations, file.path(raw_dir, "recommendations_raw.csv"), na = "")
  readr::write_csv(state$contributors, file.path(raw_dir, "movie_contributors_raw.csv"), na = "")
  readr::write_csv(state$lineage, file.path(raw_dir, "movie_lineage_raw.csv"), na = "")
  readr::write_csv(state$discovery_edges, file.path(raw_dir, "discovery_edges_raw.csv"), na = "")
  readr::write_csv(state$failures, file.path(raw_dir, "scrape_failures_raw.csv"), na = "")
  readr::write_csv(state$queue, file.path(raw_dir, "crawl_queue_raw.csv"), na = "")
  saveRDS(state, checkpoint_path)
}

# -------------------------------- Crawl loop ---------------------------------
# The target is global: movies_raw.csv contains each IMDb ID once across the
# whole team. Rotate through member queues so one seed cannot dominate the run.
team_members <- seed_registry$member_id
member_cursor <- 1L
message(
  "Resuming with ", dplyr::n_distinct(state$movies$tconst),
  " globally unique movies; target is ", target_unique_movies, "."
)

repeat {
  unique_movie_count <- dplyr::n_distinct(state$movies$tconst)
  if (unique_movie_count >= target_unique_movies) {
    message("Reached the global target of ", target_unique_movies,
            " unique IMDb movies.")
    break
  }

  # Select the next member with pending recommendations, starting at the
  # rotating cursor. This keeps all seed paths contributing to the global goal.
  member_position <- NA_integer_
  for (offset in seq_along(team_members)) {
    candidate_position <- ((member_cursor + offset - 2L) %% length(team_members)) + 1L
    candidate_member <- team_members[[candidate_position]]
    if (any(state$queue$member_id == candidate_member & state$queue$status == "pending")) {
      member_position <- candidate_position
      break
    }
  }

  if (is.na(member_position)) {
    message("No pending titles remain. Collected ", unique_movie_count,
            " globally unique IMDb movies (target ", target_unique_movies, ").")
    break
  }

  member_id <- team_members[[member_position]]
  member_cursor <- (member_position %% length(team_members)) + 1L
  seed_id <- seed_registry$seed_movie_id[match(member_id, seed_registry$member_id)]
  queue_index <- which(
    state$queue$member_id == member_id & state$queue$status == "pending"
  )[1]
  queue_item <- state$queue[queue_index, ]
  current_id <- queue_item$tconst

  if (current_id %in% state$movies$tconst) {
    # Reuse cached metadata, but keep this member's discovery lineage.
    result <- list(ok = TRUE, is_movie = TRUE, cached = TRUE)
  } else {
    result <- scrape_movie(current_id)
  }

  if (!isTRUE(result$ok)) {
    if (identical(result$stage, "anti_bot_challenge")) {
      state$processed_since_checkpoint <- 0L
      write_raw_tables(state)
      stop(
        "IMDb_CAPTCHA_BLOCKED: ", result$error,
        " Stopped after saving the checkpoint; retry only after the challenge clears."
      )
    }
    state$queue$status[queue_index] <- "failed"
    state$failures <- bind_rows(state$failures, tibble::tibble(
      member_id = member_id, tconst = current_id, stage = result$stage,
      url = paste0("https://www.imdb.com/title/", current_id, "/"),
      error_text = result$error
    ))
    state$processed_since_checkpoint <- state$processed_since_checkpoint + 1L
    message(member_id, ": request failed for ", current_id, "; saved failure and continuing.")
  } else if (!isTRUE(result$is_movie)) {
    state$queue$status[queue_index] <- "not_movie_or_jsonld_missing"
    state$failures <- bind_rows(state$failures, tibble::tibble(
      member_id = member_id, tconst = current_id, stage = "movie_type_check",
      url = paste0("https://www.imdb.com/title/", current_id, "/"),
      error_text = "No JSON-LD object classified as Movie was found."
    ))
    state$processed_since_checkpoint <- state$processed_since_checkpoint + 1L
    message(member_id, ": skipped non-movie/unclassified title ", current_id, ".")
  } else {
    if (!isTRUE(result$cached)) {
      state$movies <- append_unique(state$movies, result$movie, "tconst")
      state$cast <- append_unique(state$cast, result$cast, c("tconst", "actor_id"))
      state$votes <- bind_rows(state$votes, result$votes)
      state$recommendations <- append_unique(
        state$recommendations, result$recommendations,
        c("parent_tconst", "recommended_tconst")
      )
      if (nrow(result$failures) > 0) {
        result$failures$member_id <- member_id
        state$failures <- bind_rows(state$failures, result$failures)
      }
    }

    state$contributors <- append_unique(
      state$contributors,
      tibble::tibble(
        member_id = member_id, seed_movie_id = seed_id, tconst = current_id,
        first_discovery_round = queue_item$discovery_round
      ),
      c("member_id", "tconst")
    )
    state$lineage <- append_unique(
      state$lineage,
      tibble::tibble(
        member_id = member_id, seed_movie_id = seed_id,
        parent_tconst = queue_item$parent_tconst, tconst = current_id,
        discovery_round = queue_item$discovery_round,
        is_seed = current_id == seed_id
      ),
      c("member_id", "seed_movie_id", "parent_tconst", "tconst", "discovery_round")
    )

    children <- state$recommendations %>%
      filter(parent_tconst == current_id) %>%
      pull(recommended_tconst) %>%
      unique()
    if (length(children) > 0) {
      new_edges <- tibble::tibble(
        member_id = member_id,
        seed_movie_id = seed_id,
        parent_tconst = current_id,
        recommended_tconst = children,
        discovery_round = queue_item$discovery_round + 1L
      )
      state$discovery_edges <- append_unique(
        state$discovery_edges, new_edges,
        c("member_id", "seed_movie_id", "parent_tconst", "recommended_tconst")
      )
      not_queued <- children[!children %in% state$queue$tconst[state$queue$member_id == member_id]]
      if (length(not_queued) > 0) {
        state$queue <- bind_rows(state$queue, tibble::tibble(
          member_id = member_id,
          seed_movie_id = seed_id,
          tconst = not_queued,
          parent_tconst = current_id,
          discovery_round = queue_item$discovery_round + 1L,
          status = "pending"
        ))
      }
    }
    state$queue$status[queue_index] <- "processed"
    state$processed_since_checkpoint <- state$processed_since_checkpoint + 1L
    cached_label <- if (isTRUE(result$cached)) " (already scraped; lineage saved)" else ""
    message(
      "Unique movies: ", dplyr::n_distinct(state$movies$tconst), "/",
      target_unique_movies, "; ", member_id, " processed ", current_id,
      cached_label
    )
  }

  if (state$processed_since_checkpoint >= checkpoint_every) {
    state$processed_since_checkpoint <- 0L
    write_raw_tables(state)
    message("Checkpoint saved in data_raw/.")
  }
}

write_raw_tables(state)
save(state, file = file.path(raw_dir, "imdb_raw_workspace.RData"))
message("Task 0 finished. Raw CSVs and the resumable checkpoint are in data_raw/.")
message("Review scrape_failures_raw.csv and confirm coverage before running Task 1.")
