# =============================================================================
# OPTIONAL LAUNCHER: resume Task 0 after a fatal R/Chromote error
#
# Run this from the project folder, not from the outputs folder. It sources
# 0_Get data.R, which resumes from data_raw/crawl_checkpoint.rds. If an error
# escapes the scraper, this launcher closes and recreates Chromote, waits, and
# tries again. It stops after three errors with no new checkpoint progress.
# The PC must remain awake and connected to the internet.
# =============================================================================

project_dir <- getwd()
crawler_file <- file.path(project_dir, "0_Get data.R")
checkpoint_file <- file.path(project_dir, "data_raw", "crawl_checkpoint.rds")
restart_limit <- 100L
wait_after_error <- 60L

if (!file.exists(crawler_file)) {
  stop("Put Run_Task0_with_retries.R next to 0_Get data.R and run it from that project folder.")
}

checkpoint_movie_count <- function(path) {
  if (!file.exists(path)) return(0L)
  saved_state <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(saved_state) || is.null(saved_state$contributors) ||
      nrow(saved_state$contributors) == 0) return(0L)
  unique_pairs <- unique(saved_state$contributors[c("member_id", "tconst")])
  nrow(unique_pairs)
}

last_progress <- checkpoint_movie_count(checkpoint_file)
no_progress_errors <- 0L

for (attempt in seq_len(restart_limit)) {
  message("Starting Task 0 attempt ", attempt, " of ", restart_limit, ".")

  result <- tryCatch(
    {
      source(crawler_file, local = .GlobalEnv)
      list(ok = TRUE, error = NULL)
    },
    error = function(e) list(ok = FALSE, error = conditionMessage(e))
  )

  if (isTRUE(result$ok)) {
    message("Task 0 returned normally. Review crawl_queue_raw.csv and member coverage.")
    break
  }

  message("Task 0 stopped with an error: ", result$error)
  current_progress <- checkpoint_movie_count(checkpoint_file)
  if (current_progress > last_progress) {
    no_progress_errors <- 0L
  } else {
    no_progress_errors <- no_progress_errors + 1L
  }
  last_progress <- current_progress

  if (no_progress_errors >= 3L) {
    stop("Three retries made no checkpoint progress. Review the error and checkpoint before trying again.")
  }

  if (requireNamespace("chromote", quietly = TRUE)) {
    tryCatch({
      if (chromote::has_default_chromote_object()) {
        chromote::default_chromote_object()$close(wait = 5)
      }
    }, error = function(e) message("Chromote close note: ", conditionMessage(e)))

    tryCatch({
      fresh_chromote <- chromote::Chromote$new()
      fresh_chromote$default_timeout <- 60
      chromote::set_default_chromote_object(fresh_chromote)
    }, error = function(e) message("Chromote reconnect note: ", conditionMessage(e)))
  }

  invisible(gc())
  message(
    "Checkpoint currently contains ", current_progress,
    " member/movie contributions. Waiting ", wait_after_error,
    " seconds before resuming."
  )
  Sys.sleep(wait_after_error)
}
