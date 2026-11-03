args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop("Supply the crawl_checkpoint.rds path.")
saved_state <- readRDS(args[[1]])
if (is.null(saved_state$contributors) || nrow(saved_state$contributors) == 0L) {
  cat("0\n")
} else {
  pairs <- unique(saved_state$contributors[c("member_id", "tconst")])
  cat(nrow(pairs), "\n")
}
