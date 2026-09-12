#!/usr/bin/env Rscript

# Generic CSV data-quality harness.
# Usage:
#   Rscript run_data_quality.R path/to/data.csv [output_dir]

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat("Usage: Rscript run_data_quality.R <csv_path> [output_dir]\n")
  quit(status = 2)
}

csv_path <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
output_dir <- if (length(args) >= 2) args[[2]] else file.path(dirname(csv_path), "_data_quality_report")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

max_missing_pct <- as.numeric(Sys.getenv("MAX_MISSING_PCT", "20"))
max_duplicate_pct <- as.numeric(Sys.getenv("MAX_DUPLICATE_PCT", "5"))
strict_mode <- tolower(Sys.getenv("STRICT_MODE", "false")) %in% c("1", "true", "yes", "y")

read_result <- tryCatch(
  read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE),
  error = function(e) e
)

if (inherits(read_result, "error")) {
  cat("Could not read CSV:", conditionMessage(read_result), "\n")
  quit(status = 2)
}

df <- read_result
n_rows <- nrow(df)
n_cols <- ncol(df)

duplicate_rows <- if (n_rows == 0) 0 else sum(duplicated(df))
duplicate_pct <- if (n_rows == 0) 0 else 100 * duplicate_rows / n_rows

column_profile <- data.frame(
  column = names(df),
  class = vapply(df, function(x) paste(class(x), collapse = "/"), character(1)),
  missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
  missing_pct = vapply(df, function(x) if (n_rows == 0) 0 else 100 * mean(is.na(x)), numeric(1)),
  unique_values = vapply(df, function(x) length(unique(x[!is.na(x)])), integer(1)),
  stringsAsFactors = FALSE
)

numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]

numeric_profile <- if (length(numeric_cols) == 0) {
  data.frame()
} else {
  do.call(rbind, lapply(numeric_cols, function(col) {
    x <- df[[col]]
    valid <- x[is.finite(x)]

    if (length(valid) == 0) {
      return(data.frame(
        column = col, min = NA_real_, p25 = NA_real_, median = NA_real_,
        mean = NA_real_, p75 = NA_real_, max = NA_real_, sd = NA_real_
      ))
    }

    qs <- quantile(valid, probs = c(0, .25, .5, .75, 1), na.rm = TRUE, names = FALSE)

    data.frame(
      column = col,
      min = qs[[1]],
      p25 = qs[[2]],
      median = qs[[3]],
      mean = mean(valid),
      p75 = qs[[4]],
      max = qs[[5]],
      sd = if (length(valid) > 1) sd(valid) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}

column_csv <- file.path(output_dir, "column_profile.csv")
numeric_csv <- file.path(output_dir, "numeric_profile.csv")
summary_md <- file.path(output_dir, "data_quality_report.md")

write.csv(column_profile, column_csv, row.names = FALSE, na = "")
write.csv(numeric_profile, numeric_csv, row.names = FALSE, na = "")

bad_missing <- column_profile[column_profile$missing_pct > max_missing_pct, , drop = FALSE]
duplicate_fail <- duplicate_pct > max_duplicate_pct

status <- if (nrow(bad_missing) > 0 || duplicate_fail) "CHECK" else "PASS"

md <- c(
  "# Data quality harness report",
  "",
  paste0("- File: `", basename(csv_path), "`"),
  paste0("- Rows: **", n_rows, "**"),
  paste0("- Columns: **", n_cols, "**"),
  paste0("- Duplicate rows: **", duplicate_rows, "** (", sprintf("%.2f", duplicate_pct), "%)"),
  paste0("- Missing-value threshold: **", max_missing_pct, "%**"),
  paste0("- Duplicate threshold: **", max_duplicate_pct, "%**"),
  paste0("- Overall status: **", status, "**"),
  "",
  "## Columns above missing-value threshold",
  ""
)

if (nrow(bad_missing) == 0) {
  md <- c(md, "_None._")
} else {
  for (i in seq_len(nrow(bad_missing))) {
    md <- c(
      md,
      paste0(
        "- `", bad_missing$column[[i]], "`: ",
        sprintf("%.2f", bad_missing$missing_pct[[i]]), "% missing"
      )
    )
  }
}

md <- c(
  md,
  "",
  "## Output files",
  "",
  "- `column_profile.csv`",
  "- `numeric_profile.csv`",
  "- `data_quality_report.md`"
)

writeLines(md, summary_md, useBytes = TRUE)

cat("Data quality harness:", status, "\n")
cat("Report:", summary_md, "\n")

if (strict_mode && status != "PASS") {
  quit(status = 1)
}

quit(status = 0)
