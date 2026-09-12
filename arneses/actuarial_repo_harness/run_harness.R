#!/usr/bin/env Rscript

# Static repository harness for jcval94/Actuarial-Functions.
# It intentionally DOES NOT source/execute the repository scripts.

get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (length(hit) == 0) return(normalizePath(".", winslash = "/", mustWork = TRUE))
  normalizePath(sub("^--file=", "", hit[[1]]), winslash = "/", mustWork = TRUE)
}

script_path <- get_script_path()
script_dir <- if (dir.exists(script_path)) script_path else dirname(script_path)
repo_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(script_dir, "config.R"))

report_dir <- file.path(script_dir, "_reports")
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

all_r_files <- list.files(
  repo_root,
  pattern = "\\.R$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

normalize_rel <- function(x) {
  rel <- substring(normalizePath(x, winslash = "/", mustWork = TRUE), nchar(repo_root) + 2)
  gsub("\\\\", "/", rel)
}

is_excluded <- function(path) {
  rel <- normalize_rel(path)
  any(vapply(EXCLUDED_DIRS, function(d) startsWith(rel, d), logical(1)))
}

r_files <- all_r_files[!vapply(all_r_files, is_excluded, logical(1))]

safe_read_lines <- function(path) {
  tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character(0)
  )
}

syntax_check <- function(path) {
  tryCatch(
    {
      parse(file = path, keep.source = FALSE)
      list(ok = TRUE, error = "")
    },
    error = function(e) list(ok = FALSE, error = conditionMessage(e))
  )
}

extract_packages <- function(text) {
  rx <- "(?:library|require)\\s*\\(\\s*[\"']?([A-Za-z0-9._]+)"
  hits <- regmatches(text, gregexpr(rx, text, perl = TRUE))[[1]]
  if (identical(hits, character(0)) || identical(hits, "")) return(character(0))
  unique(sub(rx, "\\1", hits, perl = TRUE))
}

count_pattern <- function(lines, pattern) {
  sum(grepl(pattern, lines, perl = TRUE, ignore.case = TRUE))
}

rows <- list()
package_map <- list()

for (path in r_files) {
  rel <- normalize_rel(path)
  lines <- safe_read_lines(path)
  syn <- syntax_check(path)

  risk_counts <- vapply(
    RISK_PATTERNS,
    function(pattern) count_pattern(lines, pattern),
    integer(1)
  )

  packages <- extract_packages(paste(lines, collapse = "\n"))
  package_map[[rel]] <- packages

  rows[[length(rows) + 1]] <- data.frame(
    file = rel,
    lines = length(lines),
    syntax_ok = syn$ok,
    syntax_error = syn$error,
    absolute_windows_path = unname(risk_counts[["absolute_windows_path"]]),
    setwd = unname(risk_counts[["setwd"]]),
    attach = unname(risk_counts[["attach"]]),
    interactive_view = unname(risk_counts[["interactive_view"]]),
    windows_graphics = unname(risk_counts[["windows_graphics"]]),
    browser_debugger = unname(risk_counts[["browser_debugger"]]),
    source_external = unname(risk_counts[["source_external"]]),
    file_input = unname(risk_counts[["file_input"]]),
    packages = paste(packages, collapse = ", "),
    stringsAsFactors = FALSE
  )
}

results <- if (length(rows) == 0) {
  data.frame()
} else {
  do.call(rbind, rows)
}

csv_path <- file.path(report_dir, "actuarial_harness_report.csv")
write.csv(results, csv_path, row.names = FALSE, na = "")

n_files <- nrow(results)
n_syntax_errors <- if (n_files == 0) 0 else sum(!results$syntax_ok)
n_abs_paths <- if (n_files == 0) 0 else sum(results$absolute_windows_path)
n_interactive <- if (n_files == 0) 0 else sum(
  results$interactive_view + results$windows_graphics + results$browser_debugger
)

all_packages <- sort(unique(unlist(package_map, use.names = FALSE)))
all_packages <- all_packages[nzchar(all_packages)]

md <- c(
  "# Actuarial repository harness report",
  "",
  paste0("- Repository: `", basename(repo_root), "`"),
  paste0("- R files scanned: **", n_files, "**"),
  paste0("- Files with syntax errors: **", n_syntax_errors, "**"),
  paste0("- Absolute Windows path occurrences: **", n_abs_paths, "**"),
  paste0("- Interactive-only calls detected: **", n_interactive, "**"),
  paste0("- Packages referenced: ", if (length(all_packages)) paste0("`", paste(all_packages, collapse = "`, `"), "`") else "_none_"),
  "",
  "## Files needing attention",
  ""
)

if (n_files == 0) {
  md <- c(md, "_No R files found._")
} else {
  attention <- results[
    !results$syntax_ok |
      results$absolute_windows_path > 0 |
      results$setwd > 0 |
      results$attach > 0 |
      results$interactive_view > 0 |
      results$windows_graphics > 0 |
      results$browser_debugger > 0,
    ,
    drop = FALSE
  ]

  if (nrow(attention) == 0) {
    md <- c(md, "_No flagged files._")
  } else {
    for (i in seq_len(nrow(attention))) {
      r <- attention[i, ]
      flags <- character(0)
      if (!r$syntax_ok) flags <- c(flags, paste0("syntax: ", r$syntax_error))
      if (r$absolute_windows_path > 0) flags <- c(flags, paste0("absolute paths=", r$absolute_windows_path))
      if (r$setwd > 0) flags <- c(flags, paste0("setwd=", r$setwd))
      if (r$attach > 0) flags <- c(flags, paste0("attach=", r$attach))
      if (r$interactive_view > 0) flags <- c(flags, paste0("View=", r$interactive_view))
      if (r$windows_graphics > 0) flags <- c(flags, paste0("win.graph=", r$windows_graphics))
      if (r$browser_debugger > 0) flags <- c(flags, paste0("browser=", r$browser_debugger))
      md <- c(md, paste0("- `", r$file, "`: ", paste(flags, collapse = "; ")))
    }
  }
}

md <- c(
  md,
  "",
  "## Interpretation",
  "",
  "This harness is deliberately static: it inspects old scripts without executing arbitrary file I/O, plotting windows, or environment-specific code.",
  "Use the CSV for detailed filtering and the Markdown report for a quick review."
)

md_path <- file.path(report_dir, "actuarial_harness_report.md")
writeLines(md, md_path, useBytes = TRUE)

cat("Harness completed.\n")
cat("Markdown:", md_path, "\n")
cat("CSV:", csv_path, "\n")

if (STRICT_MODE && n_syntax_errors > 0) {
  quit(status = 1)
}

quit(status = 0)
