# Configuration for the Actuarial Functions repository harness.

STRICT_MODE <- FALSE

EXCLUDED_DIRS <- c(
  ".git",
  "arneses/actuarial_repo_harness/_reports"
)

RISK_PATTERNS <- list(
  absolute_windows_path = "[A-Za-z]:[\\\\/]",
  setwd = "\\bsetwd\\s*\\(",
  attach = "\\battach\\s*\\(",
  interactive_view = "\\bView\\s*\\(",
  windows_graphics = "\\bwin\\.graph\\s*\\(",
  browser_debugger = "\\bbrowser\\s*\\(",
  source_external = "\\bsource\\s*\\(",
  file_input = "\\b(read\\.csv|read\\.table|scan|load|readRDS)\\s*\\("
)
