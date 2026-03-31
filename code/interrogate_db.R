#!/usr/bin/env Rscript

# Examples:
# Rscript code/interrogate_db.R summary
# Rscript code/interrogate_db.R preview measure_info 5
# Rscript code/interrogate_db.R sql "SELECT name, radiometer_type FROM radiometer ORDER BY id LIMIT 5"

suppressPackageStartupMessages({
  library(tidyverse)
})

args <- commandArgs(trailingOnly = TRUE)

find_default_db <- function() {
  db_files <- list.files("data", pattern = "\\.db$", full.names = TRUE)

  if (length(db_files) == 0) {
    stop("No .db file found under data/.", call. = FALSE)
  }

  if (length(db_files) > 1) {
    message("Multiple .db files found. Using the first one: ", db_files[[1]])
  }

  db_files[[1]]
}

parse_args <- function(args) {
  command <- if (length(args) >= 1) args[[1]] else "summary"

  if (command %in% c("-h", "--help", "help")) {
    return(list(command = "help"))
  }

  if (command == "preview") {
    table_name <- if (length(args) >= 2) args[[2]] else "measure_info"
    n_rows <- if (length(args) >= 3) as.integer(args[[3]]) else 10L

    return(list(command = command, table_name = table_name, n_rows = n_rows))
  }

  if (command == "sql") {
    sql_text <- if (length(args) >= 2) paste(args[-1], collapse = " ") else NULL

    return(list(command = command, sql_text = sql_text))
  }

  list(command = command)
}

print_help <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript code/interrogate_db.R summary",
      "  Rscript code/interrogate_db.R preview <table> [n_rows]",
      "  Rscript code/interrogate_db.R sql \"<SQL query>\"",
      "",
      "Commands:",
      "  summary   Print table counts and measurement summaries.",
      "  preview   Show the first rows of one table (default: measure_info).",
      "  sql       Run a custom SQL query and print the result.",
      sep = "\n"
    )
  )
}

quote_ident <- function(x) {
  paste0("\"", gsub("\"", "\"\"", x), "\"")
}

run_sqlite_query <- function(db_path, sql_text) {
  query_output <- system2(
    command = "sqlite3",
    args = c("-header", "-csv", db_path),
    input = paste0(sql_text, ";\n"),
    stdout = TRUE,
    stderr = TRUE
  )

  status <- attr(query_output, "status")

  if (!is.null(status) && status != 0) {
    stop(paste(query_output, collapse = "\n"), call. = FALSE)
  }

  if (length(query_output) == 0) {
    return(tibble())
  }

  read_csv(
    I(paste(query_output, collapse = "\n")),
    show_col_types = FALSE,
    progress = FALSE
  )
}

list_tables <- function(db_path) {
  run_sqlite_query(
    db_path,
    "SELECT name AS table_name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%' ORDER BY name"
  ) |>
    pull(table_name)
}

table_counts <- function(db_path) {
  tibble(table_name = list_tables(db_path)) |>
    mutate(n_rows = map_int(table_name, \(table_name) {
      run_sqlite_query(
        db_path,
        paste0("SELECT COUNT(*) AS n_rows FROM ", quote_ident(table_name))
      ) |>
        pull(n_rows)
    })) |>
    arrange(desc(n_rows), table_name)
}

radiometer_summary <- function(db_path) {
  run_sqlite_query(
    db_path,
    paste(
      "SELECT name, full_name, radiometer_type, measure_type, spectral_resolution, rsr_count",
      "FROM radiometer",
      "ORDER BY radiometer_type, name"
    )
  )
}

measurement_summary <- function(db_path) {
  run_sqlite_query(
    db_path,
    paste(
      "SELECT",
      "  r.name,",
      "  mi.level,",
      "  mi.type,",
      "  mi.qc,",
      "  COUNT(*) AS n_measurements,",
      "  MIN(mi.day) AS first_day,",
      "  MAX(mi.day) AS last_day",
      "FROM measure_info AS mi",
      "INNER JOIN radiometer AS r",
      "  ON mi.radiometer_id = r.id",
      "GROUP BY r.name, mi.level, mi.type, mi.qc",
      "ORDER BY r.name, mi.type, mi.level, mi.qc"
    )
  )
}

preview_table <- function(db_path, table_name, n_rows = 10L) {
  available_tables <- list_tables(db_path)

  if (!(table_name %in% available_tables)) {
    stop(
      paste0(
        "Table '", table_name, "' not found. Available tables: ",
        paste(sort(available_tables), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  run_sqlite_query(
    db_path,
    paste0("SELECT * FROM ", quote_ident(table_name), " LIMIT ", as.integer(n_rows))
  )
}

run_sql <- function(db_path, sql_text) {
  if (is.null(sql_text) || !nzchar(sql_text)) {
    stop("Provide a SQL query after the 'sql' command.", call. = FALSE)
  }

  run_sqlite_query(db_path, sql_text)
}

main <- function(args) {
  parsed_args <- parse_args(args)

  if (parsed_args$command == "help") {
    print_help()
    return(invisible(NULL))
  }

  db_path <- find_default_db()
  message("Using database: ", db_path)

  if (parsed_args$command == "summary") {
    cat("\nTable counts\n")
    print(table_counts(db_path), n = Inf)

    cat("\nRadiometers\n")
    print(radiometer_summary(db_path), n = Inf)

    cat("\nMeasurements by radiometer / level / type / qc\n")
    print(measurement_summary(db_path), n = Inf)
    return(invisible(NULL))
  }

  if (parsed_args$command == "preview") {
    print(preview_table(db_path, parsed_args$table_name, parsed_args$n_rows), n = parsed_args$n_rows)
    return(invisible(NULL))
  }

  if (parsed_args$command == "sql") {
    print(run_sql(db_path, parsed_args$sql_text), n = Inf)
    return(invisible(NULL))
  }

  stop(
    paste0(
      "Unknown command '", parsed_args$command, "'. Run with --help for usage."
    ),
    call. = FALSE
  )
}

main(args)
