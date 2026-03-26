# Required packages
library(DBI)
library(RPostgres)
library(lubridate)

# Helper to get env with fallback prompt for local runs
get_env <- function(name, default = NULL, prompt_if_missing = TRUE) {
  val <- Sys.getenv(name, unset = "")
  if (nzchar(val)) return(val)
  if (!is.null(default)) return(default)
  if (prompt_if_missing) {
    message(sprintf("Environment var %s not set. Please enter value (input will be visible):", name))
    return(readline(prompt = paste0(name, ": ")))
  }
  return(NA)
}

# Connection params from environment or prompt (local-friendly)
Sys.setenv(SUPABASE_DB_HOST = "aws-1-eu-west-1.pooler.supabase.com")
db_host <- get_env("SUPABASE_DB_HOST", "db.bnnisnnqvsghpyktijal.supabase.co")
db_port <- as.integer(get_env("SUPABASE_DB_PORT", "5432"))
db_name <- get_env("SUPABASE_DB_NAME", "postgres")
Sys.setenv(SUPABASE_DB_USER = "postgres.bnnisnnqvsghpyktijal")
db_user <- get_env("SUPABASE_DB_USER")
pw<-read.table(file='~/.secrets/supabase', header = FALSE) # Read password locally
Sys.setenv(SUPABASE_DB_PASSWORD = as.character(pw[1]))
db_password <- get_env("SUPABASE_DB_PASSWORD")
sslmode  <- get_env("SUPABASE_SSLMODE", "require")

if (!nzchar(db_user) || !nzchar(db_password)) {
  stop("Database credentials are required. Set SUPABASE_DB_USER and SUPABASE_DB_PASSWORD environment variables or re-run and enter them when prompted.")
}

# Connect
con <- NULL
tryCatch({
  con <- dbConnect(
    RPostgres::Postgres(),
    host = db_host,
    port = db_port,
    dbname = db_name,
    user = db_user,
    password = db_password,
    sslmode = sslmode,
    sslrootcert= NULL
  )
}, error = function(e) {
  stop("Failed to connect to database: ", e$message)
})

# Column order expected by public.mastersheet
cols <- c("date","time","date_time","location","indoor","game_no",
          "no_of_courts","court_rank","score_method","p1","p2","p3","p4",
          "p1p2_score","p3p4_score","event_type")

# Normalize a single row: parse dates/times and coerce types to match table
normalize_row <- function(row) {
  r <- as.list(row)
  
  # ----- date normalization -----
  # Accept Date objects, POSIXct, or various string formats
  if (!is.null(r$date) && !inherits(r$date, "Date")) {
    # try lubridate parsers with common orders
    parsed_date <- NA
    if (is.character(r$date)) {
      # Try common formats: YYYY-MM-DD, DD/MM/YYYY, MM-DD-YYYY, DMY HMS, etc.
      orders <- c("Ymd", "dmy", "mdy", "Ymd HMS", "Ymd HM", "dmY HMS", "dmy HM")
      for (ord in orders) {
        parsed_date_try <- tryCatch(parse_date_time(r$date, orders = ord, truncated = 3), error = function(e) NA)
        if (!is.na(parsed_date_try)) {
          parsed_date <- as_date(parsed_date_try)
          break
        }
      }
      # As a last resort try as.Date
      if (is.na(parsed_date)) {
        try_date <- suppressWarnings(as.Date(r$date))
        if (!is.na(try_date)) parsed_date <- try_date
      }
    } else if (inherits(r$date, "POSIXt")) {
      parsed_date <- as_date(r$date)
    }
    
    r$date <- parsed_date
  }
  
  # ----- date_time normalization -----
  if (!is.null(r$date_time) && nzchar(as.character(r$date_time)) ) {
    if (!inherits(r$date_time, "character")) r$date_time <- as.character(r$date_time)
    # Attempt to parse common datetime formats
    parsed_dt <- NA
    if (is.character(r$date_time)) {
      orders_dt <- c("Ymd HMS", "Ymd HM", "YmdTz", "Ymd", "dmY HMS", "dmy HM", "mdy HMS", "mdy HM")
      for (ord in orders_dt) {
        parsed_try <- tryCatch(parse_date_time(r$date_time, orders = ord, tz = "UTC", truncated = 3), error = function(e) NA)
        if (!is.na(parsed_try)) {
          parsed_dt <- parsed_try
          break
        }
      }
      # if parsed, store as character in ISO format (text column in DB)
      if (!is.na(parsed_dt)) {
        r$date_time <- format(parsed_dt, "%Y-%m-%d %H:%M:%S")
      } else {
        # leave original string; user can inspect failures
        r$date_time <- r$date_time
      }
    }
  }
  
  # ----- time normalization (stored as text) -----
  if (!is.null(r$time) && nzchar(as.character(r$time))) {
    # Try to parse times like "19:30", "7:30 PM", etc.
    tval <- as.character(r$time)
    parsed_time <- NA
    parsed_try <- tryCatch(parse_date_time(tval, orders = c("H:M","I:M p","H:M:S")), error = function(e) NA)
    if (!is.na(parsed_try)) {
      parsed_time <- format(parsed_try, "%H:%M:%S")
      r$time <- parsed_time
    } else {
      r$time <- tval
    }
  }
  
  # ----- integer coercions -----
  int_cols <- c("indoor","game_no","no_of_courts","court_rank","p1p2_score","p3p4_score")
  for (ic in int_cols) {
    if (!is.null(r[[ic]])) {
      if (is.numeric(r[[ic]])) {
        r[[ic]] <- as.integer(r[[ic]])
      } else if (nzchar(as.character(r[[ic]]))) {
        r[[ic]] <- suppressWarnings(as.integer(as.character(r[[ic]])))
      } else {
        r[[ic]] <- NA_integer_
      }
    } else {
      r[[ic]] <- NA_integer_
    }
  }
  
  # ----- ensure all columns present -----
  for (cname in cols) {
    if (!(cname %in% names(r))) r[[cname]] <- NA
  }
  
  # Return values in the same order as cols
  vals <- lapply(cols, function(cn) r[[cn]])
  names(vals) <- cols
  vals
}

# Parameterized insert function
insert_mastersheet_row <- function(con, row) {
  row_vals <- normalize_row(row)
  
  # Validate that date is a Date or NA
  if (!is.na(row_vals$date) && !inherits(row_vals$date, "Date")) {
    stop("Normalized 'date' is not a Date object. Value: ", as.character(row_vals$date))
  }
  
  placeholders <- paste0("$", seq_along(cols), collapse = ", ")
  sql <- sprintf('INSERT INTO "public"."mastersheet" (%s) VALUES (%s);',
                 paste0('"', cols, '"', collapse = ", "), placeholders)
  dbExecute(con, sql, params = row_vals)
}

# Example single row input with mixed date formats
single_row <- list(
  date = "17/03/2026",             # dd/mm/YYYY format — lubridate will parse
  time = "7:30 PM",                # will normalize to "19:30:00"
  date_time = "2026-03-17 19:30",  # parsed and converted to "YYYY-MM-DD HH:MM:SS"
  location = "Main Hall",
  indoor = "1",
  game_no = "2001",
  no_of_courts = 4,
  court_rank = 1,
  score_method = "standard",
  p1 = "Alice",
  p2 = "Bob",
  p3 = "Carol",
  p4 = "Dave",
  p1p2_score = "21",
  p3p4_score = "18",
  event_type = "league"
)

# Insert single
tryCatch({
  insert_mastersheet_row(con, single_row)
  message("Inserted single row into public.mastersheet")
}, error = function(e) {
  warning("Insert failed: ", e$message)
})

# Example: insert multiple rows from a data.frame with mixed date formats
df_rows <- data.frame(
  date = c("2026-03-18", "18/03/2026", "03-19-2026"),
  time = c("18:00", "6:00 PM", "20:00"),
  date_time = c("2026-03-18 18:00", "18/03/2026 18:00", "2026-03-19T20:00:00"),
  location = c("Gym A", "Gym B", "Gym C"),
  indoor = c(1, 0, "1"),
  game_no = c(2002, 2003, 2004),
  no_of_courts = c(3, 2, 5),
  court_rank = c(2, 1, 3),
  score_method = c("standard", "short", "short"),
  p1 = c("Eve", "Frank", "Gina"),
  p2 = c("Grace", "Heidi", "Ivy"),
  p3 = c("Ivan", "Judy", "Ken"),
  p4 = c("Karl", "Leo", "Mona"),
  p1p2_score = c(15, 21, 19),
  p3p4_score = c(13, 19, 17),
  event_type = c("friendly", "tournament", "league"),
  stringsAsFactors = FALSE
)

# Batch insert in a transaction
tryCatch({
  dbBegin(con)
  for (i in seq_len(nrow(df_rows))) {
    row_list <- as.list(df_rows[i, , drop = TRUE])
    insert_mastersheet_row(con, row_list)
  }
  dbCommit(con)
  message("Inserted ", nrow(df_rows), " rows into public.mastersheet")
}, error = function(e) {
  dbRollback(con)
  warning("Batch insert failed: ", e$message)
})

# Close
dbDisconnect(con)