library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(janitor)
library(stringr)
library(zoo)

# Parsing helpers mirroring ARBE scripts
parse_month_abb <- function(x) {
  if (is.na(x) || !grepl("^[A-Za-z]{3}-[0-9]{2,4}$", x)) return(NA)
  mon <- match(substr(x, 1, 3), month.abb)
  yr <- substr(x, 5, nchar(x))
  yr_num <- as.integer(yr)
  yr_full <- ifelse(nchar(yr) == 2, ifelse(yr_num >= 50, 1900 + yr_num, 2000 + yr_num), yr_num)
  as.Date(sprintf("%04d-%02d-01", yr_full, mon))
}

parse_quarter_str <- function(q) {
  as.Date(zoo::as.yearqtr(gsub("-", " ", q)))
}

clean_num <- function(x) as.numeric(gsub("[^0-9.-]", "", x))

# appease linters for NSE columns
utils::globalVariables(c(
  "gdp_ppt","private_demand","net_trade","public_demand","qoq_pct","tty_pct",
  "Goods","Services","dw_value_b","yoy_pct","time_period","obs_value",
  "reference_month","aug_2019","aug_2024"
))

# Loaders following prevData/script logic
load_macro_indicators <- function(data_dir) {
  read_csv(file.path(data_dir, "macro_indicators_final.csv"), show_col_types = FALSE) %>%
    clean_names() %>% mutate(date = as.Date(date))
}

load_gdp_contributions <- function(data_dir) {
  path <- file.path(data_dir, "Contributions to quarterly growth in GDP, chain volume measures, seasonally adjusted.csv")
  if (!file.exists(path)) return(tibble())
  df <- read.csv(path, skip = 1, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- c("period", "gdp_ppt", "private_demand", "net_trade", "public_demand")
  df %>%
    filter(!is.na(period) & !grepl("^Source:", period)) %>%
    mutate(date = sapply(period, parse_month_abb)) %>%
    filter(!is.na(date)) %>%
    select(date, gdp_ppt, private_demand, net_trade, public_demand)
}

load_gdp_growth <- function(data_dir) {
  path <- file.path(data_dir, "Gross domestic product, chain volume measures, seasonally adjusted.csv")
  if (!file.exists(path)) return(tibble())
  df <- read.csv(path, skip = 1, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- c("period", "qoq_pct", "tty_pct")
  df %>%
    filter(!is.na(period) & !grepl("^Source:", period)) %>%
    mutate(date = sapply(period, parse_month_abb)) %>%
    filter(!is.na(date)) %>%
    select(date, qoq_pct, tty_pct)
}

load_cpi_goods_services <- function(data_dir) {
  path <- file.path(data_dir, "CPI Goods and Services components, annual movement (%).csv")
  if (!file.exists(path)) return(tibble())
  # File structure: title line, then a header line with an empty first column.
  # Read as data (no header), skip first 2 lines, then assign clean names.
  raw <- read.csv(path, header = FALSE, skip = 2, stringsAsFactors = FALSE, fill = TRUE, check.names = FALSE)
  raw <- raw[, 1:3, drop = FALSE]
  names(raw) <- c("period", "Goods", "Services")
  raw %>%
    filter(!is.na(period) & period != "") %>%
    mutate(date = as.Date(sapply(period, parse_month_abb), origin = "1970-01-01"),
           Goods = clean_num(Goods), Services = clean_num(Services)) %>%
    filter(!is.na(date)) %>%
    select(date, Goods, Services) %>%
    pivot_longer(c(Goods, Services), names_to = "component", values_to = "yoy_pct")
}

load_dwelling_vs_rents <- function(data_dir) {
  dw_path <- file.path(data_dir, "Total value of dwelling stock, Australia.csv")
  r_path  <- file.path(data_dir, "Rents, quarterly and annual movement (%).csv")
  if (!file.exists(dw_path) || !file.exists(r_path)) return(list(dw = tibble(), rents = tibble()))
  dw <- read.csv(dw_path, skip = 1, stringsAsFactors = FALSE, check.names = FALSE)
  names(dw) <- c("period", "dw_value_b")
  dw <- dw %>% mutate(date = as.Date(sapply(period, parse_month_abb), origin = "1970-01-01"),
                      dw_value_b = clean_num(dw_value_b)) %>%
    filter(!is.na(date)) %>% select(date, dw_value_b)
  r <- read.csv(r_path, skip = 1, stringsAsFactors = FALSE, check.names = FALSE)
  names(r) <- c("period", "qoq_pct", "yoy_pct")
  r <- r %>% mutate(date = as.Date(sapply(period, parse_month_abb), origin = "1970-01-01"),
                    yoy_pct = clean_num(yoy_pct)) %>%
    filter(!is.na(date)) %>% select(date, yoy_pct)
  list(dw = dw, rents = r)
}

load_cwd_quarterlies <- function(data_dir) {
  total <- read_csv(file.path(data_dir, "cwd_total_quarterly.csv"), show_col_types = FALSE) %>% clean_names()
  build <- read_csv(file.path(data_dir, "cwd_building_quarterly.csv"), show_col_types = FALSE) %>% clean_names()
  eng   <- read_csv(file.path(data_dir, "cwd_engineering_quarterly.csv"), show_col_types = FALSE) %>% clean_names()
  prep <- function(df) {
    df %>% transmute(
      time_period, date = parse_quarter_str(time_period),
      value_m = as.numeric(obs_value)
    )
  }
  list(
    total = prep(total),
    building = prep(build),
    engineering = prep(eng)
  )
}

load_pub_priv_quarterly <- function(data_dir) {
  read_csv(file.path(data_dir, "cwd_public_private_quarterly_merged.csv"), show_col_types = FALSE) %>%
    clean_names() %>% mutate(date = parse_quarter_str(time_period))
}

load_building_approvals_monthly <- function(data_dir) {
  read_csv(file.path(data_dir, "building_approvals_monthly.csv"), show_col_types = FALSE) %>%
    clean_names() %>% transmute(date = ym(time_period), approvals = as.numeric(obs_value))
}

load_turnover_indicator <- function(data_dir) {
  path <- file.path(data_dir, "Construction, turnover indicator.csv")
  if (!file.exists(path)) return(tibble())
  read_csv(path, skip = 1, show_col_types = FALSE) %>%
    clean_names() %>%
    # drop meta rows (e.g., Index base period, Source)
    filter(!is.na(reference_month),
           !grepl("^Index base", reference_month, ignore.case = TRUE),
           !grepl("^Source", reference_month, ignore.case = TRUE)) %>%
    # normalize column names to a stable set
    rename_with(~"idx_sa", matches("business_turnover_index.*seasonally_adjusted.*pts")) %>%
    rename_with(~"idx_trend", matches("business_turnover_index.*trend.*pts")) %>%
    rename_with(~"pct_change_sa", matches("monthly_percentage_change.*seasonally_adjusted")) %>%
    mutate(date = my(reference_month))
}

load_earnings_industry <- function(data_dir) {
  read_csv(file.path(data_dir, "Median weekly earnings in main job, by industry.csv"), skip = 1,
           col_names = c("industry","aug_2019","aug_2024"), show_col_types = FALSE) %>%
    mutate(aug_2019 = readr::parse_number(aug_2019), aug_2024 = readr::parse_number(aug_2024)) %>%
    # drop footnotes/metadata rows from the CSV
    filter(!(is.na(aug_2019) & is.na(aug_2024))) %>%
    filter(!grepl("^Source:", industry), !grepl("^1\\.", industry))
}

load_gva <- function(data_dir) {
  path <- file.path(data_dir, "gva.csv")
  if (!file.exists(path)) return(tibble())
  df <- read_csv(path, show_col_types = FALSE)
  names(df)[1] <- "period"
  df %>%
    mutate(date = as.Date(sapply(period, parse_month_abb), origin = "1970-01-01")) %>%
    select(date, total = `Construction (E) ;`) %>%
    filter(!is.na(date))
}

# FY loaders and ARBE-style plot builders
load_cwd_fy <- function(data_dir) {
  total <- read_csv(file.path(data_dir, "cwd_total_fy.csv"), show_col_types = FALSE) %>% clean_names()
  build <- read_csv(file.path(data_dir, "cwd_building_fy.csv"), show_col_types = FALSE) %>% clean_names()
  eng   <- read_csv(file.path(data_dir, "cwd_engineering_fy.csv"), show_col_types = FALSE) %>% clean_names()
  pubpr <- read_csv(file.path(data_dir, "cwd_public_private_fy.csv"), show_col_types = FALSE) %>% clean_names()
  df <- total %>%
    separate(fy_label, into = c("fy_start","fy_end_2"), sep = "-", remove = FALSE) %>%
    mutate(year = as.integer(fy_start), total_b = cwd_total_cvm_sa_millions / 1000) %>%
    left_join(build, by = "fy_label") %>%
    left_join(eng, by = "fy_label") %>%
    left_join(pubpr, by = "fy_label") %>%
    mutate(
      building_b = building_cvm_sa_m / 1000,
      engineering_b = engineering_cvm_sa_m / 1000,
      public_b = public_tot_cvm_sa_m / 1000,
      private_b = private_tot_cvm_sa_m / 1000,
      public_share = ifelse(!is.na(public_b) & !is.na(total_b), 100 * public_b / total_b, NA_real_)
    )
  df
}



