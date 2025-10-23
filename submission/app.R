  
library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)
library(janitor)
library(zoo)
library(plotly)
source("busa_helpers.R")

# Declare NSE globals for dplyr/ggplot to satisfy linters
utils::globalVariables(c(
  "building_m", "engineering_m", "gdp_growth", "cpi_inflation", "cash_rate",
  "value", "series", "value_millions", "value_m", "sector", "public_m",
  "private_m", "owner", "public_share", "approvals", "industry", "aug_2024",
  "is_construction", "idx_sa", "idx_trend", "index", "time_period", "date",
  "year", "total_m", "reference_month", "component", "yoy_pct", "dw_value_b",
  "pct_change_sa", "idx_sa_scaled", "idx_tr_scaled", "aug_2019", "month_lab"
))

# --- Helpers ---
parse_quarter_date <- function(q) {
  # Convert strings like "2023-Q1" to Date at quarter start
  as.Date(zoo::as.yearqtr(gsub("-", " ", q)))
}

# Plotly utility: clean up legend names like "(GDP,1)" => "GDP"
fix_plotly_legend_names <- function(pl) {
  if (!is.null(pl$x$data)) {
    for (i in seq_along(pl$x$data)) {
      nm <- pl$x$data[[i]]$name
      if (!is.null(nm)) {
        nm2 <- gsub("[()]", "", nm)
        nm2 <- sub(",?1$", "", nm2)
        pl$x$data[[i]]$name <- nm2
      }
    }
  }
  # Ensure expected legend interactions: single-click toggles, double-click isolates
  pl <- plotly::layout(
    pl,
    legend = list(
      itemclick = "toggle",
      itemdoubleclick = "toggleothers",
      groupclick = "toggleitem"
    )
  )
  pl
}

to_plotly <- function(p, tooltip) {
  fix_plotly_legend_names(ggplotly(p, tooltip = tooltip))
}

data_dir <- "data"

# Load via helpers (mirroring ARBE transformations)
macro_df <- load_macro_indicators(data_dir) %>% mutate(year = year(date))
gdp_contrib <- load_gdp_contributions(data_dir)
gdp_growth <- load_gdp_growth(data_dir)
cpi_long <- load_cpi_goods_services(data_dir)
dw_rents <- load_dwelling_vs_rents(data_dir)
gva_df <- load_gva(data_dir)
cwd <- load_cwd_quarterlies(data_dir)
cwd_total_q <- cwd$total %>% mutate(year = year(date), value_millions = value_m / 1000)
cwd_building_q <- cwd$building %>% mutate(year = year(date)) %>% rename(building_m = value_m)
cwd_engineering_q <- cwd$engineering %>% mutate(year = year(date)) %>% rename(engineering_m = value_m)
cwd_pub_priv_q <- load_pub_priv_quarterly(data_dir) %>%
  mutate(year = year(date), total_m = public_m + private_m, public_share = public_m / pmax(total_m, 1e-9))
approvals_m <- load_building_approvals_monthly(data_dir) %>% mutate(year = year(date))
turnover_df <- load_turnover_indicator(data_dir) %>% mutate(year = year(date))
earnings_df <- load_earnings_industry(data_dir)
cwd_fy <- load_cwd_fy(data_dir)

# --- Ranges ---
safe_years <- c(
  if ("year" %in% names(cwd_total_q)) cwd_total_q$year,
  if ("year" %in% names(cwd_pub_priv_q)) cwd_pub_priv_q$year,
  if ("year" %in% names(approvals_m)) approvals_m$year,
  if ("year" %in% names(macro_df)) macro_df$year,
  if ("year" %in% names(turnover_df)) turnover_df$year
)
year_min <- min(safe_years, na.rm = TRUE)
year_max <- max(safe_years, na.rm = TRUE)

# --- UI ---
ui <- fluidPage(
  titlePanel("Dashboard"),
  tabsetPanel(
        tabPanel(
          "Info",
          div(style = "max-width:900px; margin:16px auto; line-height:1.6;",
              h3("How to use this dashboard"),
              tags$ul(
                tags$li("Hover over charts to see detailed tooltips (exact values)."),
                tags$li("Use the legend: click to toggle a series; double-click to isolate it."),
                tags$li("Filter all time-series with the Year Range slider at the bottom."),
                tags$li("Macro Overview → use the month filter to show a single month's bars across years."),
                tags$li("Use the toolbar on each chart (top-right) to zoom, pan or download PNGs.")
              ),
              hr(),
              tags$p("Explore the tabs above for Macro, GDP, Prices & Housing, Sector and more.")
          )
        ),
        tabPanel(
          "Macro Overview",
          div(style = "padding: 0 15px 6px;",
              selectInput(
                "macro_month_filter",
                label = "Filter bars by month (optional)",
                choices = c("All months", "January", "February", "March", "April"),
                selected = "All months",
                width = "260px"
              )
          ),
          plotly::plotlyOutput("macro_gdp", height = 260),
          plotly::plotlyOutput("macro_cpi_rate", height = 260)
        ),
        tabPanel(
          "GDP",
          plotly::plotlyOutput("gdp_contrib", height = 320),
          plotly::plotlyOutput("gdp_growth", height = 320)
        ),
        tabPanel(
          "Prices & Housing",
          plotly::plotlyOutput("cpi_goods_services", height = 280),
          plotly::plotlyOutput("dwelling_vs_rents", height = 300)
        ),
        tabPanel(
          "Sector Data",
          plotly::plotlyOutput("fy_total", height = 280),
          plotly::plotlyOutput("fy_sectors", height = 280),
          div(style = "margin-top: 22px;",
              plotly::plotlyOutput("fy_public_share", height = 260)
          )
        ),
        tabPanel(
          "Public vs Private",
          plotly::plotlyOutput("pub_priv_levels", height = 320),
          div(style = "padding: 0 15px;",
              checkboxInput("show_ci", "Show confidence interval", value = TRUE)
          ),
          plotly::plotlyOutput("pub_share", height = 320)
        ),
        tabPanel(
          "Approvals",
          plotly::plotlyOutput("approvals_trend", height = 320),
          plotly::plotlyOutput("approvals_yearly", height = 280),
          plotly::plotlyOutput("gva_annual", height = 320)
        ),
        tabPanel(
          "Labour & Turnover",
          tags$div(style = "text-align:left;", plotly::plotlyOutput("earnings_bar", height = 520, width = "100%")),
          plotly::plotlyOutput("turnover_trend", height = 360),
          plotly::plotlyOutput("vacancies_lines", height = 340)
        )
  ),
  br(),
  div(class = "well well-sm", style = "padding:8px 12px; margin:6px 12px;",
      div(style = "max-width:900px; margin:0 auto;",
          sliderInput(
            "year_range", "Year Range:",
            min = year_min, max = year_max, value = c(max(year_min, 2019), year_max), step = 1, sep = "", width = "100%"
          )
      )
  )
)

# --- Server ---
server <- function(input, output, session) {
  # Reactive filters
  macro_re <- reactive({ macro_df %>% filter(year >= input$year_range[1], year <= input$year_range[2]) })
  cwd_total_re <- reactive({ cwd_total_q %>% filter(year >= input$year_range[1], year <= input$year_range[2]) })
  sectors_re <- reactive({
    left_join(cwd_building_q, cwd_engineering_q, by = c("time_period", "date", "year")) %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      pivot_longer(c(building_m, engineering_m), names_to = "sector", values_to = "value_m")
  })
  pub_priv_re <- reactive({ cwd_pub_priv_q %>% filter(year >= input$year_range[1], year <= input$year_range[2]) })
  approvals_re <- reactive({ approvals_m %>% filter(year >= input$year_range[1], year <= input$year_range[2]) })
  approvals_yearly <- reactive({
    # Prefer pre-aggregated calendar-year totals to match original PNG
    path_cy <- file.path(data_dir, "building_approvals_calendar_year.csv")
    if (file.exists(path_cy)) {
      readr::read_csv(path_cy, show_col_types = FALSE) %>%
        janitor::clean_names() %>%
        filter(year >= input$year_range[1], year <= input$year_range[2])
    } else {
      # Fallback: aggregate monthly to calendar year
      path_m <- file.path(data_dir, "building_approvals_monthly.csv")
      if (file.exists(path_m)) {
        readr::read_csv(path_m, show_col_types = FALSE) %>%
          janitor::clean_names() %>%
          transmute(date = lubridate::ym(time_period), approvals = as.numeric(obs_value)) %>%
          mutate(year = lubridate::year(date)) %>%
          group_by(year) %>% summarise(dwellings_approved = sum(approvals, na.rm = TRUE), .groups = "drop") %>%
          filter(year >= input$year_range[1], year <= input$year_range[2])
      } else tibble::tibble()
    }
  })
  turnover_re <- reactive({ turnover_df %>% filter(year >= input$year_range[1], year <= input$year_range[2]) })

  # Macro plots
  output$gdp_contrib <- renderPlotly({
    df <- gdp_contrib %>% filter(!is.na(date), date >= as.Date("2020-01-01"))
    df_long <- df %>% pivot_longer(cols = c(private_demand, net_trade, public_demand), names_to = "component", values_to = "ppt") %>%
      mutate(component = recode(component,
        private_demand = "Private demand",
        net_trade = "Net trade",
        public_demand = "Public demand"
      ))
    p <- ggplot() +
      geom_col(data = df_long, aes(x = date, y = ppt, fill = component), width = 25) +
      geom_line(data = df, aes(x = date, y = gdp_ppt, color = "GDP"), linewidth = 1.2) +
      scale_fill_manual(values = c("Private demand" = "#899DBA", "Net trade" = "#F5B041", "Public demand" = "#C0392B")) +
      scale_color_manual(values = c("GDP" = "#2E86AB"), guide = "legend") +
      labs(title = "Contributions to quarterly growth in GDP",
           x = "Quarterly", y = "ppt", fill = NULL, color = NULL) +
      theme_minimal() + theme(legend.position = "bottom") +
      scale_x_date(date_breaks = "6 months", date_labels = "%b-%y")
    fix_plotly_legend_names(ggplotly(p, tooltip = c("x","y","fill","color")))
  })

  output$gdp_growth <- renderPlotly({
    df <- gdp_growth %>% filter(!is.na(date), date >= as.Date("2020-01-01"))
    p <- ggplot(df, aes(x = date)) +
      geom_col(aes(y = qoq_pct, fill = "Quarterly growth"), width = 25) +
      geom_line(aes(y = tty_pct, color = "Through the year"), linewidth = 1.2) +
      scale_fill_manual(values = c("Quarterly growth" = "#5DADE2")) +
      scale_color_manual(values = c("Through the year" = "#154360")) +
      labs(title = "Gross domestic product (chain volume, SA)", x = "Quarterly", y = "%", fill = NULL, color = NULL) +
      theme_minimal() + theme(legend.position = "bottom") +
      scale_x_date(date_breaks = "6 months", date_labels = "%b-%y")
    fix_plotly_legend_names(ggplotly(p, tooltip = c("x","y","fill","color")))
  })
  output$macro_gdp <- renderPlotly({
    df <- macro_re() %>%
      mutate(month_lab = factor(month(date, label = TRUE, abbr = TRUE), levels = month.abb),
             month_full = factor(month(date, label = TRUE, abbr = FALSE), levels = month.name))
    # Optional filter by month
    if (!is.null(input$macro_month_filter) && input$macro_month_filter != "All months") {
      df <- df %>% filter(month_full == input$macro_month_filter)
    }
    p <- ggplot(df, aes(date, gdp_growth, fill = month_lab)) +
      geom_col(width = 25) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      scale_fill_brewer(palette = "Set3", name = "Month") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(title = "GDP growth (%)", x = "Year", y = "%") +
      theme_minimal() + theme(legend.position = "bottom")
    fix_plotly_legend_names(ggplotly(p, tooltip = c("x","y","fill")))
  })

  output$macro_cpi_rate <- renderPlotly({
    df <- macro_re() %>%
      select(date, cpi_inflation, cash_rate) %>%
      pivot_longer(-date, names_to = "series", values_to = "value") %>%
      mutate(series = recode(series,
                             cpi_inflation = "CPI Inflation",
                             cash_rate = "Cash Rate"))
    p <- ggplot(df, aes(date, value, color = series)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = c("CPI Inflation" = "#f46d43", "Cash Rate" = "#1a9850"),
                         breaks = c("CPI Inflation", "Cash Rate"), name = NULL) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      labs(title = "Inflation and Cash Rate", x = "Year", y = "%") +
      theme_minimal() + theme(legend.position = "bottom")
    fix_plotly_legend_names(ggplotly(p, tooltip = c("x","y","series")))
  })

  # Construction FY (ARBE-style)
  output$fy_total <- renderPlotly({
    df <- cwd_fy
    p <- ggplot(df, aes(year, total_b)) +
      geom_line(linewidth = 1.2, color = "#253494") +
      geom_point(size = 2, color = "#253494") +
      scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "B", accuracy = 0.1)) +
      scale_x_continuous(breaks = df$year) +
      labs(title = "Total Construction Work Done (FY, CVM SA)", x = "Year", y = "AUD billions") +
      theme_minimal()
    fix_plotly_legend_names(ggplotly(p, tooltip = c("x","y")))
  })

  output$fy_sectors <- renderPlotly({
    df <- cwd_fy %>% select(year, building_b, engineering_b) %>% pivot_longer(-year, names_to = "sector", values_to = "value_b") %>%
      mutate(sector = recode(sector, building_b = "Residential Building", engineering_b = "Engineering"))
    p <- ggplot(df, aes(year, value_b, color = sector)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "B", accuracy = 0.1)) +
      scale_x_continuous(breaks = cwd_fy$year) +
      labs(title = "Construction Sector Performance (FY, CVM SA)", x = "Year", y = "AUD billions", color = NULL) +
      theme_minimal() + theme(legend.position = "bottom")
    fix_plotly_legend_names(ggplotly(p, tooltip = c("x","y","sector")))
  })

  output$fy_public_share <- renderPlotly({
    # Show a pie for the most relevant FY within the selected range
    yr_sel <- min(max(cwd_fy$year, na.rm = TRUE), input$year_range[2])
    latest <- cwd_fy %>% filter(year == yr_sel)
    if (nrow(latest) == 0) return(NULL)
    vals <- c(as.numeric(latest$public_share), as.numeric(100 - latest$public_share))
    labs <- c("Public", "Private")
    plotly::plot_ly(
      labels = labs,
      values = vals,
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      marker = list(colors = c("#2E86AB", "#A23B72"))
    ) %>%
      plotly::layout(title = list(text = paste0("Public vs Private Sector Construction (FY ", yr_sel, ")")),
                     showlegend = TRUE)
  })

  # Public vs Private
  output$pub_priv_levels <- renderPlotly({
    df <- pub_priv_re() %>% select(date, public_m, private_m) %>% pivot_longer(-date, names_to = "owner", values_to = "value_m") %>%
      mutate(owner = recode(owner, private_m = "Private", public_m = "Public"),
             value_b = value_m / 1000)
    p <- ggplot(df, aes(date, value_b, color = owner)) +
      geom_line(linewidth = 1) +
      scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "B", accuracy = 0.1)) +
      scale_color_manual(values = c("Private" = "#d95f02", "Public" = "#1b9e77"),
                         breaks = c("Private","Public"), name = NULL) +
      labs(title = "Value of Construction Work Done", x = "Year", y = "AUD billions") +
      theme_minimal() + theme(legend.position = "bottom") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")
    fix_plotly_legend_names(ggplotly(p, tooltip = c("x","y","owner")))
  })

  output$pub_share <- renderPlotly({
    df <- pub_priv_re()
    p <- ggplot(df, aes(date, public_share)) +
      geom_point(color = "#1b9e77", size = 2, alpha = 0.6) +
      geom_smooth(method = "lm", se = input$show_ci, color = "#1b9e77", fill = "#1b9e77", alpha = 0.2) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(title = "Public Share of Total Construction Work Done", x = "Year", y = "Share of total") +
      theme_minimal() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")
    fix_plotly_legend_names(ggplotly(p, tooltip = c("x","y")))
  })

  # Approvals (monthly boxplot across years by calendar month)
  output$approvals_trend <- renderPlotly({
    df <- approvals_re() %>%
      mutate(
        month_lab = factor(month(date, label = TRUE, abbr = TRUE), levels = month.abb)
      )
    p <- ggplot(df, aes(x = month_lab, y = approvals)) +
      geom_boxplot(outlier.alpha = 0.5, width = 0.7,
                   fill = "#FFF3BF", color = "#000000") +
      labs(title = "Building Approvals (Monthly box plot by calendar month 2021-2025)",
           x = "Month", y = "Approvals") +
      theme_minimal()
    plt <- fix_plotly_legend_names(ggplotly(p, tooltip = c("x","y")))
    # Add legend entries explaining quartiles
    plt <- plt %>%
      plotly::add_trace(x = c(NA), y = c(NA), type = "scatter", mode = "markers",
                        marker = list(color = "#FFF3BF"), name = "Q1 (first quartile)") %>%
      plotly::add_trace(x = c(NA), y = c(NA), type = "scatter", mode = "markers",
                        marker = list(color = "#FFF3BF"), name = "Q3 (third quartile)")
    plt
  })

  output$gva_annual <- renderPlotly({
    if (nrow(gva_df) == 0) return(NULL)
    df <- gva_df
    p <- ggplot(df, aes(date, total)) +
      geom_line(linewidth = 1.2, color = "#333333") +
      labs(title = "Construction GVA (CVM annual)", x = "Year", y = "$m CVM") +
      theme_minimal()
    ggplotly(p, tooltip = c("x","y"))
  })

  # Building approvals (calendar year totals)
  output$approvals_yearly <- renderPlotly({
    df <- approvals_yearly()
    if (nrow(df) == 0) return(NULL)
    p <- ggplot(df, aes(x = year, y = dwellings_approved)) +
      geom_line(linewidth = 1.2, color = "#1B9E77") +
      geom_point(size = 2.2, color = "#1B9E77") +
      labs(title = "Dwellings Approved (Calendar Year)",
           subtitle = "ABS 8731.0 (BA_SA2) – monthly counts aggregated to year",
           x = "Year", y = "Dwellings approved") +
      theme_minimal()
    fix_plotly_legend_names(ggplotly(p, tooltip = c("x","y")))
  })

  # Labour & Turnover
  output$earnings_bar <- renderPlotly({
    # highlight Construction relative to others
    df <- earnings_df %>% mutate(Aug_2019 = aug_2019, Aug_2024 = aug_2024) %>%
      select(industry, Aug_2019, Aug_2024) %>%
      tidyr::pivot_longer(cols = c(Aug_2019, Aug_2024), names_to = "period", values_to = "aud") %>%
      filter(!is.na(aud) & is.finite(aud) & aud > 0, industry != "")
    order_24 <- df %>% filter(period == "Aug_2024") %>% arrange(aud) %>% pull(industry)
    df$industry <- factor(df$industry, levels = order_24)
    x_max <- ceiling(max(df$aud, na.rm = TRUE) / 100) * 100
    p <- ggplot(df, aes(x = aud, y = industry, fill = period)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      scale_fill_manual(values = c("Aug_19" = "#5DADE2", "Aug_2019" = "#5DADE2", "Aug_2024" = "#1F618D"), guide = "legend") +
      labs(title = "Median weekly earnings in main job, by industry",
           subtitle = "Aug-19 vs Aug-24 (AUD)", x = "Weekly Earnings ($)", y = NULL, fill = NULL) +
      scale_x_continuous(limits = c(0, x_max), breaks = seq(0, x_max, by = 100),
                         labels = scales::label_number(accuracy = 1, big.mark = "")) +
      theme_minimal() + theme(legend.position = "bottom")
    ggplotly(p, tooltip = c("x","y"))
  })

  output$turnover_trend <- renderPlotly({
    df <- turnover_re()
    
    # Calculate proper range for right y-axis (index values)
    min_idx <- min(c(df$idx_sa, df$idx_trend), na.rm = TRUE)
    max_idx <- max(c(df$idx_sa, df$idx_trend), na.rm = TRUE)
    idx_range <- c(floor(min_idx/5)*5, ceiling(max_idx/5)*5)
    
    # Create plotly dual-axis plot
    fig <- plot_ly(df, x = ~date)
    
    # Add bars for monthly % change (left y-axis)
    fig <- fig %>%
      add_bars(y = ~pct_change_sa, name = "Monthly % change",
               marker = list(color = "#F39C12", opacity = 0.8),
               yaxis = "y")
    
    # Add line for Index SA (right y-axis)
    fig <- fig %>%
      add_lines(y = ~idx_sa, name = "Index (SA)",
                line = list(color = "#7FB3D5", width = 2),
                yaxis = "y2")
    
    # Add line for Index trend (right y-axis)
    fig <- fig %>%
      add_lines(y = ~idx_trend, name = "Index (trend)",
                line = list(color = "#1F618D", width = 2),
                yaxis = "y2")
    
    # Configure dual y-axes
    fig <- fig %>%
      layout(
        title = list(text = "Construction, turnover indicator", x = 0, xanchor = "left"),
        xaxis = list(
          title = list(text = "Month", standoff = 20)
        ),
        yaxis = list(
          title = "%",
          side = "left"
        ),
        yaxis2 = list(
          title = "",
          overlaying = "y",
          side = "right",
          range = idx_range,
          dtick = 5
        ),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
        hovermode = "x unified",
        margin = list(b = 80, r = 60, l = 60)
      )
    
    fig
  })

  # Vacancies by industry (inline tibble as per ARBE script)
  output$vacancies_lines <- renderPlotly({
    quarters <- factor(c("Feb-2024","May-2024","Aug-2024","Nov-2024","Feb-2025","May-2025"),
                       levels = c("Feb-2024","May-2024","Aug-2024","Nov-2024","Feb-2025","May-2025"))
    vac_df <- tibble(
      quarter = quarters,
      Mining = c(10.7, 8.5, 8.4, 9.6, 10.3, 10.0),
      Manufacturing = c(23.3, 16.4, 14.7, 14.8, 18.2, 19.6),
      `Electricity, gas, water & waste` = c(4.6, 4.5, 4.3, 4.5, 3.9, 3.4),
      Construction = c(27.9, 24.9, 25.1, 22.2, 18.5, 22.3)
    )
    vac_long <- vac_df %>% pivot_longer(-quarter, names_to = "industry", values_to = "vacancies_k")
    p <- ggplot(vac_long, aes(x = quarter, y = vacancies_k, color = industry, group = industry)) +
      geom_line(linewidth = 1.1) + geom_point(size = 2) +
      labs(title = "Job Vacancies by Industry", subtitle = "ABS May 2025",
           x = "Quarter", y = "Vacancies ('000)", color = "Industry") +
      theme_minimal() + theme(legend.position = "bottom")
    ggplotly(p, tooltip = c("x","y","color"))
  })

  # Prices & Housing
  output$cpi_goods_services <- renderPlotly({
    # Filter by selected year range and match ARBE plotting style
    yr0 <- input$year_range[1]; yr1 <- input$year_range[2]
    df <- cpi_long %>%
      filter(!is.na(date)) %>%
      filter(lubridate::year(date) >= yr0, lubridate::year(date) <= yr1)
    p <- ggplot(df, aes(date, yoy_pct, color = component)) +
      geom_line(linewidth = 1.1) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      scale_color_manual(values = c("Goods" = "#2E86AB", "Services" = "#A93226"), name = NULL) +
      labs(title = "CPI: Goods vs Services – annual movement",
           subtitle = "Australia, 2020–2025 (quarterly observations)", x = "Quarterly", y = "% YoY") +
      theme_minimal() + theme(legend.position = "bottom") +
      scale_x_date(date_breaks = "6 months", date_labels = "%b-%y")
    fix_plotly_legend_names(ggplotly(p, tooltip = c("x","y","component")))
  })

  output$dwelling_vs_rents <- renderPlotly({
    yr0 <- input$year_range[1]; yr1 <- input$year_range[2]
    dw <- dw_rents$dw %>% filter(lubridate::year(date) >= yr0, lubridate::year(date) <= yr1)
    rt <- dw_rents$rents %>% filter(lubridate::year(date) >= yr0, lubridate::year(date) <= yr1)
    if (nrow(dw) == 0 || nrow(rt) == 0) return(NULL)
    merged <- dw %>% left_join(rt, by = "date")
    max_left <- max(merged$dw_value_b, na.rm = TRUE)
    max_right <- max(merged$yoy_pct, na.rm = TRUE)
    scale_factor <- ifelse(is.finite(max_left / max_right), max_left / max_right, 1)
    p <- ggplot(merged, aes(date)) +
      geom_line(aes(y = dw_value_b, color = "Dwelling stock ($b)"), linewidth = 1.1) +
      geom_line(aes(y = yoy_pct * scale_factor, color = "Rents YoY (%)"), linewidth = 1.1, linetype = "dashed") +
      scale_y_continuous(name = "Total dwelling stock ($b)", sec.axis = sec_axis(~ ./scale_factor, name = "Rents YoY (%)")) +
      scale_color_manual(values = c("Dwelling stock ($b)" = "#1F618D", "Rents YoY (%)" = "#A04000"), name = NULL) +
      labs(title = "Dwelling value vs Rents (YoY)", x = "Year") +
      theme_minimal() + theme(legend.position = "bottom")
    fix_plotly_legend_names(ggplotly(p, tooltip = c("x")))
  })
}

shinyApp(ui, server)


