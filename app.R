# ============================================================
# KENYA EQUITABLE SHARE ALLOCATION INTELLIGENCE DASHBOARD
# 2013/2014 - 2026/2027
# ============================================================

# 1. PACKAGES -------------------------------------------------
{
  library(shiny)
 library(bslib)
 library(tidyverse)
 library(plotly)
 library(DT)
 library(scales)
 library(janitor)
 library(lubridate)
}

# 2. DATA IMPORT -----------------------------------------------

equitable_raw <- read_csv(
  "data/County_Revenue_Model_v2 - Stacked Direct Revenue Data.csv",
  show_col_types = FALSE
) |>
  clean_names()

# Check required columns
required_cols <- c(
  "financial_year",
  "county_name",
  "total_equitable_share_ksh"
)

missing_cols <- setdiff(required_cols, names(equitable_raw))

if (length(missing_cols) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}


# 3. COUNTY STANDARDISATION ------------------------------------

county_lookup <- tribble(
  ~county_name, ~county,

  "MOMBASA", "Mombasa",
  "KWALE", "Kwale",
  "KILIFI", "Kilifi",
  "TANA-RIVER", "Tana River",
  "TANA RIVER", "Tana River",
  "LAMU", "Lamu",
  "TAITA-TAVETA", "Taita Taveta",
  "TAITA TAVETA", "Taita Taveta",
  "TAITA", "Taita Taveta",

  "GARISSA", "Garissa",
  "WAJIR", "Wajir",
  "MANDERA", "Mandera",
  "MARSABIT", "Marsabit",
  "ISIOLO", "Isiolo",
  "MERU", "Meru",

  "THARAKA-NITHI", "Tharaka-Nithi",
  "THARAKA NITHI", "Tharaka-Nithi",
  "THARAKANITHI", "Tharaka-Nithi",

  "EMBU", "Embu",
  "KITUI", "Kitui",
  "MACHAKOS", "Machakos",
  "MAKUENI", "Makueni",

  "NYANDARUA", "Nyandarua",
  "NYERI", "Nyeri",
  "KIRINYAGA", "Kirinyaga",
  "MURANG'A", "Murang'a",
  "MURANGA", "Murang'a",
  "KIAMBU", "Kiambu",

  "TURKANA", "Turkana",
  "WEST POKOT", "West Pokot",
  "SAMBURU", "Samburu",

  "TRANS-NZOIA", "Trans Nzoia",
  "TRANS NZOIA", "Trans Nzoia",
  "TRANZAOIA", "Trans Nzoia",

  "UASIN-GISHU", "Uasin Gishu",
  "UASIN GISHU", "Uasin Gishu",
  "UASINGISHU", "Uasin Gishu",

  "ELGEYO-MARAKWET", "Elgeyo-Marakwet",
  "ELGEYO MARAKWET", "Elgeyo-Marakwet",
  "ELGEYO/MARAKWET", "Elgeyo-Marakwet",

  "NANDI", "Nandi",
  "BARINGO", "Baringo",
  "LAIKIPIA", "Laikipia",
  "NAKURU", "Nakuru",
  "NAROK", "Narok",
  "KAJIADO", "Kajiado",
  "KERICHO", "Kericho",
  "BOMET", "Bomet",

  "KAKAMEGA", "Kakamega",
  "VIHIGA", "Vihiga",
  "BUNGOMA", "Bungoma",
  "BUSIA", "Busia",

  "SIAYA", "Siaya",
  "KISUMU", "Kisumu",
  "HOMA-BAY", "Homa Bay",
  "HOMA BAY", "Homa Bay",
  "MIGORI", "Migori",
  "KISII", "Kisii",
  "NYAMIRA", "Nyamira",

  "NAIROBI CITY", "Nairobi",
  "NAIROBI", "Nairobi"
)


# 4. FORMER PROVINCE CLASSIFICATION ----------------------------

regional_lookup <- tribble(
  ~county, ~region,

  # Coast
  "Mombasa", "Coast",
  "Kwale", "Coast",
  "Kilifi", "Coast",
  "Tana River", "Coast",
  "Lamu", "Coast",
  "Taita Taveta", "Coast",

  # North Eastern
  "Garissa", "North Eastern",
  "Wajir", "North Eastern",
  "Mandera", "North Eastern",

  # Eastern
  "Marsabit", "Eastern",
  "Isiolo", "Eastern",
  "Meru", "Eastern",
  "Tharaka-Nithi", "Eastern",
  "Embu", "Eastern",
  "Kitui", "Eastern",
  "Machakos", "Eastern",
  "Makueni", "Eastern",

  # Central
  "Nyandarua", "Central",
  "Nyeri", "Central",
  "Kirinyaga", "Central",
  "Murang'a", "Central",
  "Kiambu", "Central",

  # Rift Valley
  "Turkana", "Rift Valley",
  "West Pokot", "Rift Valley",
  "Samburu", "Rift Valley",
  "Trans Nzoia", "Rift Valley",
  "Uasin Gishu", "Rift Valley",
  "Elgeyo-Marakwet", "Rift Valley",
  "Nandi", "Rift Valley",
  "Baringo", "Rift Valley",
  "Laikipia", "Rift Valley",
  "Nakuru", "Rift Valley",
  "Narok", "Rift Valley",
  "Kajiado", "Rift Valley",
  "Kericho", "Rift Valley",
  "Bomet", "Rift Valley",

  # Western
  "Kakamega", "Western",
  "Vihiga", "Western",
  "Bungoma", "Western",
  "Busia", "Western",

  # Nyanza
  "Siaya", "Nyanza",
  "Kisumu", "Nyanza",
  "Homa Bay", "Nyanza",
  "Migori", "Nyanza",
  "Kisii", "Nyanza",
  "Nyamira", "Nyanza",

  # Nairobi
  "Nairobi", "Nairobi"
)

region_order <- c(
  "Coast",
  "North Eastern",
  "Eastern",
  "Central",
  "Rift Valley",
  "Western",
  "Nyanza",
  "Nairobi"
)

# ============================================================
# 5. CLEAN AND PREPARE DATA
# ============================================================

equitable <- equitable_raw |>

  mutate(

    county_key = county_name |>
      str_to_upper() |>
      str_squish(),

    financial_year = financial_year |>
      str_remove(regex("^FY\\s*", ignore_case = TRUE)) |>
      str_squish(),

    fy_start = as.integer(
      str_extract(financial_year, "^\\d{4}")
    ),

    total_equitable_share_ksh = parse_number(
      as.character(total_equitable_share_ksh)
    )

  ) |>

  # Standardise county names
  left_join(
    county_lookup,
    by = c("county_key" = "county_name")
  ) |>

  mutate(
    county = coalesce(
      county,
      str_to_title(county_key)
    )
  ) |>

  select(-county_key) |>

  # Assign former provinces
  left_join(
    regional_lookup,
    by = "county"
  ) |>

  mutate(
    region = factor(
      region,
      levels = region_order
    )
  ) |>

  filter(
    !is.na(fy_start),
    !is.na(total_equitable_share_ksh),
    !is.na(county)
  ) |>

  arrange(
    fy_start,
    county
  )


# ============================================================
# 6. DATA QUALITY CHECKS
# ============================================================

# Check unmatched counties

unmatched_counties <- equitable |>
  filter(is.na(region)) |>
  distinct(county)

if (nrow(unmatched_counties) > 0) {

  stop(
    "Unmatched counties found: ",
    paste(unmatched_counties$county, collapse = ", ")
  )

}


# Check county-year duplicates

duplicate_records <- equitable |>

  count(
    county,
    financial_year
  ) |>

  filter(n > 1)

if (nrow(duplicate_records) > 0) {

  warning(
    "Duplicate county-year observations detected. ",
    "Verify whether they represent separate components ",
    "or duplicated records before aggregation."
  )

}


# ============================================================
# 7. PRESERVE FINANCIAL YEARS
# ============================================================

# Keep FY 2021/2022 and FY 2022/2023 as separate
# financial years, even if their allocations are identical.

fy_levels <- equitable |>

  distinct(
    financial_year,
    fy_start
  ) |>

  arrange(fy_start) |>

  pull(financial_year)


# Convert financial year to an ordered factor

equitable <- equitable |>

  mutate(
    financial_year = factor(
      financial_year,
      levels = fy_levels,
      ordered = TRUE
    )
  )


# ============================================================
# 8. AGGREGATE COUNTY-YEAR OBSERVATIONS
# ============================================================

equitable <- equitable |>

  group_by(
    county,
    region,
    financial_year,
    fy_start
  ) |>

  summarise(

    total_equitable_share_ksh = sum(
      total_equitable_share_ksh,
      na.rm = TRUE
    ),

    .groups = "drop"

  ) |>

  arrange(
    fy_start,
    county
  )


# ============================================================
# 9. FINAL VALIDATION
# ============================================================

# Number of counties

county_coverage <- equitable |>
  distinct(county) |>
  nrow()


# Number of financial years

financial_year_coverage <- equitable |>
  distinct(financial_year) |>
  nrow()


# Check FY 2021/2022 and FY 2022/2023

fy_comparison <- equitable |>

  filter(
    financial_year %in% c(
      "2021/2022",
      "2022/2023"
    )
  ) |>

  select(
    county,
    financial_year,
    total_equitable_share_ksh
  ) |>

  pivot_wider(
    names_from = financial_year,
    values_from = total_equitable_share_ksh,
    names_prefix = "FY_"
  )


# Compare the two financial years

if (
  all(c("FY_2021/2022", "FY_2022/2023") %in%
      names(fy_comparison))
) {

  fy_comparison <- fy_comparison |>

    mutate(
      difference =
        `FY_2022/2023` - `FY_2021/2022`
    )

  print(fy_comparison)

}


# Display validation results

cat("\n========== DATA VALIDATION ==========\n")

cat("Counties represented:", county_coverage, "\n")

cat("Financial years:", financial_year_coverage, "\n")

cat(
  "Financial year sequence:",
  paste(fy_levels, collapse = " | "),
  "\n"
)

cat(
  "Total equitable share:",
  scales::comma(
    sum(equitable$total_equitable_share_ksh)
  ),
  "\n"
)

cat("=====================================\n")














# ============================================================
# 10. DASHBOARD CONFIGURATION
# ============================================================

library(WeightedTreemaps)

# Kenya-inspired colour palette
kenya_colours <- list(
  black = "#111111",
  red = "#BB0000",
  green = "#006B3F",
  white = "#FFFFFF",
  gold = "#D6A84B",
  grey = "#64748B",
  light = "#F4F6F4"
)

# Colours for the eight former provinces
region_colours <- c(
  "Coast"         = "#006B3F",
  "North Eastern" = "#3D8B68",
  "Eastern"       = "#8BAF42",
  "Central"       = "#D6A84B",
  "Rift Valley"   = "#BB0000",
  "Western"       = "#8E2525",
  "Nyanza"        = "#333333",
  "Nairobi"       = "#64748B"
)

# Formatting helper
format_ksh <- function(x) {
  case_when(
    is.na(x) ~ "—",
    abs(x) >= 1e12 ~ paste0("KSh ", number(x / 1e12, accuracy = 0.01), "T"),
    abs(x) >= 1e9  ~ paste0("KSh ", number(x / 1e9, accuracy = 0.01), "B"),
    abs(x) >= 1e6  ~ paste0("KSh ", number(x / 1e6, accuracy = 0.01), "M"),
    TRUE ~ paste0("KSh ", comma(x))
  )
}

# Percentage helper
format_pct <- function(x) {
  ifelse(is.na(x), "—", percent(x, accuracy = 0.1))
}

# Financial year labels in chronological order
fy_lookup <- equitable |>
  distinct(fy_start, financial_year) |>
  arrange(fy_start)

# Validate that the treemap package is available
if (!requireNamespace("WeightedTreemaps", quietly = TRUE)) {
  stop("Please install WeightedTreemaps using install.packages('WeightedTreemaps').")
}


# ============================================================
# 11. SHINY THEME
# ============================================================

kenya_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = kenya_colours$green,
  secondary = kenya_colours$grey,
  success = kenya_colours$green,
  danger = kenya_colours$red,
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto")
)


# ============================================================
# 12. REUSABLE UI COMPONENTS
# ============================================================

kpi_card <- function(title, output_id, subtitle = NULL, accent = kenya_colours$green) {

  div(
    class = "kpi-card",
    style = paste0("border-top: 4px solid ", accent, ";"),
    div(class = "kpi-title", title),
    div(class = "kpi-value", textOutput(output_id, inline = TRUE)),
    if (!is.null(subtitle)) {
      div(class = "kpi-subtitle", subtitle)
    }
  )
}

section_header <- function(title, description = NULL) {
  tagList(
    h3(class = "section-heading", title),
    if (!is.null(description)) {
      p(class = "section-description", description)
    }
  )
}


# ============================================================
# 13. USER INTERFACE
# ============================================================

ui <- fluidPage(

  theme = kenya_theme,

  tags$head(
    tags$style(HTML("
      body {
        background-color: #F4F6F4;
        color: #202820;
        font-family: 'Roboto', sans-serif;
      }

      .dashboard-banner {
        background: linear-gradient(110deg, #111111 0%, #262626 42%,
                    #BB0000 42%, #BB0000 58%, #006B3F 58%, #006B3F 100%);
        color: white;
        padding: 27px 30px;
        border-radius: 10px;
        margin: 15px 0 22px 0;
        box-shadow: 0 4px 12px rgba(0,0,0,0.12);
      }

      .dashboard-banner h1 {
        font-size: 27px;
        font-weight: 800;
        margin: 0 0 7px 0;
        letter-spacing: 0.3px;
      }

      .dashboard-banner p {
        margin: 0;
        opacity: 0.9;
        font-size: 14px;
      }

      .kpi-card {
        background: white;
        border-radius: 9px;
        padding: 18px 20px;
        margin-bottom: 17px;
        min-height: 120px;
        box-shadow: 0 2px 9px rgba(0,0,0,0.055);
      }

      .kpi-title {
        color: #64748B;
        font-size: 12px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.7px;
      }

      .kpi-value {
        color: #111111;
        font-size: 25px;
        font-weight: 800;
        margin-top: 10px;
      }

      .kpi-subtitle {
        color: #84908A;
        font-size: 11px;
        margin-top: 5px;
      }

      .section-heading {
        font-size: 19px;
        font-weight: 750;
        color: #17251C;
        margin-top: 12px;
        margin-bottom: 4px;
      }

      .section-description {
        color: #758078;
        font-size: 12px;
        margin-bottom: 15px;
      }

      .panel-card {
        background: white;
        padding: 18px;
        border-radius: 9px;
        margin-bottom: 20px;
        box-shadow: 0 2px 9px rgba(0,0,0,0.045);
      }

      .sidebar {
        background: white;
        border-radius: 9px;
        padding: 16px;
      }

      .control-label {
        font-weight: 650;
        color: #34443A;
      }

      .nav-tabs > li > a {
        font-weight: 650;
      }

      .dataTables_wrapper {
        font-size: 12px;
      }

      .footer-note {
        color: #87918A;
        font-size: 11px;
        text-align: center;
        padding: 20px 0 12px 0;
      }
    "))
  ),

  # Dashboard banner
  div(
    class = "dashboard-banner",
    h1("KENYA EQUITABLE SHARE INTELLIGENCE"),
    p("County allocation trends | Regional comparisons | Fiscal year analysis"),
    p("2013/2014 – 2026/2027", style = "margin-top: 5px; font-weight: 700;")
  ),

  sidebarLayout(

    sidebarPanel(
      width = 3,
      class = "sidebar",

      h4("Dashboard filters"),

      selectInput(
        "year_range",
        "Financial year range",
        choices = setNames(
          fy_lookup$fy_start,
          as.character(fy_lookup$financial_year)
        ),
        selected = range(fy_lookup$fy_start),
        multiple = TRUE
      ),

      selectizeInput(
        "region_filter",
        "Former province",
        choices = region_order,
        selected = region_order,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),

      selectizeInput(
        "county_filter",
        "County",
        choices = sort(unique(equitable$county)),
        selected = sort(unique(equitable$county)),
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = "Select counties"
        )
      ),

      hr(),

      actionButton(
        "reset_filters",
        "Reset filters",
        class = "btn btn-outline-secondary btn-sm w-100"
      ),

      hr(),

      div(
        style = "font-size: 11px; color: #78837B;",
        strong("Data coverage"),
        br(),
        paste0(county_coverage, " counties represented"),
        br(),
        paste0(financial_year_coverage, " financial years"),
        br(),
        "Geographical classification: former provinces"
      )
    ),

    mainPanel(
      width = 9,

      tabsetPanel(

        id = "dashboard_tabs",

        # ----------------------------------------------------
        # TAB 1: NATIONAL OVERVIEW
        # ----------------------------------------------------

        tabPanel(
          "National Overview",

          br(),

          fluidRow(
            column(
              3,
              kpi_card(
                "Total allocation",
                "kpi_total",
                "Across selected years and geography",
                kenya_colours$green
              )
            ),
            column(
              3,
              kpi_card(
                "Latest year allocation",
                "kpi_latest",
                "Latest selected financial year",
                kenya_colours$red
              )
            ),
            column(
              3,
              kpi_card(
                "Year-on-year growth",
                "kpi_growth",
                "Latest selected year",
                kenya_colours$black
              )
            ),
            column(
              3,
              kpi_card(
                "Counties represented",
                "kpi_counties",
                "Within current selection",
                kenya_colours$gold
              )
            )
          ),

          div(
            class = "panel-card",
            section_header(
              "Equitable share allocation trend",
              "Annual allocation across the selected geographical scope."
            ),
            plotlyOutput("national_trend", height = "350px")
          ),

          fluidRow(
            column(
              6,
              div(
                class = "panel-card",
                section_header(
                  "Leading counties",
                  "Counties ranked by allocation in the latest selected year."
                ),
                plotlyOutput("top_counties", height = "340px")
              )
            ),

            column(
              6,
              div(
                class = "panel-card",
                section_header(
                  "Regional allocation composition",
                  "How the selected allocation is distributed across regions."
                ),
                plotlyOutput("regional_share", height = "340px")
              )
            )
          )
        ),

        # ----------------------------------------------------
        # TAB 2: REGIONAL ANALYSIS
        # ----------------------------------------------------

        tabPanel(
          "Regional Analysis",

          br(),

          div(
            class = "panel-card",
            section_header(
              "Regional allocation landscape",
              "Voronoi treemap: each region's cell area represents its share of the selected year's equitable share allocation."
            ),
            plotOutput("regional_voronoi", height = "560px")
          ),

          div(
            class = "panel-card",
            section_header(
              "Regional allocation comparison",
              "Compare the allocation received by each former province."
            ),
            plotlyOutput("regional_comparison", height = "390px")
          ),

          div(
            class = "panel-card",
            section_header(
              "Regional allocation summary",
              "Allocation, share of total and county coverage."
            ),
            DTOutput("regional_table")
          )
        ),

        # ----------------------------------------------------
        # TAB 3: COUNTY ANALYSIS
        # ----------------------------------------------------

        tabPanel(
          "County Analysis",

          br(),

          div(
            class = "panel-card",
            section_header(
              "County allocation rankings",
              "Compare county allocations for the latest selected financial year."
            ),
            plotlyOutput("county_ranking", height = "520px")
          ),

          div(
            class = "panel-card",
            section_header(
              "County allocation trend",
              "Track the allocation of a selected county over time."
            ),

            selectInput(
              "trend_county",
              "Select county",
              choices = sort(unique(equitable$county)),
              selected = "Nairobi"
            ),

            plotlyOutput("county_trend", height = "350px")
          ),

          div(
            class = "panel-card",
            section_header(
              "County allocation table",
              "Detailed county-level allocation figures."
            ),
            DTOutput("county_table")
          )
        ),

        # ----------------------------------------------------
        # TAB 4: DATA EXPLORER
        # ----------------------------------------------------

        tabPanel(
          "Data Explorer",

          br(),

          div(
            class = "panel-card",
            section_header(
              "Explore the allocation dataset",
              "Search, sort and download the filtered county-year observations."
            ),
            downloadButton(
              "download_data",
              "Download filtered data",
              class = "btn btn-success btn-sm"
            ),
            br(),
            br(),
            DTOutput("data_table")
          )
        )
      )
    )
  ),

  div(
    class = "footer-note",
    "KENYA EQUITABLE SHARE INTELLIGENCE DASHBOARD | Data-driven fiscal analysis"
  )
)


# ============================================================
# 14. SERVER
# ============================================================

server <- function(input, output, session) {

  # ----------------------------------------------------------
  # FILTER RESET
  # ----------------------------------------------------------

  observeEvent(input$reset_filters, {

    updateSelectInput(
      session,
      "year_range",
      selected = range(fy_lookup$fy_start)
    )

    updateSelectizeInput(
      session,
      "region_filter",
      selected = region_order
    )

    updateSelectizeInput(
      session,
      "county_filter",
      selected = sort(unique(equitable$county))
    )

  })


  # ----------------------------------------------------------
  # FILTERED DATA
  # ----------------------------------------------------------

  filtered_data <- reactive({

    req(input$year_range)
    req(input$region_filter)
    req(input$county_filter)

    selected_years <- as.numeric(input$year_range)

    equitable |>
      filter(
        fy_start >= min(selected_years),
        fy_start <= max(selected_years),
        as.character(region) %in% input$region_filter,
        county %in% input$county_filter
      ) |>
      arrange(fy_start, county)

  })


  # Latest selected year
  latest_year_data <- reactive({

    dat <- filtered_data()

    validate(
      need(nrow(dat) > 0, "No observations match the selected filters.")
    )

    latest_year <- max(dat$fy_start, na.rm = TRUE)

    dat |>
      filter(fy_start == latest_year)

  })


  # ----------------------------------------------------------
  # AGGREGATIONS
  # ----------------------------------------------------------

  annual_summary <- reactive({

    filtered_data() |>
      group_by(financial_year, fy_start) |>
      summarise(
        allocation = sum(total_equitable_share_ksh, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(fy_start) |>
      mutate(
        growth = allocation / lag(allocation) - 1
      )

  })


  regional_summary <- reactive({

    latest_year_data() |>
      group_by(region) |>
      summarise(
        allocation = sum(total_equitable_share_ksh, na.rm = TRUE),
        counties = n_distinct(county),
        .groups = "drop"
      ) |>
      mutate(
        share = allocation / sum(allocation),
        region = factor(region, levels = region_order)
      ) |>
      arrange(desc(allocation))

  })


  county_summary <- reactive({

    latest_year_data() |>
      group_by(county, region) |>
      summarise(
        allocation = sum(total_equitable_share_ksh, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(allocation))

  })


  # ----------------------------------------------------------
  # KPI OUTPUTS
  # ----------------------------------------------------------

  output$kpi_total <- renderText({

    format_ksh(
      sum(filtered_data()$total_equitable_share_ksh, na.rm = TRUE)
    )

  })


  output$kpi_latest <- renderText({

    format_ksh(
      sum(latest_year_data()$total_equitable_share_ksh, na.rm = TRUE)
    )

  })


  output$kpi_growth <- renderText({

    growth_data <- annual_summary()

    if (nrow(growth_data) < 2) {
      return("—")
    }

    latest_growth <- tail(growth_data$growth, 1)

    format_pct(latest_growth)

  })


  output$kpi_counties <- renderText({

    paste(
      n_distinct(filtered_data()$county),
      "of 47"
    )

  })


  # ----------------------------------------------------------
  # NATIONAL ALLOCATION TREND
  # ----------------------------------------------------------

  output$national_trend <- renderPlotly({

    dat <- annual_summary()

    validate(
      need(nrow(dat) > 0, "No data available for this selection.")
    )

    p <- ggplot(
      dat,
      aes(
        x = fy_start,
        y = allocation,
        group = 1,
        text = paste0(
          "Financial year: ", financial_year,
          "<br>Allocation: ", format_ksh(allocation),
          "<br>Growth: ", format_pct(growth)
        )
      )
    ) +

      geom_area(
        fill = kenya_colours$green,
        alpha = 0.12
      ) +

      geom_line(
        colour = kenya_colours$green,
        linewidth = 1.3
      ) +

      geom_point(
        colour = kenya_colours$red,
        size = 3
      ) +

      scale_x_continuous(
        breaks = dat$fy_start,
        labels = as.character(dat$financial_year)
      ) +

      scale_y_continuous(
        labels = label_number(
          scale = 1e-9,
          suffix = "B"
        )
      ) +

      labs(
        x = NULL,
        y = "Allocation (KSh billions)"
      ) +

      theme_minimal(base_size = 12) +

      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 15, 10, 10)
      )

    ggplotly(
      p,
      tooltip = "text"
    ) |>
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(b = 80),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Roboto, sans-serif", color = "#333333")
      ) |>
      config(
        displayModeBar = "hover",
        displaylogo = FALSE,
        responsive = TRUE
      )

  })


  # ----------------------------------------------------------
  # TOP COUNTIES
  # ----------------------------------------------------------

  output$top_counties <- renderPlotly({

    dat <- county_summary() |>
      slice_max(
        allocation,
        n = 10,
        with_ties = FALSE
      ) |>
      mutate(
        county = fct_reorder(county, allocation)
      )

    validate(
      need(nrow(dat) > 0, "No county data available.")
    )

    p <- ggplot(
      dat,
      aes(
        x = allocation,
        y = county,
        text = paste0(
          county,
          "<br>Region: ", region,
          "<br>Allocation: ", format_ksh(allocation)
        )
      )
    ) +

      geom_col(width = 0.7, fill = kenya_colours$green) +

      scale_x_continuous(
        labels = label_number(
          scale = 1e-9,
          suffix = "B"
        )
      ) +

      labs(
        x = "Allocation (KSh billions)",
        y = NULL
      ) +

      theme_minimal(base_size = 11) +

      theme(
        legend.position = "none",
        panel.grid.minor = element_blank()
      )

    ggplotly(p, tooltip = "text") |>
      layout(
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Roboto, sans-serif", color = "#333333")
      ) |>
      config(
        displayModeBar = "hover",
        displaylogo = FALSE,
        responsive = TRUE
      )

  })


  # ----------------------------------------------------------
  # REGIONAL SHARE DONUT
  # ----------------------------------------------------------

  output$regional_share <- renderPlotly({

    dat <- regional_summary()

    validate(
      need(nrow(dat) > 0, "No regional data available.")
    )

    plot_ly(
      dat,
      labels = ~region,
      values = ~allocation,
      type = "pie",
      hole = 0.62,
      sort = FALSE,
      marker = list(
        colors = unname(
          region_colours[as.character(dat$region)]
        ),
        line = list(
          color = "#FFFFFF",
          width = 2
        )
      ),
      textinfo = "label+percent",
      hovertemplate = paste(
        "<b>%{label}</b>",
        "<br>Allocation: KSh %{value:,.0f}",
        "<br>Share: %{percent}",
        "<extra></extra>"
      )
    ) |>
      layout(
        showlegend = FALSE,
        margin = list(l = 10, r = 10, t = 15, b = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Roboto, sans-serif", color = "#333333")
      ) |>
      config(
        displayModeBar = "hover",
        displaylogo = FALSE,
        responsive = TRUE
      )

  })


# ---------------------------------------------------------
# CIRCULAR VORONOI TREEMAP: REGIONAL ALLOCATIONS
# ---------------------------------------------------------

output$regional_voronoi <- renderPlot({

  dat <- filtered_data()
  req(nrow(dat) > 0)

  # Aggregate allocations by former province
  voronoi_data <- dat |>
    group_by(region) |>
    summarise(
      allocation = sum(total_equitable_share_ksh, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(is.finite(allocation), allocation > 0) |>
    mutate(region = as.character(region)) |>
    arrange(match(region, region_order))

  validate(
    need(
      nrow(voronoi_data) >= 2,
      "Select at least two former provinces to display the circular Voronoi treemap."
    )
  )

  # Kenya-inspired regional palette
  region_palette <- c(
    "Coast"          = "#006B3F",
    "North Eastern"  = "#3D8B68",
    "Eastern"        = "#8BAF42",
    "Central"        = "#D6A84B",
    "Rift Valley"    = "#BB0000",
    "Western"        = "#8E2525",
    "Nyanza"         = "#333333",
    "Nairobi"        = "#64748B"
  )

  # Generate circular Voronoi treemap
  regional_voronoi <- WeightedTreemaps::voronoiTreemap(
    data = voronoi_data,
    levels = "region",
    cell_size = "allocation",
    shape = "circle",
    sort = FALSE,
    maxIteration = 150,
    positioning = "regular",
    seed = 123
  )

  # Draw treemap
  WeightedTreemaps::drawTreemap(
    treemap = regional_voronoi,
    color_type = "categorical",
    color_level = 1,
    color_palette = unname(region_palette[voronoi_data$region]),
    label_level = 1,
    label_size = 1.15,
    label_color = "white",
    label_autoscale = TRUE,
    border_color = "white",
    border_size = 3,
    title = NULL,
    legend = FALSE,
    width = 0.72,
    height = 0.72
  )

},

# Smaller square plotting area
height = function() {
  plot_width <- session$clientData$output_regional_voronoi_width

  if (is.null(plot_width) || !is.finite(plot_width) || plot_width < 1) {
    450
  } else {
    min(plot_width * 0.75, 500)
  }
},

res = 120
)


# ---------------------------------------------------------
# REGIONAL COMPARISON: LATEST SELECTED FINANCIAL YEAR
# ---------------------------------------------------------

output$regional_comparison <- renderPlotly({

  dat <- latest_year_data()

  req(nrow(dat) > 0)

  selected_fy_label <- unique(as.character(dat$financial_year))
  selected_fy_label <- selected_fy_label[1]

  # Aggregate allocation by former province
  regional_data <- dat |>
    group_by(region) |>
    summarise(
      allocation = sum(
        total_equitable_share_ksh,
        na.rm = TRUE
      ),
      county_count = n_distinct(county),
      .groups = "drop"
    ) |>
    filter(
      is.finite(allocation),
      allocation > 0
    ) |>
    mutate(
      share = allocation / sum(allocation) * 100,
      region = as.character(region)
    ) |>
    arrange(allocation) |>
    mutate(
      region = factor(region, levels = region)
    )

  validate(
    need(
      nrow(regional_data) > 0,
      "No regional allocation data available for the selected filters."
    )
  )

  # Regional colour palette
  region_palette <- c(
    "Coast"         = "#006B3F",
    "North Eastern" = "#3D8B68",
    "Eastern"       = "#8BAF42",
    "Central"       = "#D6A84B",
    "Rift Valley"   = "#BB0000",
    "Western"       = "#8E2525",
    "Nyanza"        = "#333333",
    "Nairobi"       = "#64748B"
  )

  regional_data <- regional_data |>
    mutate(
      colour = unname(
        region_palette[as.character(region)]
      ),
      tooltip = paste0(
        "<b>", region, "</b>",
        "<br>Allocation: ",
        scales::comma(allocation),
        "<br>Share: ",
        round(share, 2), "%",
        "<br>Counties: ",
        county_count
      )
    )

  plot_ly(
    data = regional_data,
    x = ~allocation,
    y = ~region,
    type = "bar",
    orientation = "h",
    marker = list(
      color = regional_data$colour,
      line = list(
        color = "#FFFFFF",
        width = 1
      )
    ),
    text = ~paste0(round(share, 1), "%"),
    textposition = "outside",
    hovertext = ~tooltip,
    hoverinfo = "text"
  ) |>
    layout(
      title = list(
        text = paste(
          "Regional Allocation Comparison",
          paste0("<br><sup>", selected_fy_label, "</sup>")
        ),
        x = 0.02,
        font = list(size = 15)
      ),
      xaxis = list(
        title = "Equitable Share Allocation (KSh)",
        tickformat = ",.2s",
        showgrid = TRUE,
        gridcolor = "#E8ECEF",
        zeroline = FALSE
      ),
      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = levels(regional_data$region)
      ),
      margin = list(
        l = 110,
        r = 65,
        t = 75,
        b = 65
      ),
      showlegend = FALSE,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(
        family = "Roboto, sans-serif",
        color = "#333333"
      )
    ) |>
    config(
      displayModeBar = "hover",
      displaylogo = FALSE,
      responsive = TRUE
    )

})

  # ----------------------------------------------------------
  # REGIONAL SUMMARY TABLE
  # ----------------------------------------------------------

  output$regional_table <- renderDT({

    dat <- regional_summary() |>
      mutate(
        allocation = format_ksh(allocation),
        share = format_pct(share)
      ) |>
      select(
        Region = region,
        Counties = counties,
        Allocation = allocation,
        `Share of total` = share
      )

    datatable(
      dat,
      rownames = FALSE,
      options = list(
        pageLength = 8,
        dom = "tip",
        ordering = FALSE
      )
    )

  })


  # ----------------------------------------------------------
  # COUNTY RANKING
  # ----------------------------------------------------------

  output$county_ranking <- renderPlotly({

    dat <- county_summary() |>
      mutate(
        county = fct_reorder(
          county,
          allocation
        )
      )

    validate(
      need(nrow(dat) > 0, "No county data available.")
    )

    p <- ggplot(
      dat,
      aes(
        x = allocation,
        y = county,
        text = paste0(
          "County: ", county,
          "<br>Region: ", region,
          "<br>Allocation: ", format_ksh(allocation)
        )
      )
    ) +

      geom_col(width = 0.72, fill = kenya_colours$green) +

      scale_x_continuous(
        labels = label_number(
          scale = 1e-9,
          suffix = "B"
        )
      ) +

      labs(
        x = "Allocation (KSh billions)",
        y = NULL
      ) +

      theme_minimal(base_size = 11) +

      theme(
      
        legend.position = "none",
        panel.grid.minor = element_blank()
      )

    ggplotly(p, tooltip = "text") |>
      layout(
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Roboto, sans-serif", color = "#333333")
      ) |>
      config(
        displayModeBar = "hover",
        displaylogo = FALSE,
        responsive = TRUE
      )

  })


  # ----------------------------------------------------------
  # INDIVIDUAL COUNTY TREND
  # ----------------------------------------------------------

  output$county_trend <- renderPlotly({

    req(input$trend_county)

    dat <- filtered_data() |>
      filter(county == input$trend_county) |>
      group_by(financial_year, fy_start) |>
      summarise(
        allocation = sum(
          total_equitable_share_ksh,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      arrange(fy_start)

    validate(
      need(nrow(dat) > 0, "No observations for this county within the selected filters.")
    )

    p <- ggplot(
      dat,
      aes(
        x = fy_start,
        y = allocation,
        group = 1,
        text = paste0(
          "Financial year: ", financial_year,
          "<br>Allocation: ", format_ksh(allocation)
        )
      )
    ) +

      geom_area(
        fill = kenya_colours$red,
        alpha = 0.10
      ) +

      geom_line(
        colour = kenya_colours$red,
        linewidth = 1.2
      ) +

      geom_point(
        colour = kenya_colours$green,
        size = 3
      ) +

      scale_x_continuous(
        breaks = dat$fy_start,
        labels = as.character(dat$financial_year)
      ) +

      scale_y_continuous(
        labels = label_number(
          scale = 1e-9,
          suffix = "B"
        )
      ) +

      labs(
        x = NULL,
        y = "Allocation (KSh billions)"
      ) +

      theme_minimal(base_size = 12) +

      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        panel.grid.minor = element_blank()
      )

    ggplotly(p, tooltip = "text") |>
      layout(
        margin = list(b = 80),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Roboto, sans-serif", color = "#333333")
      ) |>
      config(
        displayModeBar = "hover",
        displaylogo = FALSE,
        responsive = TRUE
      )

  })


  # ----------------------------------------------------------
  # COUNTY TABLE
  # ----------------------------------------------------------

  output$county_table <- renderDT({

    dat <- county_summary() |>
      arrange(desc(allocation)) |>
      mutate(
        allocation_display = format_ksh(allocation)
      ) |>
      select(
        County = county,
        Region = region,
        `Allocation (KSh)` = allocation_display
      )

    datatable(
      dat,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "tip"
      )
    )

  })


  # ----------------------------------------------------------
  # DATA EXPLORER
  # ----------------------------------------------------------

  output$data_table <- renderDT({

    dat <- filtered_data() |>
      mutate(
        financial_year = as.character(financial_year),
        total_equitable_share_ksh = round(
          total_equitable_share_ksh,
          2
        )
      ) |>
      select(
        Financial_year = financial_year,
        County = county,
        Region = region,
        `Allocation_KSh` = total_equitable_share_ksh
      )

    datatable(
      dat,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    )

  })


  # ----------------------------------------------------------
  # DOWNLOAD FILTERED DATA
  # ----------------------------------------------------------

  output$download_data <- downloadHandler(

    filename = function() {
      paste0(
        "Kenya_Equitable_Share_Allocations_",
        Sys.Date(),
        ".csv"
      )
    },

    content = function(file) {

      filtered_data() |>
        mutate(
          financial_year = as.character(financial_year),
          region = as.character(region)
        ) |>
        write_csv(file)

    }

  )

}


# ============================================================
# 15. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
