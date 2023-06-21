library(tidyverse)
library(echarts4r)
library(readxl)

# Variables: Year	ADC_w_Assumed_ROA	ADC_w_RecurringRecession_ROA	SQ_w_Assumed_ROA	SQ_w_RecurringRecession_ROA
# Read in data from chart_inputs.xlsx
file_path <- getwd()
data_file <- "/Charts/chart_inputs.xlsx"
theme_file <- "/Charts/echarts_theme.json"
chart1 <- read_xlsx(paste0(file_path, data_file), sheet = "ER Contr Amount")

chart1 |>
  dplyr::filter(Year >= 2020) |>
  dplyr::mutate(Year = as.character(Year)) |>
  e_charts(Year, renderer = "svg", height = 400, areaStyle = list(opacity = 1)) |> # You can adjust height here
  e_line(SQ_w_Assumed_ROA,
         name = "Status Quo - Assumed Return",
         symbol = "none",
         lineStyle = list(width = 3.5)
  ) |>
  e_line(ADC_w_Assumed_ROA,
         name = "ADEC - Assumed Return",
         # color="green",
         symbol = "none",
         lineStyle = list(width = 3.5)
  ) |>
  e_line(SQ_w_RecurringRecession_ROA,
         name = "Status Quo - Double Recession",
         symbol = "none",
         lineStyle = list(width = 3.5)
  ) |>
  e_line(ADC_w_RecurringRecession_ROA,
         name = "ADEC - Double Recession",
         symbol = "none",
         lineStyle = list(width = 3.5)
  ) |>
  e_x_axis(
    axisLine = list(lineStyle = list(color = "#d3d3d3bf")),
    axisTick = list(alignWithLabel = T, lineStyle = list(color = "#d3d3d3bf")),
    axisLabel = list(fontSize = 14, color = "#333", interval = 3, showMaxLabel = T)
  ) |>
  echarts4r::e_y_axis(
    max = 8000,
    min = 0,
    offset = 0,
    axisTick = list(show = T, length = 30, lineStyle = list(color = "#d3d3d3bf")),
    axisLabel = list(fontSize = 14, color = "#333", inside = T, verticalAlign = "bottom", padding = c(0, 0, 2, 0), margin = -30),
    nameLocation = "middle",
    nameTextStyle = list(
      fontSize = 14,
      color = "gray",
      padding = c(2, 2, 25, 2)
    ),
    formatter = echarts4r::e_axis_formatter(style = "currency", digits = 0)
  ) |>
  e_tooltip(
    trigger = "axis",
    borderRadius = 0,
    borderWidth = 1,
    borderColor = "#ccc",
    axisPointer = list(lineStyle = list(type = "solid", width = 2.5)),
    formatter = e_tooltip_pointer_formatter(
      style = "decimal",
      digits = 0
    ),
    textStyle = list(fontSize = 14),
    confine = T
  ) |>
  e_axis_pointer(label = list(show = F)) |>
  e_theme_custom(paste0(file_path, theme_file)) |>
  e_grid(top = "17%", bottom = "10%", right = "5%") |>
  e_text_style(fontFamily = "'Open Sans', sans-serif") |>
  e_legend(
    type = "scroll",
    top = "6%",
    itemGap = 5,
    textStyle = list(fontSize = 11),
    icons = c("circle", "circle", "circle", "circle")
  ) |>
  e_title(
    text = "Employer Contribution Amount ($ Millions)",
    left = "0%",
    top = "0%",
    textStyle = list(fontSize = 20, fontWeight = "700")
  ) |>
  e_toolbox_feature(feature = "saveAsImage")
