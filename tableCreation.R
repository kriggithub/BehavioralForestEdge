library(knitr)
library(dplyr)

results <- tribble(
  ~Response, ~Null, ~Linear, ~Power,  ~Logistic,  ~Segmented, ~Changepoint, ~PseudoR2,    ~DEI,
  "(A) Distance from Nearest Neighbors", 70.57, 68.77,   65.95, 67.96, 67.73, 75.81, "NA", "357 (292-376)",
  "(A) Number of Nearest Neighbors", 24.42, 23.5,   24.31, 26.39, 25.71, 40.43, "NA", "252 (17-368)",
  "(A) % Time Feeding", 159.1, 152.43,   153.16, 155.22, 154.52, 173.03, "NA", "278 (38-405)",
  "(A) % Time Moving", 143.33, 145.33,   147.32, 147.58, 148.46, 154.4, "NA", "No Edge Effect",
  "(A) % Time Resting", 169.49, 168.41,   170.11, 164.76, 171.12, 182.37, "NA", "121 (62-179)",
  "(R) Distance from Nearest Neighbors", 91.34, 83.14,   83.39, 84.35, 83.62, 112.14, "NA", "327 (37-467)",
  "(R) Number of Nearest Neighbors", 26.46, 15.2,   17.19, 11.49, 17.03, 30.23, "NA", "148 (121-175)",
  "(R) % Time Feeding", 217.73, 218.9,   220.37, 220.66, 218.51, 232.91, "NA", "No Edge Effect",
  "(R) % Time Moving", 184.56, 185.13,   182.07, 181.64, 179.75, 234.04, "NA", "238 (110-365)",
  "(R) % Time Resting", 234.15, 233.59,   235.59, 236.35, 235.13, 265.19, "NA", "327 (24-469)"
)

library(gt)

results %>%
  gt() %>%
  tab_header(
    title = "All Edge Model Fitting",
    subtitle = "AIC and Pseudo-R² Comparison"
  ) %>%  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "(A) Distance from Nearest Neighbors",
      columns = Power
    )
  ) %>%  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "(A) Number of Nearest Neighbors",
      columns = Linear
    )
  ) %>%  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "(A) % Time Feeding",
      columns = Linear
    )
  ) %>%  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "(A) % Time Moving",
      columns = Null
    )
  ) %>%  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "(A) % Time Resting",
      columns = Logistic
    )
  ) %>%  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "(R) Distance from Nearest Neighbors",
      columns = Linear
    )
  ) %>%  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "(R) Number of Nearest Neighbors",
      columns = Logistic
    )
  ) %>%  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "(R) % Time Feeding",
      columns = Null
    )
  ) %>%  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "(R) % Time Moving",
      columns = Segmented
    )
  ) %>%  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "(R) % Time Resting",
      columns = Linear
    )
  )



