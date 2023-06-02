

library(tidyverse)
library(janitor)
library(haven)
library(gt)
library(gtExtras)

data <- read.csv("sample_data.csv")

data2 <- data |> as_factor()

list_of_var <- list("q_1", "q_2", "q_3")



fre3 <- function(data, var) {
    name <- var
    label <- labelled::var_label(data[[var]])
    result <- count(data, !!rlang::ensym(var))

    result |>
        mutate(label = label) |>
        mutate(var = name) |>
        rename(value = 1) |>
        relocate(var, label, value, n)
}


sample_data <- map_dfr(list_of_var, fre3, data = data2)


sample_data <- sample_data |>
    rename(group = var, variable = value, value = n) |>
    drop_na() |>
    mutate(variable = as.character(variable))


sample_data <- sample_data |> data.frame()

plots <- sample_data |>
    mutate(group2 = group) |>
    group_by(group) |>
    nest() |>
    mutate(plot = map(data, function(df) {
        df |>
            mutate(variable = fct_relevel(
                variable,
                "Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"
            )) |>
            ggplot(aes(x = group2, y = value, fill = variable)) +
            geom_bar(stat = "identity", position = "fill") +
            scale_fill_manual(values = c("#6A9BC3", "#92C0DF", "#ECECEC", "#FCBE75", "#F59C3C")) +
            coord_flip() +
            theme_void() +
            theme(legend.position = "none")
    }))


gt_table <- sample_data |>
    pivot_wider(names_from = variable, values_from = value) |>
    select(label) |>
    mutate(dist = NA) |>
    gt() |>
    text_transform(
        locations = cells_body(columns = c(dist)),
        fn = function(x) {
            map(plots$plot, ggplot_image, height = px(20), aspect_ratio = 9)
        }
    ) |>
    gtExtras::gt_theme_espn() |>
    tab_options(data_row.padding = "3px")



gt_table
