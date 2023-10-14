install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

convert_files <- function() {
  # Шаг 1: создание 5 txt-файлов с числовыми значениями
  for (i in 1:5) {
    write.table(runif(10), file = paste0("file", i, ".txt"), row.names = FALSE, col.names = FALSE)
  }
  
  # Шаг 2: чтение файлов и создание переменных
  txt_files <- list.files(pattern = "*.txt")
  for (i in seq_along(txt_files)) {
    file_name <- txt_files[i]
    file_path <- file.path(getwd(), file_name)
    variable_name <- paste0("X", i)
    assign(variable_name, read.table(file_path)$V1)
  }
  
  # Шаг 3: создание графика
  combined_data <- NULL
  for (i in 1:5) {
    variable_name <- paste0("X", i)
    df <- data.frame(steps = i, value = get(variable_name))
    if (is.null(combined_data)) {
      combined_data <- df
    } else {
      combined_data <- bind_rows(combined_data, df)
    }
  }
  ggplot(combined_data, aes(x = steps, y = value)) + geom_point()
  
  # Шаг 4: объединение строк в дата-фрейм и вывод в консоль
  combined_df <- NULL
  for (i in 1:5) {
    variable_name <- paste0("X", i)
    df <- data.frame(get(variable_name))
    if (is.null(combined_df)) {
      combined_df <- df
    } else {
      combined_df <- bind_cols(combined_df, df)
    }
  }
  print(combined_df)
}

