my_function <- function() {
  data_txt <- suppressWarnings(read.table("лр3.txt", header = FALSE))
  data_console <- readline(prompt = "Введите текст: ")
  df5 <- data.frame(txt_file = data_txt, console_input = data_console)
  print(df5)
}

my_function()
