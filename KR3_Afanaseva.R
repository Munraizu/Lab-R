library(class)
library(caret)
library(lattice)
library(e1071)
library(mailR)

df <- read.csv("iris.csv")

set.seed(123)
indexes <- sample(1:nrow(iris), nrow(iris) * 0.7)
train_data <- iris[indexes, ]  # Обучающая выборка
test_data <- iris[-indexes, ]  # Тестовая выборка

k <- 3
predicted_species <- knn(train_data[, 1:4], test_data[, 1:4], train_data[, 5], k) 

test_data$predicted_species <- predicted_species

graph <- ggplot(test_data, aes(x = Sepal.Length, y = Sepal.Width, color = predicted_species)) + geom_point()

file_name <- "graph.png"
ggsave(file_name, plot = graph)

sender <- "notbelweep@mail.ru"
password <- "passoword"  
recipient <- "rizidaa@list.ru"

subject <- "График"
body <- "Прислан график"
attachment <- file.path(getwd(), file_name)

send.mail(from = sender,
          to = recipient,
          subject = subject,
          body = body,
          smtp = list(host.name = "smtp.mail.ru", port = 465, ssl = TRUE,
                      user.name = sender, passwd = password),
          attach.files = attachment,
          authenticate = TRUE,
          send = TRUE)
