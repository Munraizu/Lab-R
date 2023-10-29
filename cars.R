setClass("Car",
         slots = list(
           wheels = "numeric",
           driverseat = "character",
           passangerseats = "numeric"
         )
)

setGeneric("printCar")

setMethod("print",
    signature = "Car",
    definition = function(x) {
        cr <- x
        cat("Колес: ", cr@wheels, "\n")
        cat("Водительское сидение: ", cr@driverseat, "\n")
        cat("Пассажирских сидений: ", cr@passangerseats, "\n")
        cat("Название: ", cr@name, "\n")
        cat("Гудит как: ", cr@bip, "\n")
        cat("Ездит на ", cr@fuel, "\n")
    }
)

setClass("Disel",
         contains = "Car",
         slots = list(
           name = "character",
           bip = "character",
           fuel = "character"
         )
)

setClass("Electro",
         contains = "Car",
         slots = list(
           name = "character",
           bip = "character",
           fuel = "character"
         )
)

setClass("Benz",
         contains = "Car",
         slots = list(
           name = "character",
           bip = "character",
           fuel = "character"
         )
)

disel <- new("Disel",
             wheels = 4,
             driverseat = "cлева",
             passangerseats = 3,
             name = "дизель",
             bip = "виу-виу",
             fuel = "топливе"
)

electro <- new("Electro",
               wheels = 4,
               driverseat = "cлева",
               passangerseats = 3,
               name = "электромобиль",
               bip = "пам-пам",
               fuel = "электричестве"
)

benz <- new("Benz",
            wheels = 4,
            driverseat = "cлева",
            passangerseats = 3,
            name = "обычная",
            bip = "трррррр",
            fuel = "бензине"
)

type <- function() {
  v1 <- readline("Информацию о какой машине вы хотите узнать (Disel/Electro/Benz)?: ")
  if (v1 == "Disel") printCar(disel)
  else if (v1 == "Electro") printCar(electro)
  else if (v1 == "Benz") printCar(benz)
  else {
    stop("Неверный ввод. Введите 'Disel', 'Benz' or 'Electro'.")
  }
}

type()
