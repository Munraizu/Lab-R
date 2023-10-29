#  Neural networks - regression
#  neuralnet
#  Кварталы Бостона

library(MASS)
data.1 <- Boston

install.packages("dplyr")

# подключение пакетов

library(dplyr)
#  Стандартизация

#  Результаты получаем в виде data.frame

maxs <- apply(data.1, 2, max) 
mins <- apply(data.1, 2, min)

data.scaled <- as.data.frame(scale(data.1, center = mins, scale = maxs - mins))

## тоже самое но через пайплайн %>%

data.scaled <- data.1 %>% scale(center = mins, scale = maxs - mins) %>% as.data.frame()

#  Делим данные на обучающую и тестовую выборки

set.seed(500)
index <- sample(1:nrow(data.1),round(0.75*nrow(data.1)))

train_ <- data.scaled[index,]
test_  <- data.scaled[-index,]

#  Используем эмпирическое правило: 
#  число нейронов в скрытом слое равно 2/3 от числа нейронов во входном слое
#  2 скрытых слоя. Архитектура: 13:5:3:1. 


library(neuralnet)

#  в процедуре neuralnet()  запрещена формула y~. 
#  Поэтому заранее составляем формулу, 
#  затем подставляем ее как аргумент в процедуру neuralnet().
## Оператор %in% в R используется для проверки наличия значений первого аргумента во втором аргументе 
## и возвращает логический вектор, указывающий, есть ли совпадение или нет для его левого операнда. 
## Здесь первый и второй аргументы могут быть значением, вектором, списком или последовательностью.
## %in% — инфиксный оператор.
## %in% возвращает логический вектор (ИСТИНА или ЛОЖЬ, но никогда NA).
## Выходной логический вектор имеет ту же длину, что и левый операнд.
## %in% работает с векторами, списками, последовательностями и DataFrame.
#  Составляем формулу

n <- names(train_)
n %in% "medv"
# [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
n[!n %in% "medv"]
# [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"     "dis"     "rad"     "tax"     "ptratio"
# [12] "black"   "lstat"  
> 
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))

f<-n[!n %in% "medv"] %>% paste("medv ~") %>% paste(collapse = " + ") %>% as.formula()
?paste
#  hidden=c(5,3) - вектор с числом нейронов во внутренних слоях 
#  linear.output=TRUE  -  когда решаем задачу регрессии. В задаче регресии
#  не нужно применять функцию активации. 
#  linear.output=FALSE -  когда решаем задачу классификации

nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
?neuralnet

#  графическое представление нейронной сети

plot(nn)


#  Предсказываем medv

pr.nn <- compute(nn,test_[,1:13])

#  когда прогнозировали логарифм ряда, потом считали exp(прогноз)
#  так же и тут
#  проводим действие, обратное к стандартизации


pr.nn_ <- pr.nn$net.result*(max(data.1$medv)-min(data.1$medv))+min(data.1$medv)
test.r <- (test_$medv)*(max(data.1$medv)-min(data.1$medv))+min(data.1$medv)




MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

#  Что лучше - нейронная сеть или линейная регрессия
summary(lm(f, data=data.1))


