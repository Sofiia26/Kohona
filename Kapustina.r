Produc <- read.table("Produc.csv", header = T, sep = "," )
P <- Produc[ , -c(1,2)]
state <- Produc$state

install.packages("kohonen")
library('kohonen')
str(P)
head(P)
View(P)

set.seed(1)

som.P <- som(scale(P), grid = somgrid(3, 3, 'hexagonal'))
som.P
dim(getCodes(som.P))

plot(som.P, main = 'Produc data Kohonen SOM')

graphics.off()
par(mfrow = c(1,1))
plot(som.P, type = 'changes', main = 'Produc data SOM')

train <- sample(nrow(P), 700)
X_train <- scale(P[train,])
X_test <- scale(P[-train,],
                center = attr(X_train, "scaled:center"),
                scale = attr(X_train, "scaled:center"))
train_data <- list(measurements = X_train,
                   state = state[train])
test_data <- list(measurements = X_test,
                  state = state[-train])

mygrid <- somgrid(3, 3, 'hexagonal')

som.P <- supersom(train_data, grid = mygrid)                

som.predict <- predict(som.P, newdata = test_data)
table(state[-train], som.predict$predictions[['state']])
map(som.P)

