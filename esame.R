heart <- read.csv("heart.csv", header = TRUE, stringsAsFactors = FALSE)
str(heart)
summary(heart)
View(heart)
# rinomino colonne
names(heart)[names(heart) == "trestbps"] <- "restbps"
names(heart)[names(heart) == "thalach"] <- "maxhrh"
# rimuovo colonne inutili
heart <- subset(heart, select = -x)
as_tibble(heart)
options(max.print=999999)
heart %>% distinct()
# ricontrollo
str(heart)
summary(heart)
# trasormo sex in factor e tolgo i valori unspecified e rinomino i livelli
heart$sex[heart$sex == "unspecified"] <- NA
heart$sex <- as.factor(heart$sex)
levels(heart$sex)
# levels(heart$sex) <- c("female", "male")
str(heart$sex)
summary(heart$sex)
# tolgo gli undefined da chol e lo traformo in int
heart$chol[heart$chol == "undefined"] <- NA
heart$chol <- as.integer(heart$chol)
# trasformo in factor
heart$cp <- as.factor(heart$cp)
heart$fbs <- as.factor(heart$fbs)
heart$restecg <- as.factor(heart$restecg)
heart$exang <- as.factor(heart$exang)
heart$slope <- as.factor(heart$slope)
heart$ca <- as.factor(heart$ca)
heart$thal <- as.factor(heart$thal)
heart$target <- as.factor(heart$target)
str(heart)
summary(heart)
levels(heart$thal)
# metto NA gli 0 in thal
heart$thal[heart$thal == 0] <- NA
heart$thal <- droplevels(heart$thal)
levels(heart$thal)
# gli attr. di rapporto sono: restbps, chol
# gli attr. ordinali sono: age, maxhrh
# gli attr. nominali sono: sex, cp, fbs, restecg, exang, slop, thal, ca, target
# tolgo gli na
str(heart)
summary(heart)
heart <- na.omit(heart)
summary(heart)
# age ha valori < 0 quindi li trasmormo in na poi in media
heart$age <- replace(heart$age, heart$age < 0, NA)
heart$age[is.na(heart$age)] <- median(heart$age, na.rm = TRUE)
summary(heart$age)
# in heart$ca ci sono valori sbagliati e li tolgo
heart$ca <- replace(heart$ca, heart$ca == 4, NA)
heart <- na.omit(heart)
# rimuovo gli outlier di restbps
boxplot(heart$restbps)
q3 <- quantile(heart$restbps, .75)
q1 <- quantile(heart$restbps, .25)
iqr <- (q3 - q1)
min <- (q1 - (1.5 * iqr))
max <- (q3 + (1.5 * iqr))
sum(heart$restbps < max | heart$restbps > min)
heart <- heart[heart$restbps > min & heart$restbps < max, ]
boxplot(heart$restbps)
# tolgo gli outlier di chol
boxplot(heart$chol)
q3 <- quantile(heart$chol, .75)
q1 <- quantile(heart$chol, .25)
iqr <- (q3 - q1)
min <- (q1 - (1.5 * iqr))
max <- (q3 + (1.5 * iqr))
sum(heart$chol < max | heart$chol > min)
heart <- heart[heart$chol > min & heart$chol < max, ]
boxplot(heart$chol)
# tolgo gli outlier di maxhrh
boxplot(heart$maxhrh)
q3 <- quantile(heart$maxhrh, .75)
q1 <- quantile(heart$maxhrh, .25)
iqr <- (q3 - q1)
min <- (q1 - (1.5 * iqr))
max <- (q3 + (1.5 * iqr))
sum(heart$maxhrh < max | heart$maxhrh > min)
heart <- heart[heart$maxhrh > min & heart$maxhrh < max, ]
boxplot(heart$maxhrh)
# tolgo gli outlier di oldpeak
boxplot(heart$oldpeak)
q3 <- quantile(heart$oldpeak, .75)
q1 <- quantile(heart$oldpeak, .25)
iqr <- (q3 - q1)
min <- (q1 - (1.5 * iqr))
max <- (q3 + (1.5 * iqr))
sum(heart$oldpeak < max | heart$oldpeak > min)
heart <- heart[heart$oldpeak > min & heart$oldpeak < max, ]
boxplot(heart$oldpeak)
# per tutti gli attributi nominali faccio il barplot, ho creato una funzione per fare meno righe
barplotfunc <- function(dato, titolo, x) {
  ggbar <- ggplot(heart, aes(x = dato, fill = dato, )) +
    scale_fill_discrete(name = "legenda", labels = x) +
    geom_bar() +
    labs(
      title = titolo,
      x = "", fill = "legenda",
      y = "frequenza"
    )
  plot(ggbar)
}
barplotfunc(heart$sex, "sex frequenza", c("female", "male"))
barplotfunc(heart$restecg, "Resting electrocardiographic measurement", c("normal", "abnormality", "ventricular hypertrophy"))
barplotfunc(heart$cp, "chest pain", c("typical angina", "atypical angina", "non-anginal pain", "asymptomatic"))
barplotfunc(heart$fbs, "fasting blood sugar", c("> 120mg/dl", "< 120mg/dl"))
barplotfunc(heart$exang, "Exercise induced angina", c("yes", "no"))
barplotfunc(heart$slope, "slope", c("upsloping", "flat", "downsloping"))
barplotfunc(heart$ca, "The number of major vessels", c("0", "1", "2", "3"))
barplotfunc(heart$target, "heart disease", c("yes", "no"))
barplotfunc(heart$thal, "thalassemia", c("normal", "fixed defect", "reversable defect"))
# controllo
summary(heart)
str(heart)
# faccio la regressione lineare tra age e maxhrh
plot(heart$maxhrh, heart$age, xlab = "maximum heart rate achieved", ylab = "age")
reg <- lm(age ~ maxhrh, data = heart)
abline(reg, col = "red")
segments(heart$maxhrh, fitted(reg), heart$maxhrh, heart$age, col = "blue", lty = 2)
title(main = "Regr.lin age and max heart rate ach")
# con confint e summury controllo l'intercetta, r, r^2
reg_2 <- lm(heart$age ~ I(heart$maxhrh - mean(heart$maxhrh)))
summary(reg_2)
confint(reg)
summary(reg)
r <- cov(heart$age, heart$maxhrh) / (sd(heart$age) * sd(heart$maxhrh))
r^2
# adesso calcoli gli scarti
plot(reg$fitted, reg$residuals, main = "Residui")
abline(0, 0)
# distribuzione in quantili
qqnorm(reg$residuals)
qqline(reg$residuals)
# creo un df con 10 osservazione e provo a fare previsioni
reg1 <- lm(heart$age ~ heart$maxhrh)
pred <- data.frame("maxhrh" = c(90, 137, 200, 210, 150, 97, 267, 145, 100, 165))
predict(reg, pred, interval = "confidence")
# implemento algoritmi di machine learning
dataset <- heart[c(-1:-3,-6,-7,-9:-13)]
dim(dataset)
sapply(dataset, class)
head(dataset)
levels(dataset$target)
percentage <- prop.table(table(dataset$target)) * 100
cbind(freq = table(dataset$target), percentage = percentage)
summary(dataset)
x <- dataset[, 1:3]
dim(x)
y <- dataset[, 4]
dim(t(y))
# creo i diversi plot
par(mfrow = c(1, 3))
for (i in 1:3) {
  boxplot(x[, i], main = names(dataset)[i])
}
plot(y)
featurePlot(x = x, y = y, plot = "ellipse", auto.key = list(columns = 2))
featurePlot(x = x, y = y, plot = "box")
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
featurePlot(x = x, y = y, plot = "density", scales = scales, auto.key = list(columns = 2))
# setto il seed e creo il training con l'80% dei dati del dataset e 20% per il test
set.seed(1500)
training_index <- createDataPartition(dataset$target, p = .80, list = FALSE)
training_set <- dataset[training_index, ]
nrow(training_set)
test_set <- dataset[-training_index, ]
nrow(test_set)
summary(test_set)
seed = set.seed(1500)
control <- trainControl(method = "cv", number = 10, seed = seed)
metric <- "Accuracy"

# linear algorithms
fit_lda <- train(target ~ ., data = training_set, metric = metric, trControl = control, method = "lda")
## CART
fit_cart <- train(target ~ ., data = training_set, metric = metric, trControl = control, method = "rpart")
## kNN
fit_knn <- train(target ~ ., data = training_set, metric = metric, trControl = control, method = "knn")
## MLP
fit_mlp <- train(target ~ ., data = training_set, metric = metric, trControl = control, method = "mlp")
## Random Forest
fit_rf <- train(target ~ ., data = training_set, metric = metric, trControl = control, method = "rf")
## SVM
fit_svm <- train(target ~ ., data = training_set, metric = metric, trControl = control, method = "svmRadial")

results <- resamples(list(lda = fit_lda, cart = fit_cart, knn = fit_knn, mlp = fit_mlp, rf = fit_rf, svm = fit_svm))
summary(results)
dotplot(results)
# mi sembra che il metodo lda sia il più accurato e quindi lo uso
fit_lda$results
summary(dataset)
predictions <- predict(fit_lda, test_set)
confusionMatrix(predictions, test_set$target)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit_cart$finalModel)