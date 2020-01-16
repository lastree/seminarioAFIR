#install.packages('xgboost')
#install.packages('caret')
#install.packages('e1071')
#install.packages('pROC')
#install.packages('Ckmeans.1d.dp')
#install.packages('DiagrammeR')

library(xgboost)
library(caret)
library(e1071)
library(pROC)
library(Ckmeans.1d.dp)
library(ggplot2)
library(DiagrammeR)

# Lectura del fichero .csv obtenido
data <- read.csv('dataPrepared.csv', sep = ',', header = TRUE)
head(data)

# Primera partición: Obtención del conjunto de test
n <- nrow(data)
set.seed(0)
train_index <- sample(n, size = ?????)

entrenamiento <- ?????  # Conjunto aún a dividir entre entrenamiento y validación
df_test <- ??????

# Segunda partición: Obtención de los conjuntos de entrenamiento y validación
# IMPORTANTE!!: Balanceo de datasets. ¿Por qué?
cat('No defectos en el conjunto:', nrow(entrenamiento[entrenamiento$defecto == 0, ]), '\n')
cat('Defectos en conjunto:', nrow(entrenamiento[entrenamiento$defecto == 1, ]))

# Balanceo 

# Balanceo de conjuntos.
df_majority <- entrenamiento[entrenamiento$defecto == 0, ]
df_minority <- entrenamiento[entrenamiento$defecto == 1, ]

set.seed(0)
under_index <- sample(n_over, size = ?????)

df_majority_undersampled <- ?????

# Concatenamos de nuevo los conjuntos balanceados.
entrenamiento_balanceado <- ?????

# Segunda partición

n2 <- nrow(entrenamiento_balanceado)
set.seed(0)
train_index2 <- sample(x = n2, size = ????)

df_train <- ????
df_valid <- ????

# Tamaño final de los conjuntos.
cat('Tamaño del conjunto de entrenamiento:', nrow(df_train), '\n')
cat('Tamaño del conjunto de validación:', nrow(df_valid), '\n')
cat('Tamaño del conjunto de test:', nrow(df_test))

# Separamos de los conjuntos la variable objeto de estudio
# Convertir en matrices, ya que la función para entrenar el modelo XGB no permite utilizar dataframes
X_train <- as.matrix(df_train[, which(names(df_train) != 'defecto')])
y_train <- df_train[, 'defecto']

X_valid <- as.matrix(df_valid[, which(names(df_valid) != 'defecto')])
y_valid <- df_valid[, 'defecto']

X_test <- as.matrix(df_test[, which(names(df_test) != 'defecto')])
y_test <- df_test[, 'defecto']

params <- list(objetive = 'binary:logistic',
               nthread = 4,
               max_depth = 6,
               eta = 0.3,
               eval_metric = 'auc')

xgb_model <- xgboost(data = ????, label = ????, params = ?????, nrounds = 30, verbose = 1, seed = 0)

# Importancia de cada variable en el modelo
feature_importance <- xgb.importance(feature_names = ????, model = ????)
feature_importance

# Gráfico con la importancia
options(repr.plot.width = 6, repr.plot.height = 3.5)
xgb.ggplot.importance(importance_matrix = feature_importance, rel_to_first = TRUE)

# Ajuste/Rendimiento sobre datos de entrenamiento.
pred_train <- predict(xgb_model, X_train)

# Rendimiento sobre datos de validación
pred_valid <- predict(xgb_model, X_valid)

# Creamos un dataframe con la probabilidad de ser o no defectuosa la pieza
# y añadimos el cálculo de la predicción final

# Entrenamiento
predictions_train <- data.frame('probability' = pred_train, 
                                'prediction' = ifelse(pred_train > 0.5, 1, 0))

# Validación
predictions_valid <- data.frame('probability' = pred_valid, 
                                'prediction' = ifelse(pred_valid > 0.5, 1, 0))

# Cálculo de AUCs

# Entrenamiento
roc_train <- roc(y_train, predictions_train$prediction)
auc_train <- round(auc(roc_train), 4)

# Validación
roc_valid <- roc(y_valid, predictions_valid$prediction)
auc_valid <- round(auc(roc_valid), 4)

cat(paste('Área bajo la curva (AUC) en entrenamiento:', auc_train, '\n'))
cat(paste('Área bajo la curva (AUC) en validación:', auc_valid))

# Predicciones en conjunto de test
pred_test <- predict(xgb_model, X_test)
pred_test <- ifelse(pred_test > 0.5, 1, 0)

# Área bajo la curva
roc_test <- roc(y_test, pred_test)
auc_test <- round(auc(roc_test), 4)

cat('Área bajo la curva (AUC) en test:', auc_test)

# Matriz de confusión
cm_test <- round(prop.table(table(pred_test, y_test), 2)*100, 2)

cat('Matriz de confusión para datos de test:\n\n')
print(cm_test)

xgb.plot.tree(feature_names = xgb_model$feature_names, model = xgb_model, trees = 0)

# Guardado del modelo ya entrenado
xgb.save(xgb_model, 'modelo_entrenado')
