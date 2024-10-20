################################################################################## Exercice 1

train = read.table("synth_train.txt", header = T)

Xtrain = train[,-1]

Ytrain = train$y

par(mfrow = c(1,1))

########################## Question 1 : Estimer ces paramètres sur les données d’apprentissage.

plot(Xtrain, pch = Ytrain, col = Ytrain)

legend("topright", legend = c("Classe 1","Classe 2"), pch = 1:2, col = 1:2)

pi = as.matrix(prop.table(table(Ytrain)))

pi1 = pi[1]

pi2 = pi[2]

bd1 = as.matrix(subset(train, y == 1, select = c(2:3)))

bd2 = as.matrix(subset(train, y == 2, select = c(2:3)))

mu1 = apply(bd1, 2, mean)

mu2 = apply(bd2, 2, mean)

n1 = nrow(bd1)

n2 = nrow(bd2)

y1 = sweep(bd1,2, mu1, "-")

y2 = sweep(bd2,2, mu2, "-")

sig1 = (1/(n1-1)) *(t(y1)%*%(y1)) # var(bd1)

sig2 = (1/(n2-1)) *(t(y2)%*%(y2)) # var(bd2)

#mu = cbind(tapply(Xtrain$x1, Ytrain, mean), tapply(Xtrain$x2, Ytrain, mean))

#mu1 = mu[1,]

#mu2 = mu[2,]

############################################################ Question 1 ( Autres alternatives)

# Calcul des paramètres pour la classe 1
class1 <- Xtrain[Ytrain == 1, ]
class2 <- Xtrain[Ytrain == 2, ]

# Estimation des moyennes
mu1 <- colMeans(class1)
mu2 <- colMeans(class2)

# Estimation des matrices de covariance
Sigma1 <- cov(class1)
Sigma2 <- cov(class2)

# Estimation des probabilités a priori
pi1 <- nrow(class1) / nrow(Xtrain)
pi2 <- nrow(class2) / nrow(Xtrain)


################################################ Question 2 : Fonction discrimante quadratique

Q1 = function(x){(-1/2) * log(det(sig1)) - (1/2) * (t(x-mu1)%*%solve(sig1)%*%(x-mu1)) + log(pi1)}

Q2 = function(x){(-1/2) * log(det(sig2)) - (1/2) * (t(x-mu2)%*%solve(sig2)%*%(x-mu2)) + log(pi2)}

x = c(-1,1)

Q1(x)

Q2(x)

####################### Question 3 : Estimation les probabilités à posteriori pour x = (−1, 1)

prob_post1 = exp(Q1(x))/(exp(Q1(x)) + exp(Q2(x)))

prob_post2 = exp(Q2(x))/(exp(Q1(x)) + exp(Q2(x)))

prob_post1

prob_post2

#################################### Question 4 : Utilisation des fonctions qda et predict.qda

library(MASS)

qda = qda(y ~., data = train)

qda = qda(x = train[,2:3], grouping = as.factor(train$y) )

qda$prior

qda$scaling

predqda = predict(qda, x)

predqda$class

predqda$posterior

################################################ Question 5 : représentation de la méthode qda

load("grille.rda")

predgr = predict(qda, grille)

plot(grille, pch = 20, col = predgr$class, cex = 0.5)

points(Xtrain, pch = Ytrain, col = Ytrain)

legend("topright", legend = c("Classe 1", "Classe 2"), col = 1:2, pch = 1:2)

################################################ Question 6 : représentation de la méthode qda

N = nrow(train)

sig = (1/N) * (n1*sig1 + n2*sig2) 

sig

## Question 7 : La règle de décision de Bayes (prédire la classe la plus probable à posteriori)

L1 = function(x){t(x)%*%solve(sig)%*%mu1 - (1/2)*(t(mu1)%*%solve(sig)%*%mu1) + log(pi1)}

L2 = function(x){t(x)%*%solve(sig)%*%mu2 - (1/2)*(t(mu2)%*%solve(sig)%*%mu2) + log(pi2)}

L1(x)

L2(x)

######################################################## Question 8 : Probabilité à postériori 

prob_post1_1 = exp(L1(x))/(exp(L1(x)) + exp(L2(x)))

prob_post2_2 = exp(L2(x))/(exp(L1(x)) + exp(L2(x)))

prob_post1_1

prob_post2_2

#################################### Question 9 : Utilisation des fonctions lda et predict.lda

library(MASS)

lda = lda(y ~., data = train)

lda = lda(x = train[,2:3], grouping = as.factor(train$y))

lda$prior

lda$scaling # ????

predlda = predict(lda, x)

predlda$class

predlda$posterior

################################################### Question 10 : Representation graphique lda

predgrl_lda = predict(lda, grille)

plot(grille, pch = 20, col = predgrl_lda$class, cex = 0.5)

points(Xtrain, pch = Ytrain, col = Ytrain)

legend("topright", legend = c("Classe 1", "Classe 2"), col = 1:2, pch = 1:2)

########################################################## Question 11: Comparaison qda et lda

test = read.table("synth_test.txt", header = T)

Xtest = test[, 2:3]

Ytest = test$y

predy_qda = predict(qda, Xtest)

predy_lda = predict(lda, Xtest)

table(predy_qda$class, Ytest)

table(predy_lda$class, Ytest)

erreur_qda = sum(predy_qda$class != Ytest)/length(Ytest)

erreur_lda = sum(predy_lda$class != Ytest)/length(Ytest)

erreur_lda

erreur_qda

################################################################################### Exercice 2


############################################ Question 1 : Importation de données 

Numtrain = read.table("numbers_train.txt", header = T) 

Xntrain = as.matrix(Numtrain[,-1])

Yntrain = Numtrain$y

############################################## Question 2 : Visualisation d'images

par(mfrow=c(3,3))

for (i in 1:9){
   image(matrix(Xntrain[i,],16,16), col=gray(1:100/100), ylim=c(1,0))
}

par(mfrow=c(1,1))

############################################# Question 3 : Prédiction avec lda

im_lda = lda(Numtrain[,-1], grouping = as.factor(Yntrain))

pred_im_lda = predict(im_lda,Numtrain[,-1])

table(pred_im_lda$class, Yntrain)

erreur_app = sum(pred_im_lda$class != Yntrain) * (1/length(Yntrain))

erreur_app

############################################## Question 4 : Importation des données tests :

Num_test = read.table("numbers_test.txt", header = T)

Xn_test = Num_test[,-1]

Yn_test = Num_test$y

########################################### Question 5 : Prédiction lda et calcul de l'erreur

pred_im_test_lda = predict(im_lda, Xn_test)

table(pred_im_test_lda$class, Yn_test)

erreur_im_test = sum(pred_im_test_lda$class != Yn_test)*(1/length(Yn_test))

########################################### Question 6 : Prédiction qda et calcul de l'erreur

# im_qda = qda(Numtrain[,-1], grouping = as.factor(Yntrain))

# pred_im_test_qda = predict(im_qda, Xn_test)

# table(pred_im_test_lda$class, Yn_test)

# erreur_im_test = sum(pred_im_test_lda$class != Yn_test)*(1/length(Yn_test))

# erreur_im_test

################################################################################ Question 10 :

## Validation croisée LOO (Leave One Out)

# Charger la bibliothèque pour la validation croisée
library(MASS)

# Nombre d'observations
n <- nrow(Xtrain)

# Initialiser le compteur d'erreurs
error_count_loo <- 0

# Validation croisée Leave-One-Out (LOO)
for (i in 1:n) {
  # Utiliser toutes les données sauf la i-ème pour l'entraînement
  lda_model_loo <- lda(Xtrain[-i,], Ytrain[-i])
  
  # Prédire la classe de la i-ème observation
  prediction_loo <- predict(lda_model_loo, Xtrain[i, , drop = FALSE])$class
  
  # Comparer la prédiction avec la vraie classe
  if (prediction_loo != Ytrain[i]) {
    error_count_loo <- error_count_loo + 1
  }
}

# Calcul du taux d'erreur LOO
error_rate_loo <- error_count_loo / n

# Afficher le résultat
print(paste("Taux d'erreur LOO :", round(error_rate_loo * 100, 2), "%"))


#### validation croisée en 5 folds

# Nombre de folds
k <- 5

# Créer les indices pour les 5 folds
set.seed(123)  # Pour reproduire les résultats
folds <- sample(rep(1:k, length.out = length(Ytrain)))

# Initialiser le compteur d'erreurs
error_count_kfold <- 0

# Validation croisée 5-folds
for (i in 1:k) {
  # Séparer les données en ensemble d'apprentissage et de test
  train_idx <- which(folds != i)
  test_idx <- which(folds == i)
  
  # Ajuster le modèle LDA sur les données d'apprentissage
  lda_model_kfold <- lda(Xtrain[train_idx, ], Ytrain[train_idx])
  
  # Prédire les classes pour l'ensemble de test
  predictions_kfold <- predict(lda_model_kfold, Xtrain[test_idx, ])$class
  
  # Compter les erreurs de prédiction
  error_count_kfold <- error_count_kfold + sum(predictions_kfold != Ytrain[test_idx])
}

# Calcul du taux d'erreur 5-folds
error_rate_kfold <- error_count_kfold / n

# Afficher le résultat
print(paste("Taux d'erreur 5-folds :", round(error_rate_kfold * 100, 2), "%"))

################################################################################### Exercice 4

load("Desbois_complet.rda")

colnames(data)

## Question 1

library(MASS)

# Indices des 75% de données d'apprentissage

set.seed(10)

tr = sample(1:nrow(data),945)

tr

# Métode lda avec la syntaxe utilisant les formules

g = lda(DIFF~., data=data[tr,])

pred = predict(g, data[-tr,-1])$class

sum(pred != data[-tr,1])/length(pred)

# Taux d'erreur test (pour ce découpage) de 15.2%


################################################################################ Question 2

library(klaR)

?greedy.wilks

formula = greedy.wilks(DIFF~., data=data[tr,], niveau = 0.2)$formula

formula

g = lda(formula, data=data[tr,])

pred = predict(g, data[-tr,-1])$class

sum(pred != data[-tr,1])/length(pred)

# Taux d'erreur test (avec ces variables et pour ce découpage) de 14.6%


# Test 1 greedy wilks

base = c() # matrix vide

lda = c()

lda_wilks = c()

for (i in 1:100){

tr = sample(1:nrow(data),945)

g_lda = lda(DIFF~., data=data[tr,])

pred = predict(g_lda, data[-tr,-1])$class

err_lda = sum(pred != data[-tr,1])/length(pred)

wilks = greedy.wilks(DIFF~., data=data[tr,], niveau = 0.2)

g_lda_wilks = lda(wilks$formula, data=data[tr,])

pred_wilks = predict(g_lda_wilks, data[-tr,-1])$class

err_lda_wilks = sum(pred_wilks != data[-tr,1])/length(pred)

base = rbind(base, wilks$results[1])

lda = rbind(lda, err_lda)

lda_wilks = rbind(lda_wilks, err_lda_wilks)

}


ind = order(table(base), decreasing = T)

barplot(table(base)[ind])

