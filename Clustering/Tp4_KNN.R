################################################################################### Exercice 1

######################################## Question 1 : Chargement des données

library(ggplot2)

train = read.table("synth_train.txt", header=TRUE)

Xtrain = train[,-1] # matrice des entrées

Ytrain = train$y # vecteur des sorties (les classes des entrées)


######################################## Question 2 : Représentation graphique

plot(Xtrain, pch = Ytrain, col = Ytrain) # données + distribution suivant Ytrain

legend("topright", legend=c("classe1", "classe2"), pch=1:2, col=1:2)

# Création du graphique (alternative)

ggplot(train, aes(x = x1, y = x2, color = y)) +
  geom_point() +
  labs(title = "Données train", x = "X1", y = "X2") +
  theme_minimal()

library(class)

table(Ytrain)


###################################################### Question 3 : Prédictions (Xtest)

# les deux points à prédire sont mis dans une matrice "Xtest"

Xtest = matrix(c(0,0,-2,1), nrow=2, byrow=TRUE) # (0,0) et (-2,1)

# tous les calculs de la fonction knn sont stockés dans "pred"

pred = knn(Xtrain, Xtest, Ytrain, k=30, prob=TRUE)

# Contenu de l'objet "pred"
pred

attr(pred, "prob") # permet de récupérer les probabilités à posteriori


######################################## Question 4 : Prédictions (Xtrain)

pred_train <- knn(Xtrain, Xtrain, Ytrain, 30)

head(pred_train) # les 5 premiers points sont affectés à la classe 2

table(pred_train,Ytrain) # 10+0 données d'apprentissage sont mal prédites

sum(pred_train!=Ytrain)/length(Ytrain) # taux d'erreur d'apprentissage


########################################## Question 5 : Chargement données (test)

test = read.table(file="synth_test.txt", header=TRUE)

Xtest = test[,-1] # matrice des entrées

Ytest = test$y # vecteur des sorties (les classes des entrées)


########################################## Question 6 : Prédictions (Xtest)

pred_test = knn(Xtrain, Xtest, Ytrain, 30)

head(pred_test) # les 5 premiers points sont affectés à la classe 2

table(pred_test,Ytest) # 10+0 données d'apprentissage sont mal prédites

sum(pred_test!=Ytest)/length(Ytest) # taux d'erreur d'apprentissage


################################### Question 7 : Frontière de décision (k = 1, k = 15, k = 30)

load("grille.rda")

# avec k=30 voisins

pred_grille_30 = knn(Xtrain, grille, Ytrain, 30)

plot(grille, pch = 20, col = pred_grille_30, cex = 0.5,
     main="Frontière de décision pour k=30 voisins")

points(Xtrain, pch=Ytrain, col=Ytrain)

legend("topright", legend=c("classe 1", "classe 2"), pch=1:2, col=1:2, bg="white")


par(mfrow= c(1,2))


# avec k=15 voisins

pred_grille_15 = knn(Xtrain, grille, Ytrain, 15)

plot(grille, pch = 20, col = pred_grille_15, cex = 0.5,
     main="Frontière de décision pour k=15 voisins")

points(Xtrain, pch=Ytrain, col=Ytrain)

legend("topleft", legend=c("classe 1", "classe 2"), pch=1:2, col=1:2, bg="white")


# avec k=1 voisin

pred_grille_1 = knn(Xtrain, grille, Ytrain, 1)

plot(grille, pch = 20, col = pred_grille_1, cex = 0.5,
     main="Frontière de décision pour k=1 voisin")

points(Xtrain, pch=Ytrain, col=Ytrain)

legend("topleft", legend=c("classe 1", "classe 2"), pch=1:2, col=1:2, bg="white")



############ Exercice 2 : Appliquer les k plus proches voisins à la reconnaissance automatique 

# de caractères manuscrits.


######################################## Question 1 : Chargement des données

data = read.table("numbers_train.txt", header = T)

XnTrain = as.matrix(data[,-1])

YnTrain = as.factor(data$y)


######################################### Question 2 : Visualiser les neufs premières images

par(mfrow=c(3,3))

for (i in 1:9){
  
  image(matrix(XnTrain[i,],16,16), col=gray(1:100/100), ylim=c(1,0))
  
}


################### Question 3 : Prédiction méthodes knn avec données train sur les 500 images

pred_Xntrain = knn(XnTrain, XnTrain, YnTrain, k = 1, prob = T)


table(pred_Xntrain, YnTrain)


sum(pred_Xntrain!=YnTrain)/length(YnTrain)


########################################### Question 4 : Importer Xtest

datatest = read.table("numbers_test.txt", header = T)

Xntest = datatest[, -1]

Yntest = datatest$y

################## Question 5 : Prédiction méthodes knn avec données test sur les 500 images

pred_Xntest = knn(XnTrain, Xntest, YnTrain, k = 1, prob = T)

table(pred_Xntest, Yntest)

sum(pred_Xntest != Yntest)/length(Yntest)

table(Yntest)

##################################### Question 6 : Variation de k

prob = as.numeric(c(1:100))

for (i in 1:100){
  
  pred_Xntest = knn(XnTrain, Xntest, YnTrain, k = i, prob = T)
  
  prob[i] = sum(pred_Xntest != Yntest)/length(Yntest)
  
  }

plot(prob, ylim = c(0,1), xlab = "K voisins" , ylab = "Probabilité" ,  type = "l")


########################################## Question 7

X = rbind(XnTrain, Xntest)

X_rand = X[sample(1000),]

Xnew_train = X_rand[1:500,]
  
Xnew_test =X_rand[500:1000,]

