# Creation de l'objet Epreuve_A : un vecteur numerique contenant les notes de l'épreuve A

Epreuve_A <- c(3,4,6,7,9,10,9,11,12,13,15,4)

# Creation de l'objet Epreuve_B : un vecteur numerique contenant les notes de l'épreuve B

Epreuve_B <- c(8,9,10,13,15,14,13,16,13,19,6,19)

# statistiques descriptives de Epreuve_A

summary(Epreuve_A)

# statistiques descriptives de Epreuve_B

summary(Epreuve_B)

# Représentation minimale du nuage de point avec la fonction "plot"

plot(x = Epreuve_A, # abscisses
     y = Epreuve_B, # ordonnées
     main = "Nuage de points", # titre
     xlab = "Note de l'épreuve A (sur 20)", # Label de l'axe x
     ylab = "Note de l'épreuve B (sur 20)", # Label de l'axe y
     col = "blue", # Couleur
     type = "p", # "p" : points ; "l": ligne
     pch = 4) # Style de points

# pour obtenir l'aide de la function

?lm()

# estimation du modèle

model_epreuve <- lm(Epreuve_B~Epreuve_A)

# résumer de la sortie

summary(model_epreuve)

# Représentation minimale du nuage de point avec la fonction "plot"

plot(x = Epreuve_A, # abscisses
     y = Epreuve_B, # ordonnées
     main = "Nuage de points", # titre
     xlab = "Note de l'épreuve A (sur 20)", # Label de l'axe x
     ylab = "Note de l'épreuve B (sur 20)", # Label de l'axe y
     col = "blue", # Couleur
     type = "p", # "p" : points ; "l": ligne
     pch = 4) # Style de points

abline(a = model_epreuve$coefficients[1],
       b = model_epreuve$coefficients[2],
       col = "blue")

# équivalent à

abline(model_epreuve, col = "blue")

# les deux dernières observations

y <- Epreuve_B[-c(11,12)]

x <- Epreuve_A[-c(11,12)]

# estimation du modele

model2 <- lm(y~x)

# Représentation minimale du nuage de point avec la fonction "plot"

plot(x = Epreuve_A, # abscisses
     y = Epreuve_B, # ordonnées
     main = "Nuage de points", # titre
     xlab = "Note de l'épreuve A (sur 20)", # Label de l'axe x
     ylab = "Note de l'épreuve B (sur 20)", # Label de l'axe y
     col = "blue", # Couleur
     type = "p", # "p" : points ; "l": ligne
     pch = 4) # Style de points
points(x = x,
       y = y,
       pch = 4,
       col = "red")##### pch format dessin

abline(a = 11.99028, b = 0.1079284, col = "blue")

abline(a = model2$coefficients[1], b = model2$coefficients[2], col = "red") # sans les points aberrants

# data

x <- Epreuve_A

y <- Epreuve_B

# alternatives à la fonction lm()

X <- cbind(1, x)

beta <- as.vector( solve(t(X)%*%X) %*% t(X) %*% y )

beta

########### Test

x0 = rnorm(30)
x1 = rnorm(30)
x2 = rnorm(30)
X3 = rnorm(30)
y0 = rnorm(30)

Model_al = lm(y0 ~ x0 + x1 + x2 + X3)

summary(Model_al)

################################################# Séance 2

# nombre d'observations

n <- 100

#Spécification des paramètres (inconnus)

constante <- 0

pente <- 3000

sigma <- 20000

# Simulation de la variable dépendante : superficie des logements

superficie <- runif(n = n, min = 18, max = 95)

# simulation des erreurs de mesures (inconnues)

erreur <- rnorm(n = n, mean = 0, sd = sigma)

# Création de la variable réponse

prix <- constante + pente*superficie + erreur

# Représentation graphique

plot(prix~superficie)

abline(a = constante, b = pente)

eucalypt <- read.table("eucalyptus.txt", header=T, sep=";")

plot(ht ~ circ, # hauteur en fonction de la circonférence
     data = eucalypt,
xlab = "Circonférence", ylab = "Hauteur")

model1 <- lm(ht~circ, data = eucalypt)

summary(model1)

names(model1)

hist(model1$residuals)

?hist

#### utilisation de la fonction predict

?predict

?predict.lm

pred_fct <- predict(model1, interval = "prediction", level = 0.95)

head(pred_fct)

class(pred_fct)

tab_pred <- as.data.frame(pred_fct)

########## TP3

# getwd() #### V?rifier chemin d'acc?s

# setwd() #### Changer chemin d'acc?s

chenilles <- read.table("chenilles.txt", header=T, sep=" ")

chenilless = chenilles[,c(1:11)]

attach(chenilles)

names(chenilles)

plot(chenilles)

cor(chenilless)

model0 = lm(NbNids ~ Altitude + Pente + NbPins + Hauteur +  Diametre + Densite + Orient + HautMax + NbStrat + Melange)

summary(model0)

par(mfrow = c(2,2))

plot(model0)### Le modèle n'est pas linéaire et il y a des outliers

cooks.distance(model0)

model1 = lm(LogNids ~ Altitude + Pente + NbPins + Hauteur +  Diametre + Densite + Orient + HautMax + NbStrat + Melange)

summary(model1)

plot(model1)

cooks.distance(model1)

par(mfrow = c(1,2))

plot(cooks.distance(model1))

plot(cooks.distance(model0))

model2 = lm(LogNids ~ Altitude + Pente + NbPins + Hauteur +  Diametre  + NbStrat)

summary(model2)

model3 = lm(LogNids ~ Altitude + Pente + Hauteur +  Diametre)

summary(model3)

par(mfrow = c(2,2))

plot(model2)

AIC(model2)

AIC(model3)

BIC(model2)

BIC(model3)
