library(FactoMineR)

library(factoextra)

library(gridExtra)

library(RColorBrewer)

library(ggplot2)

library(maps)

####################################################################################### TP 1

# Version 1
column1 = c(5, 4, 1, 0)
column2 = c(4, 5, -2, -3)
X1 = cbind(column1, column2)
colnames(X1) = c('X1', 'X2')
rownames(X1) = LETTERS[1:4]

# Version 2
all = c(5, 4, 1, 0, 4, 5, -2, -3)
X2 = matrix(all, nrow = 4)
colnames(X2) = c('X1', 'X2')
rownames(X2) = LETTERS[1:4]

# Question 2

plot(X2[, 'X1'], X2[, 'X2'], type = "p", xlim = c(-4, 6), ylim = c(-4, 6),
     xlab = "X1", ylab = "X2")


# Question 3
d = dist(X2, method = "euclidean", diag = T, upper = T)
# as.matrix(d) nous donnes d sous forme de matrix
# as.dist(d) nous donnes d sous forme d'un objet dist

tree = hclust(d, method = "complete")

tree$height

# dendogramme
# hag = -1 pour mettre la hauteur des singletons à -1
plot(tree, hang = -1, xlab = "", sub = "", main = "Lien maximum")


# Méthode de Ward
# Pour faire du vrai Ward
treeWard = hclust(d^2/2, method = "ward.D")
# Plot
plot(treeWard, hang = -1, xlab = "", sub = "", main = "Ward")

# Hauteur
treeWard$height

# Partition en 2 classe
y = cutree(treeWard, k = 2)
y

# Inertie expliquée B(between) / T(total)
Total = sum(treeWard$height) # ou calculer la somme des variances

# Between (Somme des K-1 plus grandes hauteurs avec K le nombre de classe)
# Ici K = 2 => K = 1 => B = + grande hauteur
# B = grandes hauteurs / nombre de classes 67/n , 64/4 = Variance Total avec
# (wi = 1/n)

# Dans le cas général :
K = 2
h = sort(treeWard$height, decreasing = T)
B = sum(h[1:K-1])

(B/Total)*100 # = 97,0149

# 97% de l'inertie (la variance) des données expliquée par la partition
# A nombre de classe identique le meilleur algorithme est celui qui donne
# le meilleur poucentage d'inertie expliquée.

################################################################################### Exercice 3

# Question 1

fromage = read.table("fromage.txt", header = T, sep = "\t",
                     row.names = 1, dec = ".")

# Question 2

round(apply(fromage, 2, sd), digit=2)

# Question 3

M = apply(fromage, 2, mean)

M

Y = sweep(fromage,2,M,"-")

Y

S = apply(fromage, 2, sd) * sqrt(28/29)

S

Z = scale(fromage, center = T, scale = T) * sqrt(29/28)

Z = sweep(Y, 2, S, "/")

Z

apply(Z,2,mean)

apply(Z,2,sd) * sqrt(28/29)

# Question 4

d2 = dist(Z, method = "euclidean", diag = T, upper = T)

treeWard2 = hclust(d2^2/58, method = "ward.D")

treeWard2$height

plot(treeWard2, hang = -1, xlab = "", sub = "", main = "Ward")

# Inertie totale

T2 = sum(treeWard2$height)

# Beetween

h2 = sort(treeWard2$height, decreasing = T)

B2 = sum(h2[1:4])

B2

barplot(h2, main = "Hauteurs de l'arbre")

# Partitionnement

part = cutree(treeWard2,k=5)

part = factor(part,levels = c(1, 2, 3, 4, 5),
              labels = c("Groupe1", "Groupe2", "Groupe3", "Groupe4", "Groupe5"))
part


# ACP

acp_Z = PCA(Z, graph = F)

round(acp_Z$eig, digit=3)

couleurs = c("black","red", "green", "blue", "violet")

proj_ind = fviz_pca_ind(acp_Z,axes=c(1,2), repel = T, col.ind = part, palette = couleurs)

proj_ind

cercle1_2=fviz_pca_var(acp_Z,axes=c(1,2),col.var = "contrib", 
                       gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)
cercle1_2

fromage = cbind(fromage, part)

cat = catdes(fromage,num.var = 10)

round(cat$quanti$Groupe5, digits = 1)

v_test=-3

p_value=2*(1-pnorm(abs(v_test)))

p_value

#round(cat$quanti$Groupe5, digit=1)

################################################################################# Exercice 4

summary(USArrests)

# Question 1

nrow(USArrests)

Sd = apply(USArrests, 2, sd) * sqrt(49/50)

Sd

# Les unités divergent et donc les variables avec des variances plus fortes porterons les axes.

# Question 2

## données brutes

dward_usa = dist(USArrests[,-3], method = "euclidean", diag = T, upper = T)

treeWard_usa = hclust(dward_usa^2/100, method = "ward.D")

plot(treeWard_usa, hang = -1, xlab = "", sub = "", main = "Ward sur données brutes")

sum(treeWard_usa$height)

rect.hclust(treeWard_usa, k=5)

## données standardisés

USArr_Std = scale(USArrests[,-3], center = T, scale = T) * sqrt(50/49)

dward_usa2 = dist(USArr_Std, method = "euclidean", diag = T, upper = T)

treeWard_usa2 = hclust(dward_usa2^2/100, method = "ward.D")

plot(treeWard_usa2, hang = -1, xlab = "", sub = "", main = "Ward sur données standardisées")

sum(treeWard_usa2$height)

rect.hclust(treeWard_usa2, k=5)

# Question 3

## données brutes

### Partitionnement

part_usa = cutree(treeWard_usa,k=5)

part_usa = factor(part_usa,levels = c(1, 2, 3, 4, 5),
              labels = c("part_brut_1", "part_brut_2", "part_brut_3", "part_brut_4", "part_brut_5"))
part_usa

res_usa_nnorm = PCA(USArrests,ncp = 3, graph = F, scale.unit = F, quanti.sup = 3)

couleurs = c("black","red", "green", "blue", "violet")

proj_ind_usa = fviz_pca_ind(res_usa_nnorm, axes=c(1,2), repel = T, col.ind = part_usa, palette = couleurs)

proj_ind_usa

proj_var_usa =fviz_pca_var(res_usa_nnorm,axes=c(1,2),col.var = "contrib", 
                       gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)
proj_var_usa

# res_usa_nnorm_test = PCA(cbind(USArrests,part_usa),ncp = 3, graph = F, scale.unit = F, quanti.sup = 3, quali.sup = 5)

# proj_ind_usa_test = plot.PCA(res_usa_nnorm_test, choix = "ind", axes=c(1,2), repel = T, habillage = 5, repel = T)

# proj_ind_usa_test

## données standardisés

### Partitionnement

part_usa2 = cutree(treeWard_usa2,k=5)

part_usa2 = factor(part_usa2, levels = c(1, 2, 3, 4, 5),
                  labels = c("part_stand_1", "part_stand_2", "part_stand_3", "part_stand_4", "part_stand_5"))
part_usa2

res_usa_norm = PCA(USArrests, ncp = 3, graph = F, scale.unit = T, quanti.sup = 3)

proj_ind_usa2 = fviz_pca_ind(res_usa_norm, axes=c(1,2), repel = T, col.ind = part_usa2, palette = couleurs)

proj_ind_usa2

proj_var_usa2 =fviz_pca_var(res_usa_norm,axes=c(1,2),col.var = "contrib", 
                           gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)
proj_var_usa2

# Question 4

USArrests_new = cbind(USArrests, part_usa2)

result_catdes = catdes(USArrests_new, proba = 0.01, num.var = 5)

# Affichage des graphiques avec la méthode plot de la classe catdes

plot.catdes(result_catdes, show = "quanti", output = "figure", barplot = F)

plot.catdes(result_catdes, show = "quanti", output = "figure", barplot = T)

################################################################################### Exercice 5

# Question 1

load("protein.rda")

acp_Pro = PCA(protein,ncp = 9, graph = F)

acp_Pro$eig

sum(acp_Pro$eig[,1])

fviz_screeplot(acp_Pro)

nrow(protein)

Z1 = scale(protein, center = T, scale = T) * sqrt(25/24)

d_ward = dist(Z1, method = "euclidean", diag = T, upper = T)

treeWard_pro = hclust(d_ward^2/50, method = "ward.D")

plt1 = plot(treeWard_pro, hang = -1, xlab = "", sub = "", main = "CAH sur toutes variables standardisées")

sum(treeWard_pro$height)

# rm(pl1)

## CAH avec 9 composantes

protein_9comp = acp_Pro$ind$coord

ncol(protein_9comp)

d_ward1 = dist(protein_9comp, method = "euclidean", diag = T, upper = T)

treeWard_pro1 = hclust(d_ward1^2/50, method = "ward.D")

plot(treeWard_pro1, hang = -1, xlab = "", sub = "", main = "CAH sur toutes variables standardisées")

sum(treeWard_pro1$height)

all.equal(treeWard_pro$height,treeWard_pro1$height)

## CAH avec 5 composantes

protein_4comp = acp_Pro$ind$coord[,1:4]

d_ward2 = dist(protein_4comp, method = "euclidean", diag = T, upper = T)

treeWard_pro2 = hclust(d_ward2^2/50, method = "ward.D")

plot(treeWard_pro2, hang = -1, xlab = "", sub = "", main = "CAH sur les 4 variables synthétiques de ClustOfVar")

#grid.arrange(,,ncol=2)

all.equal(treeWard_pro1$height,treeWard_pro2$height)


# Question 2 (Clustering des variables)

library(ClustOfVar)

treevar <- hclustvar(protein)

plot(treevar, main="Clustering des variables")

rect.hclust(treevar, k=4)

treevar2 <- hclustvar(Z1)

plot(treevar2, main="Clustering des variables")

rect.hclust(treevar2, k=4)

treevar$height

treevar2$height

all.equal(treevar$height, treevar2$height)

cov <- cutreevar(treevar, k = 4)

# Nouvelles données avec 4 variables synthétiques

newdat3 <- cov$scores

newdat3


######################################################################################### TP2

# Appliquer avec R la méthode des kmeans en prenant comme centres initiaux les points A et B

km = kmeans(X2, centers = X2[1:2,], iter.max = 10, nstart = 1)

km$centers

km

################################################################################# Exercice 2

# Question 1

fromage = read.table("fromage.txt", header = T, sep = "\t",
                     row.names = 1, dec = ".")

# Question 2

Z = scale(fromage, center = T, scale = T) * sqrt(29/28) 

# Question 3

d2 = dist(Z, method = "euclidean", diag = T, upper = T)

treeWard2 = hclust(d2^2/58, method = "ward.D")

treeWard2$height

plot(treeWard2, hang = -1, xlab = "", sub = "", main = "Ward")

# Inertie totale

T2 = sum(treeWard2$height)

# Beetween

h2 = sort(treeWard2$height, decreasing = T)

B2 = sum(h[1:4])

B2

# Partitionnement

part = cutree(treeWard2,k=5)

part_ward = factor(part,levels = c(1, 2, 3, 4, 5),
              labels = c("Groupe1", "Groupe2", "Groupe3", "Groupe4", "Groupe5"))
part_ward

# Question 4

(B2/T2)*100

# Question 5

km_frm1 = kmeans(Z, 5, iter.max = 10, nstart = 1)

100 *km_frm1$betweenss/km_frm1$totss


# Question 6

km_frm2 = kmeans(Z, 5, iter.max = 10, nstart = 50)

100 *km_frm1$betweenss/km_frm1$totss

# Question 7


# Question 8

## L'algo de kmeans donne la meilleure partition en cinq classe car 
## son inertie inter est plus grande

# Question 9 (Comparaison)

library(pdfCluster)

adj.rand.index(km_frm2$cluster, part)

# L'indice de Rand ajusté varie de -1 à 1, où 1 indique une concordance parfaite entre les partitions et 
# 0 indique un alignement aléatoire entre les partitions. Une valeur positive indique une similarité supérieure à ce qui serait attendu par hasard, 
# tandis qu'une valeur négative indique une dissimilarité supérieure à ce qui serait attendu par hasard.

table(km_frm2$cluster)

table(part)

table(km_frm2$cluster, part)


################################################################################## Exercice 3

# Question 1

load("protein.rda")

nrow(protein)

Z1 = scale(protein, center = T, scale = T) * sqrt(25/24)

d_ward = dist(Z1, method = "euclidean", diag = T, upper = T)

treeWard_pro = hclust(d_ward^2/50, method = "ward.D")

plt1 = plot(treeWard_pro, hang = -1, xlab = "", sub = "", main = "CAH sur toutes variables standardisées")

sum(treeWard_pro$height)

# Question 2

h_pro = sort(treeWard_pro$height, decreasing = T)

B_pro = sum(h_pro[1:2])

B_pro

B_pro*100/9

part_pro = cutree(treeWard_pro,k=3)

rect.hclust(treeWard_pro, k=3)

# Question 3

# Calculer les centres de gravité des clusters de la CAH

cent_grav = aggregate(Z1, by = list(part_pro), FUN = mean)

cent_grav = cent_grav[-1]

# Appliquer la méthode des k-means en utilisant les centres initiaux de la CAH

result_consol = kmeans(Z1, centers = cent_grav, nstart = 50)

100 * result_consol$betweenss/result_consol$totss

# Question 4 (Croisement)

table(part_pro, result_consol$cluster)


################################################################################## Exercice 4

# Question 1

accidents = read.table("urbanGB.txt", header = T, sep = ",", dec = ".")

colnames(accidents) <- c("longitude", "latitude")

# plot(accidents$longitude, accidents$latitude, pch = "o", col = "black", xlab = "Longitude", ylab = "Latitude", main = "Localisation des accidents de voiture en Grande-Bretagne")

# Question 2

k_macc = kmeans(accidents, 1000, iter.max = 20, nstart = 1)

100 *k_macc$betweenss/k_macc$totss

acc_1000 = as.data.frame(k_macc$centers)

plot(acc_1000$longitude, acc_1000$latitude, pch = "+", col = "black", xlab = "Longitude", ylab = "Latitude", main = "Localisation des accidents de voiture en Grande-Bretagne")

# Question 3

dward_acc = dist(acc_1000,method = "euclidean", diag = T, upper = F)

dward_acc

tree_acc = hclust(dward_acc^2/2000, method = "ward.D")

plot(tree_acc, hang = -1, xlab = "", main = "Ward des 1000 points pondérés")

part_acc = cutree(tree_acc, k = 10)

rect.hclust(tree_acc, k = 10)

h_acc = sort(tree_acc$height, decreasing = T)

B_acc = sum(h_acc[1:9])

100*B_acc/sum(h_acc)


# Quetion 4

# Charger les données de la carte du monde
worldmap = map_data('world')

# Filtrer les données pour ne conserver que les informations sur l'Angleterre
england_map = subset(worldmap, region == "UK")

# Créer la carte de l'Angleterre
carte = ggplot() +
  geom_polygon(data = england_map, aes(x = long, y = lat, group = group),
               fill = 'gray90', color = 'black') +
  coord_fixed(ratio = 1.3, xlim = c(-10, 3), ylim = c(50, 59)) +
  theme_void()

# Ajouter les 1000 points moyens d'accidents colorés en fonction de leur classe
carte = carte +
  geom_point(data = acc_1000, aes(x = longitude, y = latitude, color = as.factor(part_acc))) +
  scale_color_brewer(palette = "Set3", name = "Cluster")

# Afficher la carte
print(carte)


################################################################################## Exercice 5

# Question 1

library(FactoMineR)

library(factoextra)

data(wine)

print(wine[1:5, c(1, 2, 27, 26, 29)])

famd_res = FAMD(wine, ncp = 5, graph = FALSE)

fviz_eig(famd_res)

proj_ind_wine = fviz_famd_ind(famd_res, axes=c(1,2), repel = T, col.ind = "contrib",
                              gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"))
proj_ind_wine

proj_var_wine =fviz_famd_var(famd_res,axes=c(1,2),col.var = "contrib", 
                            gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)
proj_var_wine

# Question 2

summary(wine)

num_var_quan = sum(sapply(wine, is.numeric))

num_var_qual = ncol(wine) - num_var_quan

num_var_qual

nrow(wine) - 1 

num_var_quan + 7 - num_var_qual

famd_res2 = FAMD(wine, ncp = 20, graph = FALSE)

fviz_eig(famd_res2)

proj_ind_wine2 = fviz_famd_ind(famd_res2, axes=c(1,2), repel = T, col.ind = "contrib",
                              gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"))
proj_ind_wine2

proj_var_wine2 =fviz_famd_var(famd_res2,axes=c(1,2),col.var = "contrib", 
                             gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)
proj_var_wine2

# Question 3

## CAH

wine_20comp = famd_res2$ind$coord

ncol(wine_20comp)

dwine_ward = dist(wine_20comp, method = "euclidean", diag = T, upper = T)

treeWard_wine = hclust(dwine_ward^2/42, method = "ward.D")

plot(treeWard_wine, hang = -1, xlab = "", sub = "", main = "CAH sur les vins")

rect.hclust(treeWard_wine, k = 4)

part_wine = cutree(treeWard_wine, k=4)

# Question 4

# Calculer les centres de gravité des clusters de la CAH

cent_grav_wine = aggregate(wine_20comp, by = list(part_wine), FUN = mean)

cent_grav_wine = cent_grav_wine[-1]

# Appliquer la méthode des k-means en utilisant les centres initiaux de la CAH

res_cons_wine = kmeans(wine_20comp, centers = cent_grav_wine, nstart = 50)

100 * res_cons_wine$betweenss/res_cons_wine$totss

# Question 4 (Croisement)

table(part_wine, res_cons_wine$cluster)

# seul de vin ont changé de classe

######################################################### TP 3 (Partitionnement avec les GMM)

# Question 1

library(mvtnorm)

n = 1000

set.seed(50)

U = runif(n)

X = matrix(NA,n,2)

Z = rep(NA,n)

for(i in 1:n) {

  if(U[i] < .3) {
    X[i,] = rmvnorm(1,c(0,0),diag(c(1,1)))
    Z[i] = 1} 
  
  else {
    X[i,] = rmvnorm(1,c(3,3),diag(c(1,1)))
    Z[i] = 2 }}

Z = as.factor(Z)

levels(Z) = c("classe1", "classe2")

colnames(X) = c("X1","X2")

plot(X, pch=20, col=Z)