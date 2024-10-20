library(dplyr)

library(FactoMineR)

library(factoextra)

library(ggplot2)

library(GGally)

library(ggrepel)

library(gridExtra)

#chargement du fichier Rdata
load("data.baby.RData")
X=data.baby2.comp

res.acp_nnorm = PCA(X, scale.unit = F, graph = F)

fviz_pca_var(res.acp_nnorm, col.var = "contrib", gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)

#res.acp_nnorm$var$coord[,1]
# matrice des données centrée réduites 
Z = scale(X)*sqrt(362/361)
#matrice de corrélation 
corZ=cor(Z)
corZ
# ACP normée
acp_Z = PCA(X, scale.unit = T, graph = F)

#  tableau de valeurs propres
acp_Z$eig
#éboulis du tableau des valeurs propres

fviz_screeplot(acp_Z,ncp=13,addlabels=TRUE)
#les cerles  cercles de correlation
cercle1_2=fviz_pca_var(acp_Z,axes=c(1,2),col.var = "contrib", gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)

cercle3_4=fviz_pca_var(acp_Z,axes=c(3,4),col.var = "contrib", gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)

grid.arrange(cercle1_2,cercle3_4,ncol=2)
premier_plan_fac = acp_Z$ind$coord[,c(1,2)]
cor_fac = premier_plan_fac[c(237,218,216,144,141,140,149,340,125),]
cor_fac
var_abs1 = apply(acp_Z$ind$coord, 2, var) * 361/362
var_abs1
f= function(x){sum(x^2)} # fonction tirée à partir de la contribution
var_absc2 = apply(acp_Z$var$coord, 2, f)
var_absc2
fviz_contrib(acp_Z, choice = "ind", axes = 1, top = 20)
fviz_contrib(acp_Z, choice = "ind", axes = 2, top = 20)
fviz_contrib(acp_Z, choice = "ind", axes =1:2, top = 20)

proj_ind1=fviz_pca_ind(acp_Z,axes=c(1,2),col.ind = "cos2", gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)
proj_ind1
proj_ind2=fviz_pca_ind(acp_Z,axes=c(3,4),col.ind = "cos2", gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)
proj_ind2

X1 = X[,-c(10:13)]
# matrice des données centrée réduites 
Z1=scale(X1)*sqrt(362/361)
#matrice de corrélation 
corZ1=cor(Z1)
corZ1
# ACP normée
acp_Z1= PCA(X,quanti.sup=10:13, scale.unit = T,graph = F)

#  tableau de valeurs propres
acp_Z1$eig
#éboulis du tableau des valeurs propres

fviz_screeplot(acp_Z1,ncp=9,addlabels=TRUE)
#les cerles  cercles de correlation
cercleZ1_2=fviz_pca_var(acp_Z1,axes=c(1,2),col.var = "contrib", gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)

cercleZ3_4=fviz_pca_var(acp_Z1,axes=c(3,4),col.var = "contrib", gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)

grid.arrange(cercleZ1_2,cercleZ3_4,ncol=2)

premier_plan_facZ1 =acp_Z1$ind$coord[,c(1,2)]
cor_facZ1=premier_plan_facZ1[c(237,218,216,144,141,140,149,340,125),]
cor_facZ1
var_absZ1 = apply(acp_Z1$ind$coord, 2, var)*361/362
var_absZ1
var_abscZ2 = apply(acp_Z1$var$coord, 2, f)
var_abscZ2
fviz_contrib(acp_Z1, choice = "ind", axes = 1, top = 20)
fviz_contrib(acp_Z1, choice = "ind", axes = 2, top = 20)
fviz_contrib(acp_Z1, choice = "ind", axes =1:2, top = 20)

proj_indZ1=fviz_pca_ind(acp_Z1,axes=c(1,2),col.ind = "cos2", gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)
proj_indZ1
proj_indZ2=fviz_pca_ind(acp_Z1,axes=c(3,4),col.ind = "cos2", gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)
proj_indZ2

ind_plts_filtr <- which(acp_Z1$ind$coord[, 1] > 3 & acp_Z1$ind$coord[, 2] < 0)

acp_Z1$ind$coord[ind_plts_filtr,c(1,2)] # vérification

X2=X[-ind_plts_filtr,-c(10:13)]
# matrice des données centrée réduites 
Z2=scale(X2)*sqrt(nrow(X2)/(nrow(X2)-1))
#matrice de corrélation 
corZ2 = cor(Z2)
corZ2
# ACP normée
acp_Z2 = PCA(X[-ind_plts_filtr,], quanti.sup = 10:13, scale.unit = TRUE , graph=FALSE)

#  tableau de valeurs propres
acp_Z2$eig
#éboulis du tableau des valeurs propres

fviz_screeplot(acp_Z2,ncp=9,addlabels=TRUE)
#les cerles  cercles de correlation
cercleZZ1_2=fviz_pca_var(acp_Z2,axes=c(1,2),col.var = "contrib", gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)

cercleZZ3_4=fviz_pca_var(acp_Z2,axes=c(3,4),col.var = "contrib", gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)

grid.arrange(cercleZZ1_2,cercleZZ3_4,ncol=2)
var_abscZZ1 = apply(acp_Z2$ind$coord, 2, var)*(nrow(X2)-1)/nrow(X2)
var_abscZZ1
var_abscZZ2 = apply(acp_Z2$var$coord, 2, f)
var_abscZZ2
fviz_contrib(acp_Z2, choice = "ind", axes = 1, top = 20)
fviz_contrib(acp_Z2, choice = "ind", axes = 2, top = 20)
fviz_contrib(acp_Z2, choice = "ind", axes =1:2, top = 20)

proj_indZZ1=fviz_pca_ind(acp_Z2,axes=c(1,2),col.ind = "cos2", gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)
proj_indZZ1
proj_indZZ2=fviz_pca_ind(acp_Z2,axes=c(3,4),col.ind = "cos2", gradient.cols = c("#FF5733", "#FFD700", "#4CAF50"), repel = T)
proj_indZZ2
