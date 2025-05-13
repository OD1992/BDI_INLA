rm(list=ls())

# Charger les librairies
library(INLA)
library(malariaAtlas)
library(raster)
library(sp)
library(tidyverse)
library(lattice)
library(gridExtra)

# Importer les bibliothèques nécessaires -----------------------------------------------------------
#pour la préparation des données, veuillez consulter le script data_prep.R

load('input/MDG_clean.Rdata')

# Construction du maillage -------------------------------------------------------
#Avertissement :
#il n’existe pas de règle stricte pour déterminer la taille et l’extension spatiale appropriées du maillage. C’est à l’analyste de définir les paramètres du maillage, qui varient selon les cas. Les modèles basés sur un maillage avec un grand nombre de sommets sont plus gourmands en ressources informatiques et ne donnent pas nécessairement de meilleurs résultats qu’un maillage plus grossier. Nous recommandons donc d’utiliser un maillage relativement grossier lors de la phase préliminaire de l’analyse et d’opter pour un maillage plus fin uniquement comme étape finale, lorsque les résultats de l’analyse sont satisfaisants. Pour plus de détails sur la construction d’un maillage, voir la Section 2.1 de l’article de Lindgren et Rue : https://www.stat.washington.edu/peter/591/Lindgren.pdf.

#le maillage est un compromis entre le champ aléatoire et le coût computationnel
#d’après notre compréhension, plus le maillage est fin (c’est-à-dire que mesh$n > nombre de points), plus il est
#coûteux en calcul ; malheureusement, trouver le bon maillage demande un peu de tâtonnements
#à mon avis, avoir mesh$n approximativement similaire ou inférieur au nombre de points serait préférable

coords = cbind(MDG_pr_data$longitude, MDG_pr_data$latitude)
bdry <- inla.sp2segment(MDG_shp)
bdry$loc <- inla.mesh.map(bdry$loc)

## coordonnées x et y dans les données de réponse
#utilisation de max.edge
#max.edge est la longueur maximale autorisée d’un triangle ; plus la valeur est basse, plus la résolution est élevée
#max.edge = c(valeur à l’intérieur de la frontière, valeur à l’extérieur de la frontière) ; cela est utile pour éviter

#avec uniquement max.edge

mesh0 <- inla.mesh.2d(loc = coords, boundary = bdry, max.edge=c(0.5))
par(mfrow=c(1,1))
plot(MDG_shp)
plot(mesh0,add=TRUE)

#max.edge avec des valeurs différentes pour l’intérieur et l’extérieur (pour l’effet de bordure)

mesh1 <-
par(mfrow=c(1,1))
plot(MDG_shp)
plot(mesh1,add=TRUE)



#ajout d’un décalage (offset)
#offset définit jusqu’où vous souhaitez étendre votre domaine (par ex. une seconde boîte de frontière)
#pour la frontière intérieure et la frontière extérieure ; l’unité géographique doit être la même que max.edge
mesh2 <-
#REMARQUE : inclure l’argument boundary rend la valeur de la frontière intérieure redondante.
#vous pouvez essayer de retirer l’argument boundary ici et vous verrez une différence entre
#les bords intérieurs et extérieurs

#cutoff peut être utilisé pour éviter la construction de trop petits triangles autour des emplacements de données groupés
mesh3 <-
par(mfrow=c(1,1))
plot(MDG_shp)
plot(mesh3,add=TRUE)

#QUESTION 1 : jouez avec différentes valeurs pour max.edge, offset et cutoff
#et observez les résultats

# Construction spatiale -----------------------------------------------
#Les modèles spatiaux peuvent être vus comme des modèles gaussiens multivariés ;
#la structure de corrélation est déterminée par une fonction de covariance spatiale (modélisée comme une fonction de covariance de Matérn)
#pour des raisons de calcul, le champ gaussien complet est approché par un champ aléatoire de Markov gaussien (GMRF)
#Approcher le GMRF est très coûteux en calcul, c’est pourquoi on utilise INLA

#construction de A
#la matrice A projette le GMRF depuis les nœuds du maillage vers les n emplacements d’observation
#vous obtenez une matrice de dimension nombre d’observations × nombre de nœuds
A<-
dim(A)

#création du spde pour la structure spatiale
#c’est une fonction qui s’appuie sur la structure spatiale que l’on suppose exister sur notre maillage
#c’est l’équivalent d’un GMRF dans un modèle spatial
spde <-

#Remarque : les paramètres de portée et de variance doivent être interprétés
#avec précaution car ils ne sont pas totalement identifiables par le modèle ajusté

#enfin, nous créons tous les index requis pour le modèle SPDE
#ici, nous définissons le "nom de l'effet" qui sera utilisé dans la formule
iset <-

# Empilage des données -----------------------------------------------------------
#crée une pile INLA avec les données d’entrée
#c’est la structure de INLA. Pensez-y comme une liste d’entrées
#vous entrez les données (c.-à-d. votre réponse)
#puis la matrice A et vous ajoutez un 1 pour la liste des covariables
#les effets doivent contenir une liste des effets spatiaux et une liste des covariables
stk <-



# Construction du modèle ----------------------------------------------------------
#c’est similaire à glm/glmer ; créez une formule avec le champ spatial

#vous pouvez ajouter +1 pour inclure l’interception par défaut ou -1 pour l’exclure et la coder vous-même
formula0<-y ~ +1  + Elevation  + f(spatial.field, model=spde)

#1. Ajustement du modèle (la fonction INLA)
model0<-inla()

#vérification du modèle
summary(model0)


# Sélection du modèle ---------------------------------------------------------

###sélection du modèle avec WAIC (d’autres critères peuvent être utilisés)
mypb <- txtProgressBar(min = 0, max = 5, initial = 0, width = 150, style = 3)

for(i in 1:6){

  f1 <- as.formula(paste0("y ~ +1  + f(spatial.field, model=spde) + ", paste0(colnames(covs_df)[1:i], collapse = " + ")))

  model1<-inla(f1, data=inla.stack.data(stk,spde=spde),family= 'binomial',
               Ntrials = n,
               control.predictor=list(A=inla.stack.A(stk),compute=TRUE),
               control.compute = list(dic = TRUE, cpo=TRUE, waic = TRUE)) #verbose=TRUE,

  model_selection <- if(i==1){rbind(c(model = paste(colnames(covs_df)[1:i]),waic = model1$waic$waic))}else{rbind(model_selection,c(model = paste(colnames(covs_df)[1:i],collapse = " + "),waic = model1$waic$waic))
  }
  setTxtProgressBar(mypb, i, title = "number complete", label = i)

}

model_selection

#QUESTION 2 : quel modèle vous semble le meilleur ? Pourquoi ?


# Ajustement du modèle (Meilleur modèle) --------------------------------------------------

# Ré-ajustement du modèle avec la meilleure combinaison de covariables
formula <- y ~ +1 + f(spatial.field, model=spde) + Access + Elevation + EVI + LST_day

# Ajustement du modèle INLA
model1 <- inla(formula,
               data = inla.stack.data(stk, spde = spde),
               family = 'binomial',
               Ntrials = n,  # nombre d'essais pour la distribution binomiale
               control.predictor = list(A = inla.stack.A(stk), compute = TRUE),  # pour calculer les marges du prédicteur linéaire
               control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),  # pour diagnostics du modèle (DIC, WAIC) et structure GMRF
               verbose = FALSE)  # TRUE pour voir les logs

# Résumé des résultats du modèle
summary(model1)

# Pour voir les paramètres fixes estimés, par exemple les coefficients des covariables
model1$summary.fixed

# Résumé des hyperparamètres estimés (ex. : log(tau), log(kappa))
model1$summary.hyperpar

# Obtenir les résultats du modèle SPDE pour extraire les paramètres transformés
model1.res <- inla.spde2.result(model1, 'spatial.field', spde, do.transf = TRUE)

# Résumé de la plage spatiale (range) en échelle log – donne une idée de la portée de la corrélation spatiale
model1.res$summary.log.range.nominal

# Résumé de la variance marginale en échelle log
model1.res$summary.log.variance.nominal



#Tracer les paramètres estimés
##observer les graphiques pour les paramètres fixes
par(mfrow=c(3,2))
plot(model1$marginals.fixed[[1]], ty = "l", xlab = expression(beta[0]), ylab = "Density")
plot(model1$marginals.fixed[[2]], ty = "l", xlab = expression(beta[Access]), ylab = "Density")
plot(model1$marginals.fixed[[3]], ty = "l", xlab = expression(beta[Elevation]), ylab = "Density")
plot(model1$marginals.fixed[[4]], ty = "l", xlab = expression(beta[EVI]), ylab = "Density")
plot(model1$marginals.fixed[[5]], ty = "l", xlab = expression(beta[LST_day]), ylab = "Density")

#observer les graphiques pour les hyperparamètres
par(mfrow=c(1,3))
plot(model1.res$marginals.var[[1]], ty = "l", xlab = expression(sigma[randomfield]^2), ylab = "Density")
plot(model1.res$marginals.kap[[1]], type = "l", xlab = expression(kappa), ylab = "Density")
plot(model1.res$marginals.range[[1]], type = "l", xlab = "range nominal", ylab = "Density")

#Projection sur une grille pour observer le champ aléatoire
#observation du champ spatial et de son apparence
gproj <- inla.mesh.projector(mesh3,  dims = c(300, 300))
g.mean <- inla.mesh.project(gproj, model1$summary.random$spatial.field$mean)
g.sd <- inla.mesh.project(gproj, model1$summary.random$spatial.field$sd)

grid.arrange(levelplot(g.mean, scales=list(draw=F), xlab='', ylab='', main='mean',col.regions = heat.colors(16)),
             levelplot(g.sd, scal=list(draw=F), xla='', yla='', main='sd' ,col.regions = heat.colors(16)), nrow=1)


# Prédiction du modèle (OPTION 1) --------------------------------------------------------
### OPTION 1 : Prédiction combinée avec l’ajustement

#créer une grille de surface avec les coordonnées de chaque pixel
reference.image <- raster('covariates/Access.tif')
in.country <- which(!is.na(getValues(reference.image)))
reference.coordinates <- coordinates(reference.image)[in.country,]

#transformer ces coordonnées en points et extraire les covariables pour la grille de prédiction
pred.points <- SpatialPoints(reference.coordinates, proj4string = crs(MDG_shp))
covs <- list.files('covariates/', full.names = T) %>% stack()
pred.covs <- raster::extract(covs, pred.points, df=T)

#reconstruire la matrice A pour la prédiction
Aprediction <- inla.spde.make.A(mesh = mesh3 , loc = reference.coordinates);
dim(Aprediction)

#reconstruire la pile pour la prédiction
stk.pred <- inla.stack(data=list(y=NA), #la réponse (ici NA car on fait une prédiction)
                       A=list(Aprediction,1),  #la matrice A
                       #ceci inclut vos covariables et composantes spatiales
                       effects=list(c(list(Intercept=1), #l’interception
                                      iset),  #l’index spatial
                                    list(Elevation = pred.covs$Elevation,
                                         Access=pred.covs$Access,
                                         LST_day = pred.covs$LST_day,
                                         Rain = pred.covs$Rain,
                                         EVI = pred.covs$EVI)
                       ),
                       #un nom rapide pour pouvoir l’appeler facilement
                       tag='pred')


#joindre la pile de prédiction avec celle des données complètes
stk.full <- inla.stack(stk, stk.pred)

#exécuter un modèle INLA pour la prédiction
##REMARQUE : ce code peut prendre plusieurs minutes ;
#nous l’avons exécuté et fourni la sortie à titre d’exemple

p.res.pred<-inla(formula,
                 data=inla.stack.data(stk.full,spde=spde),
                 family= 'binomial',
                 Ntrials = n,
                 control.predictor=list(link = 1, A=inla.stack.A(stk.full),
                                        compute=FALSE),
                 control.compute = list(config = TRUE),
                 control.inla(strategy = 'simplified.laplace', huge = TRUE),
                 verbose = FALSE)


## Extraction des valeurs prédites
index.pred<-inla.stack.index(stk.full, "pred")$data
post.mean.pred.logit<-p.res.pred$summary.linear.predictor[index.pred,"mean"]
p.pred<-exp(post.mean.pred.logit)/(1 + exp(post.mean.pred.logit))

###visualisation
x <- as.matrix(reference.coordinates)
z <- as.matrix(p.pred)
pr.mdg.in<-rasterize(x, reference.image, field=z, fun='last', background=0)
par(mfrow=c(1,1))
plot(pr.mdg.in)
writeRaster(pr.mdg.in, filename="output/PR.MDG_withinINLA.tif",
            format = "GTiff", overwrite=TRUE, options = c('COMPRESS' = 'LZW'))




# Prédiction du modèle (OPTION 2) --------------------------------------------------------
### OPTION 2 : Prédiction après l’ajustement
## en utilisant les résultats de Model1
model = model1
## recall:: formula<-y ~ -1 + Intercept + f(spatial.field, model=spde) + Access + Elevation + EVI + LST_day

# Covariables pour les points de prédiction
Access <- pred.covs$Access
Elevation <-  pred.covs$Elevation
EVI <- pred.covs$EVI
LST_day <-  pred.covs$LST_day

#création de la structure spatiale
sfield_nodes <- model$summary.random$spatial.field['mean']
field <- (Aprediction %*% as.data.frame(sfield_nodes)[, 1])

#créer une matrice vide pour remplir les prédictions
pred <- matrix(NA, nrow = dim(Aprediction)[1], ncol = 1)

## Calculer les valeurs prédites en utilisant la formule de régression
pred <- model$summary.fixed['(Intercept)', 'mean'] +
  model$summary.fixed['Access', 'mean'] * Access +
  model$summary.fixed['Elevation', 'mean'] * Elevation +
  model$summary.fixed['EVI', 'mean'] * EVI +
  model$summary.fixed['LST_day', 'mean'] * LST_day +
  field

# écrire les résultats dans un fichier csv
results <- exp(pred)/(1+exp(pred))

# écrire les résultats dans un fichier raster
x <- as.matrix(reference.coordinates)
z <- as.matrix(results)
pr.mdg.out <- rasterFromXYZ(cbind(x, z))
plot(pr.mdg.out)
writeRaster(pr.mdg.out, filename="output/PR.MDG_outsideINLA.tif",
            format = "GTiff", overwrite=TRUE, options = c('COMPRESS' = 'LZW'))

# lire le fichier raster existant
library(raster)
tt<- raster("")
plot(tt)

## Optionnel - Selon le temps disponible
# Validation du modèle ---------------------------------------------------
#Tout d’abord, nous allons diviser les données en ensembles d’apprentissage et de test (75 % pour l’apprentissage)

## 75 % de la taille de l’échantillon
smp_size <- floor(0.75 * nrow(MDG_pr_data))

## fixer la graine pour rendre la partition reproductible
## sinon la fonction sample() fournit des valeurs differentes
set.seed(123)
train_ind <- sample(seq_len(nrow(MDG_pr_data)), size = smp_size, replace = FALSE)

train <- MDG_pr_data[train_ind, ]
test <- MDG_pr_data[-train_ind, ]
test$positive <- NA  #make the y values for test NA

#ensuite, nous reconstruisons la matrice A pour l’ensemble d’apprentissage
train_coords <- coords[train_ind,]
Ae<-inla.spde.make.A(mesh=mesh3,loc=as.matrix(train_coords));dim(Ae)

#On peut en construire une pour la partie test
test_coords <- coords[-train_ind,]
Ap <- inla.spde.make.A(mesh = mesh3, loc = test_coords);dim(Ap)

#Visualiser les ensembles d'apprentissage et de test
par(mfrow=c(1,2))
plot(MDG_shp)
points(test_coords, pch=21, bg=1,col="white", cex=1.2)
plot(MDG_shp)
points(train_coords, pch=21, bg=1,col="blue", cex=1.2)

# Ensuite, nous créons les piles (stacks) pour les estimations et les prédictions
stk.e <- inla.stack(data=list(y=train$pf_pos, n=train$examined), #la réponse
                    A=list(Ae,1),  #la matrice A ; le 1 est inclus pour créer une liste (covariables)
                    #ce sont vos covariables
                    effects=list(c(list(Intercept=1), #l’interception
                                   iset),  #l’index spatial
                                 list(Elevation = train$Elevation,
                                      Access=train$Access,
                                      LST_day = train$LST_day,
                                      Rain = train$Rain,
                                      EVI = train$EVI)
                    ),
                    #un nom rapide pour pouvoir l’appeler facilement
                    tag='est')

stk.p <- inla.stack(data=list(y=test$pf_pos, n=test$examined), #la réponse
                    A=list(Ap,1),  #la matrice A ; le 1 est inclus pour créer une liste (covariables)
                    #ce sont vos covariables
                    effects=list(c(list(Intercept=1), #l’interception
                                   iset),  #l’index spatial
                                 list(Elevation = test$Elevation,
                                      Access=test$Access,
                                      LST_day = test$LST_day,
                                      Rain = test$Rain,
                                      EVI = test$EVI)
                    ),
                    #un nom rapide pour pouvoir l’appeler facilement
                    tag='pred')

#les combiner
stk.full <- inla.stack(stk.e, stk.p)

p.res<-inla(formula, data=inla.stack.data(stk.full,spde=spde),family= 'binomial',
            Ntrials = n,
            control.predictor=list(link = 1, A=inla.stack.A(stk.full),compute=TRUE),  #compute permet d’obtenir les marginales du prédicteur linéaire
            control.compute = list(config = TRUE), #diagnostics du modèle et config = TRUE pour obtenir le GMRF
            verbose = FALSE) #vous pouvez mettre verbose=TRUE pour voir le journal (log)

#obtenir les prédictions
index.pred <- inla.stack.index(stk.full, "pred")$data
post.mean.logit <- p.res$summary.linear.predictor[index.pred,'mean'] #la postérieure est en échelle logit
pred <- exp(post.mean.logit)/(1 + exp(post.mean.logit))
obs <- test$pf_pr #c’est le nombre de positifs / nombre examinés

#visualiser avec un graphique
# Sauvegarder le graphique base R dans un fichier PNG
png("output/validation.png", width = 800, height = 800)
p<- plot(obs, pred, xlab = "Observé", ylab = "Prévu", col='red')
abline(a=0, b=1)
dev.off()

#corrélation entre les valeurs observées et prédites
cor(obs, pred)

#prédiction pas bonne - Pas sûr pourquoi les résultats sont mauvais,
#peut-être qu'une transformation logit empirique aiderait ?
#Ou peut-être qu’il faut simplement des covariables non linéaires.
