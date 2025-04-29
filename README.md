Introduction à INLA pour la modélisation géospatiale 
============
La présentation associée à ce tutoriel est disponible:
https://docs.google.com/presentation/d/12skzuN4HZfDNrday52C97bycvNzCP20R/edit#slide=id.p1

Vue d'ensemble 
------
Dans cet atelier, nous illustrerons comment utiliser le modèle SPDE avec le package R-INLA. Nous analyserons des données de prévalence parasitaire de Madagascar. Nous aborderons :

- Comment créer un maillage pour les effets spatiaux continus
- Implémentation du modèle SPDE avec R-INLA
- Sélection simple du meilleur modèle
- Prédiction spatiale avec et en dehors de "inla"
- Validation du modèle

Pour des détails approfondis, voir: www.math.ntnu.no/inla/r-inla.org/papers/jss/lindgren.pdf et : www.math.ntnu.no/inla/r-inla.org/tutorials/spde/spde-tutorial.pdf

### Data used
Données

Téléchargez le projet à partir de <https://github.com/PunamA/BDI_INLA> : unzippez et ouvrez `BDI_INLA.Rproj`. Utilisez `Intro_to_INLA_students.R`.

**Données de prévalence** : disponibles via le <a href=https://malariaatlas.org/pr-survey-data/>Malaria Atlas Project</a> (données MIS Madagascar 2021)

**Covariables** : Images satellites nettoyées, accessibles sur demande.

```{r data, eval = FALSE, echo = FALSE}
source('data_prep.R')
```

Ou chargez directement :

```{r}
load('input/MDG_clean.Rdata')

Chargement des bibliothèques nécessaires:

```{r}
library(INLA)
library(malariaAtlas)
library(raster)
library(sp)
library(tidyverse)
library(lattice)     
library(gridExtra)
```
Pour l' installation
```{r}
packages <- c("malariaAtlas", "raster", "sp", "tidyverse",
              "lattice", "gridExtra", "devtools", "rlang")
if(length(setdiff(packages, rownames(installed.packages()))) > 0) { 
  install.packages(setdiff(packages, rownames(installed.packages()))) }

# Pour INLA!!
if(length(setdiff("INLA", rownames(installed.packages()))) > 0){
  install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
}

```
**Ressources supplémentaires** :
1. Site web INLA <http://www.r-inla.org/>
2. Modeles spatiotemporel Bayesien <https://sites.google.com/a/r-inla.org/stbook>
3. Inference Bayesienne avec INLA et R-INLA <https://becarioprecario.bitbucket.io/inla-gitbook/index.html>
