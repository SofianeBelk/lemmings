# Lemmings

Implementation d'une mini-copie du jeu [Lemmings](https://fr.wikipedia.org/wiki/Lemmings_(jeu_vid%C3%A9o,_1991)) en *Haskell* en mettant en avant les concepts de la programmation fonctionnelle vu en cours dans le cadre de l'ue *Programmation avancée en Fonctionnelle* (**PAF**) vu en *Master 1 informatique - Science et Technologie du Logiciel* à *Sorbonne Université*.

## Installation

Pour pouvoir l'executer il faut avoir préalabement installer *Stack* et la bibliothèque *SDL*.

Puis exécutez les commandes :

```sh
$ cd lemmings
$ stack update
$ stack build
$ stack run lib/XXXXX.txt
```

Où XXXXX.txt correspond à un fichier *text* de niveau.

(Si la commande `stack update` n'est pas executée, l'interface risque de ne pas s'afficher.)

## Paramètres du jeu

Dans le fichier `src/etat.hs`, vous trouverez les trois fonctions qui permettent de modifier les paramètres du jeu :

```hs
hauteurMax :: Int
hauteurMax = 8          -- Hauteur chute maximale auquelle un Lemming peut survivre

poseMax :: Int
poseMax = 4             -- Nombre de cases dans l'inventaire du Lemming boucheur

nbLemmings :: Int
nbLemmings = 6          -- Nombre initial de Lemmings dans le niveau
````


## Comment jouer

Pour modifier le comportement d'un *lemming*, faites un click de souris dessus et appuiyez sur une des touches :
* **x** : le lemming devient un *Exploseur*, il explose en détruisant toutes les cases en *terre* qui l'entourent et tue les lemmings qui sont à côté de lui.
* **c** : le lemming devient un *Creseur*, si la case du bas à côté de lui est en *terre* il la détruit.
* **v** : le lemming devient un *Bloqueur*, il empêche les autres lemmings de passer pendant 8 tours de jeu.
* **b** : le lemming devient un *Constructeur*, si la case à côté de lui est vide, il pose un bloque en *terre*.
* **n** : le lemming devient un *Boucheur*, il bouche tout les vides sur lesquels il passe jusqu'à vider son inventaire (pour changer la taille de son inventaire, changer la valeur de retour de `poseMax`).
