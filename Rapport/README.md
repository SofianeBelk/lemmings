# Rapport du projet Lemmings

# Organisation du projet

Vous trouverez dans le projet les répertoires :

* **app** : qui contient le point d'entrée du programme (le *main*). 
* **assets** : qui contient les *images* formant l'interface graphique.
* **lib*** : qui contient une base de divers niveaux.
* **src** : qui contient le code source du projet.
* **test** : qui contient l'ensemble des tests du projet.

# Creation d'un niveau

Pour créer un niveau, chaque case doit correspondre à une des lettres :

* **X** : pour une case en métal.
* **0** : pour une case en terre.
* **E** : pour une entrée.
* **S** : pour une sortie.
* **M** : pour une mine.
* **Espace** : pour une case vide.

Et le niveau doit respecter certaines propriétés :

* Les bords du niveau doivent être en métal.
* Le niveau possède exactement une entrée et une sortie.
* L'entrée se trouve au dessus d'une case vide.
* La sortie se trouve au dessus d'une case en métal.
* Toutes les coordonnées comprise entre 0 et la hauteur/largeur du niveau sont associées à une case et réciproquement.
* Le niveau doit être écrit dans un fichier text et donné en argument lors du lancement de l'application.

Voici un exemple de niveau valide :
```
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
X E                            X
X                              X
X0000000MXXXX0000      00000000X
X                              X
X                              X
X           00000000000        X
X                              X
X000MXXXM000000                X
X                              X
X                              X
X                              X
X          00000000000000000000X
X                              X
X                              X
X0000000000000000000      00000X
X                              X
X                              X
X                  000000000000X
X                              X
X                             SX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
```

## Lancement du jeu

Pour lancer le jeu, il suffit de lancer la commande `stack run lib/XXXX.txt` (remplacer `XXXX.txt` par un nom de fichier valide).

## Comment jouer 

Pour modifier le comportement d'un *lemming*, il suffit de faire un click de souris dessus et appuyez sur une des touches :

* **w** : le lemming devient un *Demineur*, dès qu'il passe sur une mine, il la désactive et redevient *Marcheur*
* **x** : le lemming devient un *Exploseur*, il explose en détruisant toutes les cases en *terre* qui l'entourent et tue les lemmings qui sont à côté de lui.
* **c** : le lemming devient un *Creuseur*, si la case du bas à côté de lui est en *terre* il la détruit.
* **v** : le lemming devient un *Bloqueur*, il empêche les autres lemmings de passer pendant 8 tours de jeu.
* **b** : le lemming devient un *Constructeur*, si la case à côté de lui est vide, il pose un bloque en *terre*.
* **n** : le lemming devient un *Boucheur*, il bouche tout les vides sur lesquels il passe jusqu'à vider son inventaire (pour changer la taille de son inventaire, changer la valeur de retour de `poseMax`).
* **Esc** : pour quitter le jeu.

