# Rapport du projet Lemmings

## Manuel d'utilisation

### Organisation du projet

Vous trouverez dans le projet les répertoires :

* **app** : qui contient le point d'entrée du programme (le *main*). 
* **assets** : qui contient les *images* formant l'interface graphique.
* **lib*** : qui contient une base de divers niveaux.
* **src** : qui contient le code source du projet.
* **test** : qui contient l'ensemble des tests du projet.

### Creation d'un niveau

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

### Lancement du jeu

Pour lancer le jeu, il suffit de lancer la commande `stack run lib/XXXX.txt` (remplacer `XXXX.txt` par un nom de fichier valide).

### Comment jouer 

Pour modifier le comportement d'un *lemming*, il suffit de faire un click de souris dessus et appuyez sur une des touches :

* **w** : le lemming devient un *Demineur*, dès qu'il passe sur une mine, il la désactive et redevient *Marcheur*
* **x** : le lemming devient un *Exploseur*, il explose en détruisant toutes les cases en *terre* qui l'entourent et tue les lemmings qui sont à côté de lui.
* **c** : le lemming devient un *Creuseur*, si la case du bas à côté de lui est en *terre* il la détruit.
* **v** : le lemming devient un *Bloqueur*, il empêche les autres lemmings de passer pendant 8 tours de jeu.
* **b** : le lemming devient un *Constructeur*, si la case à côté de lui est vide, il pose un bloque en *terre*.
* **n** : le lemming devient un *Boucheur*, il bouche tout les vides sur lesquels il passe jusqu'à vider son inventaire (pour changer la taille de son inventaire, changer la valeur de retour de `poseMax`).
* **Esc** : pour quitter le jeu.

### Tests

Pour lancer les tests il suffit de lancer la commande `stack test`.


## Presentation du projet

L'objectif du projet est la création d'un "*Lemmings*" en *Haskell* **sur**, en 2D en utilisant la bibliothèque *SDL2*.

Dans une map, des lemmings sont générés et le joueur doit faire en sorte d'en conduire le maximum à la sortie en utilisant leurs différents potentiels au bon moment et au bon endroit.

Si tout les lemmings meurent, la partie se termine et le joueur a perdu, sinon, si il arrive à en sauver quelques-uns, on parle de victoire partielle, sinon, si tout les lemmings sont sauvés, alors le joueur a gagné.

Au lancement du jeu une interface graphique s'ouvre, quand on lance le niveau donné ci-dessus, on a l'affichage suivant :

![](./niveau1.png)

## Propositions

### Coord

Invariant du type *Coord* :

* Les coordonnées sont positives.
* Un déplacement à gauche puis à droite revient à la même coordonnée.
* Un déplacement à droite puis à gauche revient à la même coordonnée.
* Un déplacement à gauche puis en haut revient à aller à la coordonnée d'en haut à gauche.
* Un déplacement à droite puis en bas revient à aller à la coordonnée d'en bas à droite.

Post-condition des fonctions :

* initCoord : la coordonnée instanciée respecte l'invariant.

### Environnement

Invariants du type *Environnement* :

* Chaque entité présente dans la liste des entités de l'environnement est présente dans au moins une case de l'environnement.
* Chaque entité présente dans une case de l'environnement est présente dans la liste de entités de l'environnement.

Pré-conditions des fonctions :

* makeEnvironnement : la hauteur et la largeur sont strictement positives.
* trouveIdEnvi : l'identifiant est positif.
* trouveIdSeq : l'identifiant est positif.
* trouveIdMap : l'identifiant est positif.
* appliqueIdSeq : l'identifiant est positif.

Post-conditions des fonctions :

* enleveEnvi : l'entité n'est plus dans l'environnement.
* deplaceDansEnvironnement : l'entité est dans la sequence corréspondante à ses nouvelles coordonnées.
* ajouteEntite : l'entité a été ajouté à l'environnement.





### Etat

Invariants du type *Etat* :

* l'invariant de son environnement est vérifié.
* l'invariant de son niveau est vérifié.
* le nombre total de lemmings est strictement supérieur à 0.

Pré-conditions des fonctions :

* ...

Post-conditions des fonctions :

* ...

### Lemmings

Invariant du type *Lemmings* :

* l'invariant de ses coordonnées est vérifié.


### Niveau

Invariants du type *Niveau* :

* un niveau a exactement une entrée et une sortie.
* les bords du niveau sont en métal.
* l'entrée se trouve au dessus d'une case vide et la sortie au dessus d'une case en métal.
* la hauteur et la largeur du niveau sont strictement positives.
* le niveau a au moins une case.


Pré-conditions des fonctions :

* supprimerCase : la case a détruire est de la terre.
* poserCase : la case est une case vide.
* activerMine : la case est une mine.
* desactiverMine : la case est une mine activée.
* bloquer : la case est vide.
* debloquer : la case bloquée

Post-conditions des fonctions :

* readNiveau : le niveau créé vérifie son invariant.
* supprimerCase : la case supprimée est une case Vide.
* poserCase : la case est une case de terre.
* activerMine : la case est une mine activée.
* desactiverMine : la case est une case de terre.
* exploserCase : les 8 cases autours et la case elle même ne sont pas des cases de terre.
* bloquer : la case est bloquée
* debloquer : la case debloquée













