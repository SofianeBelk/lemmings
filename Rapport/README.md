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
