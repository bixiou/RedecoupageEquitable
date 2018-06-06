# RedecoupageEquitable
Redécoupage automatique des circonscriptions françaises à partir de critères objectif : égalité de population, contiguïté, compacité

Le fichier principal est 'districting.ipynb', il appelle 'districting_functions.py'.

Les résultats s'ouvrent avec un logiciel tels que QGis (ouvrir les .shp). Le nom de fichier décrit l'unité de base qui a servi a créé la carte (iris ou cantons) et le ratio de populations entre les circonscriptions la plus et la moins peuplée (ex: ratio de 1.5 calculé avec des IRIS: 'decoupage_iris1-5.shp').

La préparation des données est effectuée dans redécoupage.R. 
Dans ce fichier, est aussi décrite l'approche initiale, qui utilisait le logiciel autoredistrict. Le découpage créé par ce logiciel n'était pas continu, donc nous avons abandonné cette approche au profit du code de Guy Lifshitz. Pour faire des cartes contiguës avec autoredistrict, il faudrait tenter ce qui est décrit dans ce ticket : https://github.com/happyjack27/autoredistrict/issues/204 

Si vous voulez juste les shapefiles dont le code a besoin pour tourner, ils sont dans Data/ pour ce qui concerne la métropole, et dans Annexes/IRIS/ et Annexes/cantons/ pour les cas spéciaux (outre-mer, Français de l'étranger, Paris, Lyon).

Ces fichiers, et bien d'autres, sont dans le dépôt https://github.com/bixiou/RedecoupageEquitable