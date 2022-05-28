# AlgorithmeMCMC

Projet réalisé en 2021.

Projet pour décrypter un texte à l'aide d'un algorithme MCMC.

Ce projet repose sur des statistiques et des probabiltés sur les bigrammes de lettre. En effet en important des livres anglais et français sous format de fichier texte et en comptant les occurences des bigrammes, on peut construire des matrices de transitions qui représente la probabilité d'apparition d'une lettre en fonction d'une autre.

On peut voir l'alphabet comme une permutation de taille 26. La permutation identité est défini comme cela 1->a,2->b....26->z.
Pour crypter un texte, on peut changer la permutation (changer l'ordre des lettres). Par exemple si on prend 2->a,1->b...26->z, ici on inverse les a par les b dans le texte à chiffrer.

Grace à cette application shiny, on peut résoudre les chiffrements par permutation. En effet grace à des chaines de Markov et des propriétés sur la stationnaritée d'une chaine on peut par les probabilités  retrouver la permutation inverse du chiffrement pour déchiffrer le texte.

Cette méthode est implémentée pour textes en anglais et français ici.










