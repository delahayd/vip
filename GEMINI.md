# Contexte du projet : prouveur automatique `ip`

## Objectif Général
Développement d’un prouveur automatique du premier ordre en OCaml appelé `ip`.
Il est basé sur les concepts suivants :
* Résolution
* Factoring
* Superposition ordonnée
* Indexation de termes
* Parsing TPTP
* Support CNF + FOF
* Génération de preuves
* Benchmarking massif contre Vampire / E / Zenon

Le projet utilise :
* dune
* ocamllex / menhir
* TPTP
* OCaml

## Architecture Actuelle

### `src/resolution.ml`
Cœur du moteur de preuve.
Évolutions importantes déjà intégrées :
* Résolution ordonnée
* Factoring
* Superposition ordonnée
* Sélection de littéraux
* Simplification $true/$false
* Démultiplication / réécriture
* Indexation de termes
* Boucle de saturation Active/Passive
* Stratégie given-clause
* Sélection age/weight

Direction actuelle : Given-clause saturation loop + age/weight clause selection + simplification inter-reduction.

### `src/term_index.ml`
Indexation des termes et égalités.
Types importants : `term_entry` et `equality_entry`.
Utilisé pour :
* Résolution indexée
* Superposition indexée
* Récupération de sous-termes

### `src/discrimination_index.ml`
Index discrimination tree.
A servi à accélérer :
* Retrieval de termes
* Matching
* Unification candidates

**RÈGLE ABSOLUE :** Les constructeurs OCaml doivent commencer par une majuscule.

### `src/main.ml`
CLI du prouveur.
Fonctionnalités actuelles : `--time-limit`, `--max-clauses`, `--mode`, `--proof`, `--version`, `--duke`.

## Versionnage
Le numéro de version est `e - (1/n)`, calculé via la formule `version_of_n n = exp 1.0 -. (1.0 /. float_of_int n)`.
Le header ASCII est intégré dans `Header.header`.

## Benchmarks

### `bench/run_bench.ml`
Outil de benchmark massif. Supporte : `ip`, `Vampire`, `E`, `Zenon`.
Options : `--dir`, `--time-limit`, `--robust-time-limit`, `--max-clauses`, `--mode`, `--onlyip`, `--casc`, `--home`, `--logs`, `--size-limit`.

### Fonctionnalités de benchmark
* **Génération :** CSV, HTML.
* **Statistiques par prouveur :** succès, timeouts, incomplétude, incorrection, erreurs, temps moyen, uniques.
* **Comparaisons :** win/draw/loss.

### Définition des succès
Pour les problèmes `Satisfiable` et `CounterSatisfiable`, les résultats suivants comptent comme succès : `Satisfiable`, `CounterSatisfiable`, `Timeout`, `GaveUp`.

### Hyperliens (HTML)
* Les problèmes sont affichés sans le préfixe `--dir`.
* `--home` permet de reconstruire le chemin réel.
* **RÈGLE ABSOLUE :** Quand `--home` n’est pas fourni, `link_path` doit utiliser le chemin absolu basé sur `--dir`.

## Outils Tiers & Paramètres Spécifiques

### Zenon
* Commande correcte : `zenon -I $TPTP -itptp -max-time %d %s`.
* **RÈGLE ABSOLUE :** Si `$TPTP` est vide, ne pas mettre `-I`.

### Timeout Robuste
Fonction correcte :
`let robust_timeout_seconds time_limit = int_of_float (ceil (float_of_int time_limit *. 1.50))`

## Scripts et Utilitaires

### BENCHS-MESO
Script SLURM.
* **Fonctions :** clone Git automatique, lancement `make bench`, génération SLURM, mail de fin de job.
* **Options :** `--time-limit`, `--robust-time-limit`, `--max-clauses`, `--mode`, `--dir`, `--home`, `--logs`, `--casc`, `--tptp`, `--onlyip`, `--local`.
* **Comportement de `--local` :** Utilise `./ip` localement, aucun `git clone`. Le script crée la commande `ln -s "$LOCAL_IP_DIR" ip`.

### `check_regression.ml`
Compare deux CSV benchmark.
* **Détecte :** régressions, nouveaux problèmes prouvés.
* **Produit :** HTML.
* **Options :** `--home` pour les hyperliens.

---

## Contexte de la session en cours

### État Actuel du Moteur
* **Gains :** +24 problèmes SYN.
* **Régressions :** ~210 régressions.
* **Causes probables :** backward subsumption trop agressive, simplification excessive, stratégie age/weight trop orientée poids.

### Direction Recommandée
**Étape immédiate :**
* Conserver : Active/Passive, given-clause, age/weight.
* Désactiver temporairement : backward subsumption, forward subsumption sur passive.

**Stratégie future :**
1. given-clause robuste
2. simplification sûre
3. backward simplification
4. indexation complète
5. fallback strategy

### Philosophie Actuelle
Ne pas utiliser un SOS strict.
Préférer : given-clause globale + priorité conjecture via pondération.

### Objectif à Moyen Terme
Se rapprocher de `E` et `Vampire` en ajoutant :
* boucle given-clause mature
* simplification inter-réduction
* sélection sophistiquée
* saturation contrôlée
* superposition fortement indexée
* bonnes heuristiques de clause selection
