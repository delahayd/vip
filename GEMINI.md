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
Évolutions majeures intégrées pour la compétition CASC :
* **Résolution ordonnée & Superposition ordonnée**
* **Literal Selection :** Sélection stricte des littéraux maximaux (ordre KBO) pour les clauses positives, et sélection du premier littéral négatif pour les autres, réduisant drastiquement le facteur de branchement.
* **Feature Vector Indexing (FVI) :** Indexation avancée basée sur un Trie Creux pour un filtrage en $O(1)$ des candidats à la subsomption (Forward & Backward).
* **Backward Demodulation :** Simplification rétroactive des clauses actives par les nouvelles égalités unitaires.
* **Subsumption Resolution :** Règle de simplification agressive (ex: $A \lor C$ et $\neg A \lor D \rightarrow D$).
* **Portfolio Strategy (CASC Mode) :** Orchestration à deux étages. 
  - *Stage 1 (Flash)* : 2.0s avec ratio 10:1 (poids/âge), sans simplifications lourdes pour résoudre instantanément les problèmes simples.
  - *Stage 2 (Deep)* : Reste du temps avec ratio 4:1 et toutes les optimisations activées pour l'exploration profonde.

### `lib/match.ml` & `lib/clause.ml`
* Implémentation d'un matching du premier ordre (Sound & Complete).
* Subsomption mathématiquement correcte (1-à-1 glouton, sans multiset complet pour des raisons de vitesse).

### `src/main.ml` & `lib/prover.ml`
CLI du prouveur.
Fonctionnalités actuelles : `--time-limit`, `--max-clauses`, `--mode`, `--proof`, `--version`, `--duke`, `--tptp`, `--sos`, `--no-sos`.
* **Note sur le SOS :** Activé par défaut, désactivable via `--no-sos` (place toutes les clauses dans le *Passive set* initialement).

## Tests Unitaires
Suite de tests exhaustive (`tests/unit/`) utilisant Alcotest :
* `test_unify.ml`, `test_parser.ml`, `test_clausify.ml`
* `test_match_subsume.ml` : Valide le matching profond, la subsomption, la consistance des variables et la Subsumption Resolution.
* `test_fvi.ml` : Valide l'insertion, le filtrage et la suppression sécurisée dans l'index.
* `test_resolution.ml` & `test_ordering.ml` : Valide la résolution binaire, l'égalité (paramodulation basique), le factoring et l'ordre KBO.

## Benchmarks et Outils

### `bench/run_bench.ml`
Outil de benchmark massif. Supporte : `ip`, `Vampire`, `E`, `Zenon`.
Options : `--dir`, `--time-limit`, `--robust-time-limit`, `--max-clauses`, `--mode`, `--onlyip`, `--casc`, `--home`, `--logs`, `--size-limit`.

### `bench/check_regression.ml`
Compare deux CSV benchmark pour détecter les régressions et les nouveaux succès. Produit du HTML.

### BENCHS-MESO
Script SLURM pour lancement sur cluster.
* **Comportement de `--local` :** Utilise `./ip` localement, aucun `git clone`. 

---

## Contexte de la session en cours et Bilan

### État Actuel du Moteur
* Le moteur a été **sécurisé mathématiquement** (Soundness). L'ancienne version supprimait des clauses par erreur (bug de matching), ce qui trichait sur la taille de l'espace de recherche.
* **Gains :** ~46 problèmes très difficiles prouvés que l'ancienne version ratait.
* **Régressions :** ~266 problèmes simples/moyens en timeout. 
* **Diagnostic :** La rigueur mathématique empêche de tailler l'arbre au hasard. Sur les problèmes FOF (First-Order Formulas), l'explosion combinatoire est inévitable sans *Clause Splitting*. Le mode "Flash" du Portfolio a permis d'atténuer le problème, mais le mur combinatoire reste présent sur les clauses disjointes (ex: $A(x) \lor B(y)$).

### L'Étape Manquante (Ce qu'il reste à implémenter)
Pour dépasser les performances de Vampire sur les problèmes SYN et annuler les régressions, la **seule** brique architecturale manquante est :
1. **Le Clause Splitting (Architecture AVATAR) :** Casser les clauses sans variables communes en sous-problèmes gérés par un solveur SAT. (Déjà en cours de développement sur une autre branche).

### Objectif à Moyen Terme (CASC)
* Fusionner le moteur de base actuel (FVI, Match, Portfolio, Subsumption Resolution) avec la branche du **Splitting**.
* Implémenter le filtrage **Sine Level** pour ignorer les axiomes hors-sujet.
* Continuer de peaufiner les stratégies du **Portfolio**.