# Contexte courant du projet VIP

Ce fichier sert de point de reprise pour les agents qui travaillent sur le
prouveur automatique VIP. Il doit rester factuel : scores mesures,
branches/commits importants, commandes utiles, risques connus et priorites.

## Objectif CASC

Le but court terme est de livrer une version CASC FOF fonctionnelle :

- prouveur automatique premier ordre en OCaml ;
- sortie SZS/TSTP conforme ;
- packaging StarExec executable ;
- comportement reproductible, sans specialisation par chemin de fichier ;
- meilleur score possible sur FOF, en particulier FEQ/FNE, sans sacrifier la
  validite des preuves.

Objectif de performance vise : continuer a progresser au-dela de la meilleure
baseline locale mesuree a `125 / 500` sur l'ancien set CASC FOF.

## Branches et commits importants

- `casc-stable-5738a77` :
  ancienne branche stable dediee au score `118 / 500`.
- `5738a77 Recover FEQ narrow SInE cases in CASC 240` :
  meilleure baseline de performance mesuree sur CASC FOF 120s.
- `recover-regressions-df93698` / branche experimentale courante :
  base recente autour de `8133361`, score observe `125 / 500` en mode `casc-150`.
- `df93698 Add MESO memory options to CASC experiment` :
  ajoute `--mem` / `--mem-per-cpu` au script MESO.
- `f806630 Add StarExec run script` :
  ajoute le script d'execution StarExec.
- `17f2317 Improve CASC TSTP proof verification` :
  ameliore le preambule TSTP et les sources de clauses.
- `ccc6960 Report Theorem status for conjecture refutations` :
  corrige le statut SZS : une refutation d'un probleme avec conjecture doit
  sortir `Theorem`, pas seulement `Unsatisfiable`.
- `c8bcf2b Make CASC TSTP proofs GDV-verifiable` :
  dernier commit de preuve ; panel local de preuves valide par `tptp4X` et GDV.

Ne pas ecraser la branche stable avec des experiences. Toute nouvelle idee doit
aller sur une branche experimentale ou un mode portfolio separe.

## Baseline de performance

Meilleure baseline actuelle mesuree :

- commit : `8133361`
- mode : `casc-150`
- benchmark : CASC FOF, 120s, 500 problemes, max clauses 75000
- score : `solved = 125 / 500`
- comparaison avec `5738a77` : 2 regressions, 5 nouveaux problemes prouves
- regressions observees : `FEQ/CSR069+4.p`, `FNE/CSR062+3.p`

Cette baseline est fragile mais importante. Plusieurs versions recentes ont
regresse :

- `efcfb4d` : `97 / 500`, soit 21 regressions et 0 nouveau probleme par
  rapport a `5738a77`.
- `9fa2cd5`, `dd3d89b` : environ `97 / 500`.
- certains runs 32G ont fini en OOM ; les runs 64G passent mieux mais n'ont pas
  automatiquement ameliore le score.

Conclusion : ne jamais remplacer directement une strategie stable. Ajouter un
stage court, mesurer, puis integrer seulement si le gain net est confirme.

## Etat actuel du moteur

Le moteur contient actuellement :

- parsing TPTP CNF/FOF avec includes via `TPTP` ;
- clausification FOF basique ;
- resolution ;
- factoring ;
- superposition / paramodulation ordonnee ;
- demodulation ;
- condensation rapide et complete ;
- subsomption forward/backward ;
- subsumption resolution ;
- indexation de clauses et de termes ;
- selection de litteraux ;
- selection passive avec plusieurs modes ;
- selection d'axiomes de type SInE/ranked ;
- portfolio dedie CASC (`casc-240`, `casc-150`, `feq-modern`,
  `experimental-casc`, etc.) ;
- AVATAR / splitting experimental ;
- options DMT / one-way definitions experimentales ;
- sortie SZS/TSTP ;
- script StarExec ;
- script MESO avec options memoire.

Les chemins vers les provers externes consultes localement :

- Vampire : `/home/etudiant/Cours/stage/Vampire/vampire-5.0.1`
- Zipperposition : `/home/etudiant/Cours/stage/zipperposition-master`
- StarExec source : `/home/etudiant/Cours/stage/StarExec-fb1`
- TPTP local : `/home/etudiant/Cours/stage/TPTP-v9.2.1`
- GDV : `/home/etudiant/Cours/stage/GDV/GDV`
- CASC local : `/home/etudiant/Cours/stage/casc`

## Preuves CASC

Etat le plus recent : les preuves TSTP sont beaucoup plus propres.

Corrections recentes :

- sortie `% SZS status Theorem` pour les problemes avec conjecture ;
- preservation du preambule TSTP quand une sous-strategie du portfolio trouve
  la preuve ;
- provenance `file(...)` pour les formules source ;
- feuilles FOF/CNF choisies pour mieux passer GDV ;
- clauses initiales condensees imprimees comme :
  `source FOF -> clause CNF brute -> condensation -> clause utilisee` ;
- les simplifications de clause selectionnee sont maintenant tracees comme
  etapes de preuve au lieu d'ecraser silencieusement la clause.

Panel de validation local du commit `c8bcf2b` :

- `FEQ/COM125+1.p`
- `FEQ/COM126+1.p`
- `FEQ/COM142+1.p`
- `FEQ/GEO500+1.p`
- `FEQ/ITP003+1.p`
- `FEQ/SET144+3.p`
- `FNE/KRS189+1.p`
- `FNE/KRS233+1.p`
- `FNE/KRS258+1.p`

Resultat du panel :

- statut solveur : `Theorem` pour les 9 ;
- `tptp4X = 0` pour les 9 ;
- `GDV = VerifiedGood` pour les 9.

Dernier dossier de preuves du panel :

```bash
/tmp/vip_gdv_panel_final2_20260626_124959
```

Commande type pour tester une preuve :

```bash
ROOT=/home/etudiant/Cours/stage/casc
VIPBIN=/home/etudiant/Cours/stage/ip/_build/default/src/main.exe
OUT=/tmp/vip-proof.out

"$VIPBIN" \
  --competition-output \
  --proof-tstp \
  --portfolio casc-240 \
  --time-limit 95 \
  --max-clauses 75000 \
  --tptp "$ROOT" \
  "$ROOT/FOF/FEQ/COM142+1.p" > "$OUT"

/home/etudiant/Cours/stage/TPTP-v9.2.1/Scripts/tptp4X -q1 "$OUT"
TPTP="$ROOT" /home/etudiant/Cours/stage/GDV/GDV \
  -q2 -d -u -p "$ROOT/FOF/FEQ/COM142+1.p" "$OUT"
```

Point important : pour GDV, il faut positionner `TPTP` vers le dossier racine
qui contient `Axioms/`, sinon les includes relatifs ne sont pas resolus.

## StarExec et CASC

Le script StarExec actuel se trouve dans :

```bash
starexec/starexec_run_default
```

Il doit etre utilise comme base pour produire le script attendu par CASC,
typiquement `bin/starexec_run_FOF` dans le `.tgz` executable.

Contraintes CASC importantes :

- stdout uniquement ;
- sortie SZS obligatoire ;
- preuve TPTP obligatoire en FOF ;
- pas de specialisation par chemin de fichier, nom de probleme ou solution ;
- includes TPTP resolus soit depuis le dossier du probleme, soit depuis
  l'environnement `TPTP` ;
- systeme interruptible par `SIGXCPU` / `SIGALRM` ;
- pas de fichiers temporaires hors `/tmp` en cas d'interruption.

Il faudra fournir :

- un `.tgz` executable StarExec contenant uniquement le necessaire pour lancer ;
- un `.tgz` source contenant le code et ce qu'il faut pour reconstruire.

## MESO

Script :

```bash
meso/benchs-meso
```

Options utiles :

- `--local` : utilise le repo local via symlink, ne clone pas ;
- `--dir` : dossier de problemes ;
- `--tptp` : racine TPTP / CASC pour les includes ;
- `--time-limit` ;
- `--robust-time-limit` ;
- `--portfolio` ;
- `--max-clauses` ;
- `--mem` / `--mem-per-cpu` sur les branches qui contiennent `df93698`.

Commande CASC type :

```bash
cd ~/benchs-vip/vip && \
git fetch && \
git checkout casc-stable-150-exp && \
git pull && \
cd ~/benchs-vip && \
./vip/meso/benchs-meso \
  --local \
  --dir ~/benchs-vip/casc/FOF/ \
  --tptp ~/benchs-vip/casc \
  --time-limit 120 \
  --robust-time-limit \
  --onlyvip \
  --portfolio casc-240 \
  --max-clauses 50000 \
  --mem 64G
```

Pour verifier les jobs :

```bash
sacct -u nokranii --starttime YYYY-MM-DD \
  --format=JobID,JobName,State,ExitCode,Elapsed,MaxRSS,ReqMem,Start,End
```

Pour lire les derniers logs uniques :

```bash
cd ~/benchs-vip/vip && \
for CSV in $(ls -t ~/benchs-vip/vip_bench_*/vip/bench/logs/bench_*.csv \
  | awk '{split($0,a,"/"); n=a[length(a)]; if(!seen[n]++){print $0}}' \
  | head -10); do
  echo "==== $CSV"
  grep -E '^# bench_date|^# problem_dir|^# time_limit_seconds|^# max_clauses|^# portfolio|^# version_vip' "$CSV"
  echo -n "results="
  grep -c '^result,' "$CSV"
  echo -n "solved="
  grep '^result,' "$CSV" | grep -E '","(Theorem|Unsatisfiable)",' | wc -l
done
```

## Logs de reference

Sur MESO, les logs importants ont ete regroupes dans :

```bash
~/benchs-ip/logs_reference/
```

Fichiers de reference connus :

- `baseline_may_2026-05-15.csv`
- `syn_stable_2026-06-09_10-03-10.csv`
- `casc_30s_ref_2026-06-09_11-47-00.csv`
- `casc_120s_ref_2026-06-09_16-25-54.csv`
- `casc_120s_avatar_2a832aa_2026-06-10_16-26-09.csv`
- `syn_avatar_2a832aa_2026-06-10_15-55-22.csv`

Le meilleur benchmark `5738a77` a aussi ete conserve comme reference de fait :

```bash
/home/nokranii/benchs-ip/ip_bench_20260611_102643/ip/bench/logs/bench_2026-06-23_11-02-54.csv
```

Il vaut mieux le copier explicitement dans `logs_reference` si ce n'est pas deja
fait.

Commande de regression CASC contre l'ancienne reference 120s :

```bash
cd ~/benchs-vip/vip && \
BASE=~/benchs-ip/logs_reference/casc_120s_ref_2026-06-09_16-25-54.csv && \
CSV=/chemin/vers/nouveau/bench.csv && \
dune exec -- bench/check_regression.exe "$BASE" "$CSV"
```

Commande de regression contre le meilleur benchmark `5738a77` :

```bash
cd ~/benchs-vip/vip && \
BASE=/home/nokranii/benchs-ip/ip_bench_20260611_102643/ip/bench/logs/bench_2026-06-23_11-02-54.csv && \
NEW=/chemin/vers/nouveau/bench.csv && \
dune exec -- bench/check_regression.exe "$BASE" "$NEW"
```

## Priorites actuelles

1. Stabiliser le packaging StarExec executable.
2. Tester le script StarExec sur quelques problemes avec includes.
3. Garder `8133361` / `casc-150` comme baseline de performance actuelle, et conserver `5738a77` comme reference historique stable.
4. Relancer une version stable avec `--mem 64G` ou plus, sans changer le moteur,
   pour verifier que le score reste coherent sans OOM.
5. Reprendre les gains FEQ/FNE de facon prudente :
   - stages courts ;
   - flags desactives par defaut ;
   - comparaison stricte contre `8133361` et contre `5738a77`.
6. Controler la memoire :
   - eviter les stages qui accumulent trop de clauses ;
   - surveiller `MaxRSS` ;
   - ne pas conclure depuis des runs OOM partiels.
7. Etendre le panel de validation TSTP/GDV sur plus de problemes prouves.

## Risques connus

- Le score `118 / 500` est fragile : de petits changements de portfolio peuvent
  perdre 20 problemes.
- Les runs partiels peuvent etre trompeurs si le job finit en OOM apres avoir
  quand meme produit un CSV/HTML.
- Les preuves peuvent etre syntaxiquement valides mais structurellement
  refusees par GDV si les feuilles ne sont pas rattachees proprement au probleme.
- Les chemins des fichiers seront anonymises au vrai CASC ; aucune strategie ne
  doit dependre de `FEQ/`, `FNE/`, `SYN`, du nom de fichier ou du chemin.
- Les headers TPTP seront supprimes/obfusques ; ne pas compter sur eux.
- Le vrai CASC impose 128 GiB de memoire d'apres les regles CASC-30 consultees.

## A dire a l'encadrant

Etat actuel :

- une meilleure baseline mesuree a `125 / 500` sur l'ancien CASC FOF ;
- strategies separees FEQ/FNE via portfolio CASC ;
- selection d'axiomes type SInE/ranked ;
- plusieurs optimisations de saturation et simplification ;
- support MESO avec memoire configuree ;
- sortie SZS/TSTP ;
- panel local de preuves maintenant valide par `tptp4X` et GDV ;
- script StarExec en preparation.

Le verrou principal n'est plus seulement de faire tourner le prouveur, mais de
livrer un systeme CASC complet :

- performance stable ;
- preuves acceptables ;
- packaging StarExec ;
- controle memoire ;
- absence de tuning specifique aux anciens problemes.

Objectif realiste : conserver les 125 problemes, consolider le packaging et les
preuves, puis chercher des gains prudents vers 130-150 via FEQ/FNE sans casser
la baseline.


## Renommage VIP

Le projet s'appelle maintenant VIP (Vibe Prover). Les nouveaux scripts, logs et sorties doivent utiliser `vip`. Les anciens alias `ip`, `--onlyip` et les variables `IP_*` restent acceptes pour compatibilite avec les anciens benches et CSV. Les chemins historiques sous `~/benchs-ip` ou `ip_bench_*` peuvent encore apparaitre uniquement lorsqu'on relit d'anciens resultats.
