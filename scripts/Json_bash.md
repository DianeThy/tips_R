# Lignes de commande pour manipuler des fichiers JSON depuis le terminal

### Changer le wd :
cd ~/Desktop/Étude\ des\ APC/etude_APC/data/1.raw/BSO/jsonl

### Passer d’un jsonl à un json, garder 3 lignes : 
head -n 3 data/raw/study-apc_2020.jsonl | jq -c '{doi, year, journal_name, journal_title, journal_issn_l, publisher, has_apc, amount_apc}' |jq --slurp |pygmentize -l json

### Unnest variable : 
head -n 3 data/raw/study-apc_2020.jsonl | jq -c '{doi, year, journal_name, journal_title, journal_issn_l, publisher, has_apc, amount_apc, author: .authors[]}' |jq --slurp |pygmentize -l json

### Voir les occurrences des valeurs d’1 colonne : 
jq '.year’ study-apc_2013.jsonl |sort |uniq -c
le “-c” c’est pour qu’il donne le nombre d’occurrences associées à telle valeur unique

### Liste des noms de variables :
jq -rc 'keys[]' ~/Downloads/jsonl/study-apc_2013.jsonl |sort |uniq -c > ~/Downloads/columns/colnames_13.txt

### Occurences des valeurs d'une variable (less : naviguer entre les valeurs + occurrences de la col) : 
jq -c '.coi' study-apc_2020.jsonl |sort |uniq -c |sort -nr |less

### Occurence des non NA d'une variable
jq -c study-apc_2013.jsonl | map(select(.amount_apc_EUR == null)) | length

### Regarder une entrée en particulier
grep "10.3389/fpls.2013.00038" study-apc_2013.jsonl |python -m json.tool

### Longueur objet après sélection ‘doi, year, authors’ et passe en JSON :
 jq length 2.interim/subjson_BSO/authors_bso_2013.json

### Nombre d’auteurs sans les 1060 NA : 
grep -v '"authors": NaN' study-apc_2013.jsonl | jq -c '.authors[]' |wc -l  

### Nombre de lignes fichier JSON
wc -l data/2.interim/subjson_BSO/authors_bso_2013.jsonl
