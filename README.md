# ASCO : Compilateur C-minus

Ce projet est un compilateur expérimental développé en OCaml pour un sous-ensemble du langage C (souvent appelé C-minus). Il effectue l'analyse lexicale, syntaxique et sémantique de code source C-minus.

## Fonctionnalités Principales

- **Analyse Lexicale & Syntaxique** : Utilisation de `ocamllex` et `ocamlyacc` pour définir la grammaire (voir `lexer.mll` et `parser.mly`). Le parseur supporte les déclarations de variables, les fonctions, les boucles (`while`, `for`, `do while`), les conditions (`if`/`else`) et diverses opérations arithmétiques.
- **Construction d'AST** : Génération d'un Arbre Syntaxique Abstrait complet modélisant le programme (déclarations globales, arguments de fonctions, portée des blocs) via `ast.ml`.
- **Analyse Sémantique** :
  - **Vérification de portée (Scope checking)** : Vérifie que toutes les variables et fonctions appelées ont bien été déclarées dans l'environnement courant (`check_scope.ml`).
  - **Vérification de types (Type checking)** : S'assure de la cohérence des types (entiers, flottants, pointeurs, void) lors des affectations, opérations et appels de fonctions (`check_types.ml`).

## Technologies

- **OCaml**
- **ocamllex** (Lexer)
- **ocamlyacc** (Parser / Grammaire)

## Utilisation

Le point d'entrée du programme est `main.ml`. Pour compiler et exécuter sur un fichier source :

```bash
make
./asco <fichier.cmoins>
```

Le programme affichera soit l'AST (en cas de succès), soit une erreur de syntaxe, de type ou de portée si le code lu est invalide.
