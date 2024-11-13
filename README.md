# Une inférence de types du OCaml, en OCaml

## Notations

Le langage reconnu est défini par:
```
e ::= n | i | b
    | if e1 then e2 else e3
    | fun x -> e
    | e1 e2

n ::= x | bop

bop ::= ( + ) | ( * ) | ( <= )

t ::= int | bool | t1 -> t2
```

Dans le cas général:
- On note $\Gamma$ un environnement, c'est-à-dire une liste de couples $(x, \tau)$ où $x$ est une variable et $\tau$ son type.
- On note $C$ un ensemble quelquonque de contraintes.
- On note $\Gamma\vdash e:\tau\dashv C$ une dérivation de typage, c'est-à-dire une preuve que l'expression $e$ a le type $\tau$ dans l'environnement $\Gamma$ et génère les contraintes $C$.
- On note $S$ un ensemble de substitutions, c'est-à-dire une liste de couples $(\alpha, \tau)$ où $\alpha$ est une variable de type et $\tau$ est un type. On note donc $e\space S$ le résultat de l'application de la substitution $S$ à l'expression $e$, et $\tau\space S$ le résultat de l'application de la substitution $S$ au type $\tau$.

## Règles de typage

#### Constantes

$$
\frac{}{\Gamma\vdash i:\text{int}\dashv\emptyset}
$$

$$
\frac{}{\Gamma\vdash b:\text{bool}\dashv\emptyset}
$$

#### Variables

$$
\frac{(x, \tau)\in\Gamma}{\Gamma\vdash x:\tau\dashv\emptyset}
$$

#### Fonctions

$$
\frac{\Gamma, x:\tau_1\vdash e:\tau_2\dashv C}{\Gamma\vdash \text{fun }x\to e:\tau_1\to\tau_2\dashv C}
$$

#### Applications

$$
\frac{\Gamma\vdash e_1:\tau_1\dashv C_1\quad\Gamma\vdash e_2:\tau_2\dashv C_2}{\Gamma\vdash e_1\space e_2:\tau\dashv C_1\cup C_2\cup\{\tau_1=\tau_2\to\tau\}}
$$

#### Conditions

$$
\frac{\Gamma\vdash e_1:\tau_1\dashv C_1\quad\Gamma\vdash e_2:\tau_2\dashv C_2\quad\Gamma\vdash e_3:\tau_3\dashv C_3}{\Gamma\vdash \text{if }e_1\text{ then }e_2\text{ else }e_3:\tau\dashv C_1\cup C_2\cup C_3\cup\{\tau_1=\text{bool}, \tau=\tau_2,\tau=\tau_3\}}
$$

## Let-*polymorphisme*

Pour implémenter le polymorphisme, on ajoute un type possible à notre langage:
```
t ::= int | bool | t1 -> t2 | forall a. t
```

Ainsi que la notation $\forall a. t$ pour signifier que $t$ est polymorphe sur le type $a$.

Voici les règles de typage supplémentaires pour le let-polymorphisme:

#### Let

$$
\frac{\Gamma\vdash e_1:\tau_1\dashv C_1\quad\text{generalize}(\Gamma, C_1, x, \tau_1)\vdash e_2:\tau_2\dashv C_2}{\Gamma\vdash \text{let }x=e_1\text{ in }e_2:\tau\dashv C_1\cup C_2}
$$

#### Variables

$$
\frac{(x, \tau)\in\Gamma}{\Gamma\vdash x:\text{instantiate}(\tau)\dashv\emptyset}
$$

### Définitions de `generalize` et `instantiate`

On notera $V(e)$ l'ensemble des variables présentes dans l'expression $e$, et $V(\Gamma)$ l'ensemble des variables présentes dans l'environnement $\Gamma$.

#### `generalize`

#### `instantiate`


#### Usefull links

- https://okmij.org/ftp/ML/generalization.html
- https://github.com/ocaml/ocaml/blob/ad2aecfab0a425ffbaa01dc22a5661adcbe88710/parsing/parsetree.mli