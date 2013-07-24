Installation
------------

1. Install the CUDD library to the directory ./cudd-2.4.2
2. Call "make"


Usage
-----
psl2ba --help

Examples:
./psl2ba -aba 'a U b'
./psl2ba -smv '{{a | b*} && {c & d}*} <>-> c S d'

You can find more examples in ./test/README.TXT


Syntax for an RTL formula
------------------------
The syntax of an RTL formula is given in lexer.mll.

The strings "tt" and "ff" are interpreted as true and false, respectively.

A SERE has the form
  r ::= tt | ff | p | !r | r* | r+ | r||r | r&&r | r;r | {r}
where p is a proposition.

An RTL formula has the form
  f ::= tt | ff | p | !f | f|f | f&f | Xf | Yf | Zf
        | fUf | fWf | fSf | fRf | fVf | fTf
        | {r}<>->f | {r}<->->f | {r}[]->f | {r}[-]->f | Cl{r}
        | Ff | Of | Gf | Hf | f->f | f<->f | (f)
where p is a proposition and r is a SERE.
