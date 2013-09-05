We use the old NuSMV model checker, version 2.1.0.0. This version calls the ltl2smv executable to obtain the automaton of an LTLSPEC.
To check for correctness of the translator, we additionally use the current NuSMV model checker, version 2.4.0.0.

./NuSMV should be executable
../psl2ba should be executable

./OldSMV should be executable
./ltl2smv should be executable when OldSMV is used


Some test cases:
----------------------
../psl2ba -subeq 'a U b' 'a U b' | ./NuSMV
../psl2ba -supeq 'a U b' 'a U b' | ./NuSMV

../psl2ba -subeq 'G(grant -> O request)' 'G(grant -> O request)' | ./NuSMV
../psl2ba -supeq 'G(grant -> O request)' 'G(grant -> O request)' | ./NuSMV

../psl2ba -subeq '{tt*} <>-> a' 'F a' | ./NuSmv
../psl2ba -supeq '{tt*} <>-> a' 'F a' | ./NuSmv

../psl2ba -subeq '{tt*} []-> a' 'G a' | ./NuSmv
../psl2ba -supeq '{tt*} []-> a' 'G a' | ./NuSmv

../psl2ba -subeq '{{! request}*} []-> !grant' 'G(grant -> O(request))' | ./NuSMV
../psl2ba -supeq '{{! request}*} []-> !grant' 'G(grant -> O(request))' | ./NuSMV

../psl2ba -subeq '{a;b} []-> c' 'a & X b -> X c' | ./NuSMV
../psl2ba -supeq '{a;b} []-> c' 'a & X b -> X c' | ./NuSMV

../psl2ba -subeq '{a || b} <>-> c' '(a | b) & c' | ./NuSMV
../psl2ba -supeq '{a || b} <>-> c' '(a | b) & c' | ./NuSMV

../psl2ba -subeq '{a && b} <>-> c' '(a & b) & c' | ./NuSMV
../psl2ba -supeq '{a && b} <>-> c' '(a & b) & c' | ./NuSMV

../psl2ba -subeq '{{!start}* || {{!start}*;start & !end;{!end}*}} []-> !grant' 'G(grant -> O(end & O start))' | ./NuSMV
../psl2ba -supeq '{{!start}* || {{!start}*;start & !end;{!end}*}} []-> !grant' 'G(grant -> O(end & O start))' | ./NuSMV

../psl2ba -subeq 'G(grant -> O(Y({start;end} <>-> tt)))' 'G(grant -> O(end & Y start))' | ./NuSMV
../psl2ba -supeq 'G(grant -> O(Y({start;end} <>-> tt)))' 'G(grant -> O(end & Y start))' | ./NuSMV

../psl2ba -subeq 'G(grant -> O(Y Y({start; tt; end} <>-> tt)))' 'G(grant -> O(end & Y Y start))' | ./NuSMV
../psl2ba -supeq 'G(grant -> O(Y Y({start; tt; end} <>-> tt)))' 'G(grant -> O(end & Y Y start))' | ./NuSMV

../psl2ba -subeq '{tt*;b} <>-> tt' 'F b ' | ./NuSMV
../psl2ba -supeq '{tt*;b} <>-> tt' 'F b ' | ./NuSMV

../psl2ba -subeq '{a} <>-> b' 'a & b' | ./NuSMV
../psl2ba -supeq '{a} <>-> b' 'a & b' | ./NuSMV

../psl2ba -subeq '{tt*;b} <>-> a' 'F (b & a)' | ./NuSMV
../psl2ba -supeq '{tt*;b} <>-> a' 'F (b & a)' | ./NuSMV

../psl2ba -subeq 'G(grant -> {request; tt*} <->-> tt)' 'G(grant -> O(request))' | ./NuSMV
../psl2ba -supeq 'G(grant -> {request; tt*} <->-> tt)' 'G(grant -> O(request))' | ./NuSMV

../psl2ba -subeq 'G(grant -> {start; tt; tt*} <->-> tt)' 'G(grant -> O( Y start))' | ./NuSMV
../psl2ba -supeq 'G(grant -> {start; tt; tt*} <->-> tt)' 'G(grant -> O( Y start))' | ./NuSMV



Formulas from our paper:
------------------------

"Example 1"
../psl2ba -phi 'G(grant -> ({{{start; tt*; end} && {!cancel}*}; tt*} <->-> tt))'
../psl2ba -subeq 'G(grant -> ({{{start; tt*; end} && {!cancel}*}; tt*} <->-> tt))' 'G(grant -> O(end & !cancel & Y(!cancel S (start & !cancel))))' | ./NuSMV
../psl2ba -supeq 'G(grant -> ({{{start; tt*; end} && {!cancel}*}; tt*} <->-> tt))' 'G(grant -> O(end & !cancel & Y(!cancel S (start & !cancel))))' | ./NuSMV


../psl2ba -supeq '{{(!start|cancel) || {(start&!cancel); {!cancel&!end}*; cancel}*}* ; {cancel* || (start&!cancel); {!cancel&!end}*}} []-> !grant' 'G(grant -> O (end & !cancel & Y (!cancel S (start & !cancel))))' | ./NuSMV
../psl2ba -subeq '{{(!start|cancel) || {(start&!cancel); {!cancel&!end}*; cancel}*}* ; {cancel* || (start&!cancel); {!cancel&!end}*}} []-> !grant' 'G(grant -> O (end & !cancel & Y (!cancel S (start & !cancel))))' | ./NuSMV

