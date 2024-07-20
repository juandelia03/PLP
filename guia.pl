% 1
padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis,pablo).
padre(luis,manuel).
padre(luis,ramiro).
abuelo(X,Y) :- padre(X,Z), padre(Z,Y).

hijo(Y,X) :- padre(X,Y).

hermano(X,Y) :- padre(Z,X),padre(Z,Y),X\=Y.

descendiente(X,Y) :- hijo(X,Y).
descendiente(X,Y):- hijo(X,Z),descendiente(Z,Y).


ancestro(X, X).
ancestro(X, Y) :- padre(Z, Y),ancestro(X, Z).


% ejercicio 3
% si ponemos las reglas al reves entra infinitamente a la segunda y nunca corta
natural(0).
natural(suc(X)) :- natural(X).
menorOIgual(X,X) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).


% ejercicio 4 (definir el append de prolog)
% juntar(?Lista1,?Lista2,?Lista3)
juntar([],L2,L2).
juntar([X|L1],L2,[X|L3]) :- juntar(L1,L2,L3).


% ejercicio 5 definir usando append

% i. last(?L, ?U), donde U es el último elemento de la lista L.
last(L,U) :- append(_,[U],L).
% ii. reverse(+L, -L1), donde L1 contiene los mismos elementos que L, pero en orden inverso.
reverse([],[]).
reverse([X|L],R1) :-  reverse(L,R2),append(R2,[X],R1).
% iii. prefijo(?P, +L), donde P es prefijo de la lista L.
prefijo(P,L) :- append(P,_,L).
% iv. sufijo(?S, +L), donde S es sufijo de la lista L.
sufijo(S,L) :- append(_,S,L).
% v. sublista(?S, +L), donde S es sublista de L.
%creo que dijeron en clase que es sublista sii es sufijo y pre
sublista([],L).
sublista(S,L) :-prefijo(P,L),sufijo(S,P),S\=[].
% vi. pertenece(?X, +L), que es verdadero sii el elemento X se encuentra en la lista L (member)
pertenece(X,L) :- append(L1,[X|L2],L).

% ejercicio 6 definir flatten
aplanar([],[]).
% caso el primel elto no es lista
%con X\=[],not(member(E,X)).chequeo q X no sea lista,
%despues digo que con [X] q X ya esta aplanado concatenado con Ys2 (que es Xs aplanado, se arma Ys)
aplanar([X|Xs],Ys) :- X\=[],not(member(E,X)),aplanar(Xs,Ys2),append([X],Ys2,Ys). 
%caso el primer elto si es lista
aplanar([X|Xs],Ys) :- aplanar(X,X2),aplanar(Xs,Xs2),append(X2,Xs2,Ys).

% ejercicio 7
% palíndromo(+L, ?L1)
palindromo(L,P) :- reverse(L,LR), append(L,LR,P).
% ii. iésimo(?I, +L, -X), donde X es el I-ésimo elemento de la lista L.
iesimo(I,L,X) :- append(L1,[X|L2],L), length(L1,I).

% ejercicio 8
% a)
% hay q ir vaciando L1 para meterlos en su orden
interseccion([],L2,[]).
%elem no pertenece a L1
interseccion([X|L1],L2,L3) :- not(member(X,L2)),interseccion(L1,L2,L3).
%elem en comun
interseccion([X|L1],L2,[X|L3]) :- member(X,L2),delete(L1,X,LD),interseccion(LD,L2,L3).

% b)
partir(N,L,L1,L2):- append(L1,L2,L),length(L1,N). 

% ii. borrar(+ListaOriginal, +X, -ListaSinXs),
borrar([],X,[]).
borrar([E|L],X,[E|Lf]) :- E\=X, borrar(L,X,Lf).
borrar([X|L],X,Lf) :- borrar(L,X,Lf).

% iii. sacarDuplicados(+L1, -L2), que saca todos los elementos duplicados de la lista L1.
sacarDuplicados([],[]).
%caso no esta repetida en L1
sacarDuplicados([X|L1],L2) :- member(X,L1), sacarDuplicados(L1,L2). 
%caso esta repetida en L1
sacarDuplicados([X|L1], [X|L2]) :- not(member(X,L1)), sacarDuplicados(L1,L2).

% iv. permutación(+L1, ?L2), que tiene éxito cuando L2 es permutación de L1. 
permutacion([], []).
permutacion([X|Xs], Ys) :- permutacion(Xs, Zs), insertar(X, Zs, Ys).

insertar(X, L, LX) :- append(P, S, L), append(P, [X|S], LX).

% v. reparto(+L, +N, -LListas) que tenga éxito si LListas es una lista de N listas (N ≥ 1) de cualquier
% longitud - incluso vacías - tales que al concatenarlas se obtiene la lista L.
reparto(L,1,[L]).
reparto(L,N,[X|LL]) :- N>1,append(X,Resto,L), N2 is N-1,reparto(Resto,N2,LL).

%Ejercicio 9
elementosTomadosEnOrden(L,0,[]).
elementosTomadosEnOrden(L,N,[X|E]) :- append(L1,[X|L2],L), N2 is N-1,elementosTomadosEnOrden(L2,N2,E).

%ejercicio 10
desde(X,X).
desde(X,Y) :- var(Y),N is X+1, desde(N,Y).
desde(X,Y) :- nonvar(Y), Y >= X.

%ejercicio 11
intercalar([], L2, L2).
intercalar(L1, [], L1).
intercalar([X|L1],[Y|L2],[X,Y|L3]) :- intercalar(L1,L2,L3).
%todo es reversible intercalar(?L1,?L2,?L3)

%ejercicio 12
%vacio(+ABB)
vacio(nil).
%raiz(+ABB,?R)
raiz(bin(Izq,V,Der),V).
%altura(+ABB,?H)
altura(nil,0).
altura(bin(Izq,V,Der),H) :- altura(Izq,HI),altura(Der,HD),H is max(HI,HD) + 1.
%nodos(+ABB,?N)
nodos(nil,0).
nodos(bin(Izq,V,Der),N) :- nodos(Izq,N1), nodos(Der,N2), N is 1+N1+N2.

%bin(bin(bin(nil, 4, nil),6,bin(nil, 8, nil)),11,bin(nil,15,bin(nil, 17, nil)))

%ejercicio 13 
% inorder(+AB,-Lista)
inorder(nil,[]).
inorder(bin(I,V,D),L) :- inorder(I,LI),inorder(D,LD),append(LI,[V|LD],L).

% arbolConInorder(+Lista,-AB), versión inversa del predicado anterior.
arbolConInorder([],nil).
arbolConInorder(L,bin(I,V,D)) :- append(LI,[V|LD],L),arbolConInorder(LI,I),arbolConInorder(LD,D).

% iii. aBB(+T), que será verdadero si T es un árbol binario de búsqueda.
aBB(nil).
%maxlist falla si es vacia
aBB(bin(I,V,D)) :-  inorder(I,LI),max_list([V|LI],V),inorder(D,LD),min_list([V|LD],V),aBB(I), aBB(D).

%iv aBBInsertar(+X, +T1, -T2)
%caso vacio
aBBInsertar(X,nil,bin(nil,X,nil)).
%caso menor a la raiz, es lo mismo que insertarlo en el subarbol izquierdo 
% (A es el arbol resultante de meterle X al subarbol I)
aBBInsertar(X,bin(I,V,D),bin(A,V,D)) :- X<V,aBBInsertar(X,I,A).
%caso mayor a la raiz
aBBInsertar(X,bin(I,V,D),bin(I,V,A)) :- X>V, aBBInsertar(X,D,A).
%caso repetido
aBBInsertar(X,bin(I,X,D),bin(I,X,D)).

%ejercicio 14
son_coprimos(X,Y) :- 1 is gcd(X,Y).
%le paso uno instanciado ese lo aumento y genero todos los pares hasta ese
%generar_pares_desde(+X,-Y)
generar_pares_desde(X,X).
generar_pares_desde(X,Y) :- var(Y),N is X+1, generar_pares_desde(N,Y).
generar_pares_desde(X,Y) :- nonvar(Y), X<Y.

paresSuman(S,X,Y) :- S1 is S-1, between(1,S1,X), Y is S-X.  

generar_pares(X,Y) :- generar_pares_desde(2,S), paresSuman(S,X,Y).

coprimos(X,Y) :- generar_pares(X,Y), son_coprimos(X,Y). 

%ejercicio 15 cuadradoSemiLatino(+N, -XS).
cuadrado(N,C) :- length(C,N),filas(N,N,C).


filas(0,N,[]).
filas(M,N,[F|C]) :- M>0,M2 is M-1,length(F,N),filas(M2,N,C).

filasSuman([],Y).
filasSuman([F|C],Y) :- filaSuma(F,Y),filasSuman(C,Y).

filaSuma([],0).
filaSuma([L|LS],Y) :- between(0,Y,L),Y2 is Y-L, filaSuma(LS,Y2).

cuadradoSemiLatino(N,C) :- cuadrado(N,C),desde(0,Y),filasSuman(C,Y).


%recu 2023
camino(arbol(nil,R,nil),[R]).
camino(arbol(I,R,_),[R|C]) :- I\=nil,camino(I,C).
camino(arbol(_,R,D),[R|C]) :- D\=nil,camino(D,C).

caminoMasLargo(A,C) :- camino(A,C), not((camino(A,C2),length(C,N),length(C2,M),M>N)).

caminoUnicoLong(A,N,C) :- camino(A,C), length(C,N), not((camino(A,C2),C2\=C,length(C2,N))).


%sublistaMasLargaDePrimos clase de repaso
sublistaMasLargaDePrimos(L,L2):- esSublistaDeP(L,L2), 
                        not((esSublistaDeP(L,L3), length(L2,N),length(L3,M),M>N)).

primo(N) :- N>1,N2 is N-1, not((between(2,N2,D),mod(N,D) =:=0)).

esSublistaDeP(L,L2) :- sublista(L2,L) , todosSonPrimos(L2).

todosSonPrimos([]).
todosSonPrimos([X|L]) :- primo(X), todosSonPrimos(L).

%parcial viejo 
palabra(A,N,P) :- length(P,N), todaLetraEsDeA(P,A).

todaLetraEsDeA([],_).
todaLetraEsDeA([P|Ps],A):- member(P,A),todaLetraEsDeA(Ps,A).

frase(_,[]).
frase(A,F) :- desde(1,X), longitudesSuman(A,F,X).

longitudesSuman(_,[],0).
longitudesSuman(A,[P|F],X) :- between(1,X,L),palabra(A,L,P),X2 is X-L, longitudesSuman(A,F,X2).

