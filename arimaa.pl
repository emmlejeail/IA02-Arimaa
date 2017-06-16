:- module(bot,
      [  get_moves/3
      ]).

% default call
get_moves(Moves, Gamestate, Board):- play(Board, [],Moves,0),!.

%board([[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).




% DEFINITION DES PIECES

% FORCE DES PIECES
force(rabbit,1).
force(cat,2).
force(dog, 3).
force(horse, 4).
force(camel, 5).
force(elephant, 6).

% FORCE SUPERIEURE
stronger(X,Y):- force(X, A), force(Y, B), A>B.




% TESTS CONCERNANT LES CARACTERISTIQUES DES PIECES

% RETOURNE LES PIECES EXISTANTES SUR LE BOARD
piece([X,Y],[[X,Y,T,C]|Q]).
piece([X,Y],[_|Q]):- piece([X,Y],Q).
testExistence([X,Y], Board):- piece([X,Y],Board).

% RETOURNE LA COULEUR D UNE PIECE
getColor([X,Y,_,C],[[X,Y,T,C]|Q]).
getColor([X,Y,_,C],[_|Q]):- getColor([X,Y,_,C], Q).
testColor(X,Y,C, Board):- getColor([X,Y,_,C],Board).

% RETOURNE L ANIMAL D UNE PIECE
getAnimal([X,Y,A,_],[[X,Y,A,C]|Q]).
getAnimal([X,Y,A,_],[_|Q]):- getAnimal([X,Y,A,_], Q).
testAnimal(X,Y,A, Board):- getAnimal([X,Y,A,_],Board).

% TESTE TOUTES LES CARACTERISTIQUES D UNE PIECE
testPiece(X,Y,A,C, Board):- testExistence([X,Y], Board), testColor(X,Y,C, Board), testAnimal(X,Y,A, Board).

% RECUPERE LA LISTE DES PIECES VOULUES
getPiece(Row,Col,Piece, Board):-setof((Row,Col,A,C),testPiece(Row,Col,A,C, Board),Piece).

% TEST SI UNE PIECE EST AMIE DUNE AUTRE AKA MEME COULEUR
is_friend([X,Y,A,C],[U,V,W,C]).

% TEST SI UNE PIECE EST PROTEGEE AKA FRIEND A COTE
is_protected([X,Y,A,C], Board):- testPiece(U,V,W,Z,Board),
                								near([X,Y],[U,V]),
                								is_friend([X,Y,A,C],[U,V,W,Z]).

% TEST SI UNE PIECE EST GELEE
is_frozen([X,Y,A,silver],Board):- testPiece(U,V,W,gold,Board),
                									near([X,Y],[U,V]),
                									stronger(W,A),
                									\+is_protected([X,Y,A,silver],Board).





% FONCTIONS OUTILS POUR TROUVER LES PIECES AUX ALENTOURS
up([X1,Y1],[X2,Y1]):- X2 is X1-1, X2>=0.
down([X1,Y1],[X2,Y1]):- X2 is X1+1, X2<8.
right([X1,Y1],[X1,Y2]):- Y2 is Y1+1, Y2<8.
left([X1,Y1],[X1,Y2]):- Y2 is Y1-1,Y2>=0.

near(C1,C2):- up(C1,C2); down(C1,C2); right(C1,C2); left(C1,C2).



% ATTENTION AUX PIEGES
isTrap([2,2]).
isTrap([2,5]).
isTrap([5,2]).
isTrap([5,5]).




% OPERATIONS SUR LES LISTES

retire_el([],_,[]).
retire_el([X|L],X,L):- !.
retire_el([Y|L],X,[Y|NewL]):- retire_el(L,X,NewL).

change_el([],_,_,[]).
change_el([X|L],X,Y,[Y|L]):-!.
change_el([Y|L],X,_,[Y|NewL]):- change_el(L,X,NewL).

replace(E,S,[],[]).
replace(E,S,[E|T1],[S|T2]):-replace(E,S,T1,T2).
replace(E,S,[H|T1],[H|T2]):-E\=H, replace(E,S,T1,T2).

member(X,[X|_]):- !.
member(X,[_|Reste]) :- member(X,Reste).

long([], N).
long([_|Q], N) :- long(Q, N1), N is N1 + 1.

concat([],L,L).
concat([H|T],L2,[H|L3]):- concat(T,L2,L3).






% A PROPOS DES MOUVEMENTS DE NOTRE IA

% EMPECHER DE RECULER LES LAPINS
rabbitBackward([[X1,Y1],[X2,Y2]],Board):- up([X1,Y1],[X2,Y2]),
							                            testAnimal(X1,Y1,rabbit,Board).


% VERIFIE SI UN MOVE EST VALIDE PAR RAPPORT AUX REGLES

valid_move([[ORow, OCol],[NRow, NCol]], Moves, Board):- testPiece(ORow,OCol,A,silver, Board),               % vérif moves de base
                                              					near([ORow, OCol],[NRow, NCol]),
                                                        \+isTrap([NRow,NCol]),
                                              					\+testExistence([NRow,NCol], Board),
                                              					\+is_frozen([ORow,OCol,A,silver],Board),
                                              					\+rabbitBackward([[ORow,OCol],[NRow,NCol]],Board),
                                              					\+member([[ORow,OCol],[NRow,NCol]],Moves).

valid_move([[ORow, OCol],[NRow, NCol]], Moves, Board):- testPiece(ORow,OCol,L,silver, Board),               % si la case voisine est prise mais qu on peut la push
                                                        \+is_frozen([ORow,OCol,L,silver],Board),
                                                        ok_to_push([OCol,ORow],[NRow,NCol],[A,B],Board),
                                              					\+member([[ORow,OCol],[NRow,NCol]],Moves).


% POUSSER UN ADVERSAIRE SI POSSIBLE, DANS UN PIEGE
ok_to_push([X,Y],[U,V],[A,B],Board):- testColor(X,Y, silver, Board),              % nous sommes argent
                                      testColor(U,V, gold, Board),                % l autre est or
                                      near([X, Y],[U, V]),                        % c est notre voisin
                                      near([U, V],[A, B]),                        % il a des cases voisines
                                      isTrap([A,B]),                              % qui sont des pieges
                                      \+testExistence([A,B], Board),              % et qui sont libres
                                      testAnimal(X,Y,R,Board),                    % recup de notre animal
                                      testAnimal(U,V,P,Board),                    % et de celui du voisin
                                      stronger(R,P),                              % pour savoir si nous sommes plus fort
                                      \+is_protected([U,V,P,gold],Board).         % si en plus il n est pas protege on peut le pousser dans le piege !

push([X,Y],[U,V],Board,NBoard):- ok_to_push([X,Y],[U,V],[A,B],Board),
                                 replace([U,V,P,gold],[A,B,P,gold],Board,RBoard),             % [U,V] va en [A,B] -> et boum capturé !
                                 replace([X,Y,R,silver],[U,V,R,silver],RBoard,NBoard).        % [X,Y] va en [U,V]



% TIRER UN ADVERSAIRE POUR AIDER UN AMI GELE
ok_to_pull([[X,Y],[A,B]],Board):- testColor(X,Y,silver,Board),        % nous sommes argent
                                                                                  % l autre est or
                                            near([X, Y],[U, V]),
                                            testPiece(U,V,P,gold,Board),                 % c est notre voisin
                                            near([X, Y],[A, B]),                  % nous avons des cases voisines
                                            \+testExistence([A,B], Board),        % libres pour pouvoir bouger
                                            testAnimal(X,Y,R,Board),              % recup de notre animal
                                            %testAnimal(U,V,P,Board),              % et de celui de notre voisin
                                            stronger(R,P),                        % pour voir si on est plus fort
                                            \+is_protected([U,V,P,gold],Board),   % sachant qu il n est pas protege
                                            near([U, V],[L, M]),                  % notre voisin a un voisin
                                            testPiece(L,M,I,silver,Board),        % qui existe et est notre ami
                                            is_frozen([L,M,I,silver],Board).      % et est gele par sa faute car moins forte donc on veut l aider !

pull([X,Y],[A,B],Board,NBoard):- ok_to_pull([[X,Y],[A,B]],Board),
                                 replace([X,Y,R,silver],[A,B,R,silver],Board,RBoard),       % [X,Y] va en [A,B]
                                 replace([U,V,P,gold],[X,Y,P,gold],RBoard,NBoard).          % [U,V] va en [X,Y] -> [L,M] est libéré !





% RECUPERE TOUS LES MOUVEMENTS POSSIBLES
getAllMoves([X,Y],Moves,ListePossibleMoves, Board):- setof([[X,Y],[A,B]], valid_move([[X,Y],[A,B]], Moves, Board), ListePossibleMoves).




% DEFINITION DES SCORES DES MOVES AKA NOTRE STRATEGIE

score_move([[X,Y],[U,V]], Score, Board):- testColor(X,Y, silver, Board),
                          								testAnimal(X,Y,rabbit,Board),
                          								down([X,Y],[U,V]), U=7,                 % LA VICTOIRE !!!
                          								Score is 20.

score_move([[X,Y],[U,V]], Score, Board):- testColor(X,Y, silver, Board),
                      										testAnimal(X,Y,rabbit,Board),
                      										down([X,Y],[U,V]),                      % faire descendre nos lapins en priorité
                      										Score is 10.

score_move([[X,Y],[U,V]], Score, Board):- ok_to_pull([[X,Y],[U,V]],Board),          % help a friend in need
                      										Score is 9.

score_move([[X,Y],[U,V]], Score, Board):- ok_to_push([X,Y],[U,V],[A,B],Board),    % capturer l ennemi
                      										Score is 9.


score_move([[X,Y],[U,V]], Score, Board):- testColor(X,Y,silver, Board),
                      										testAnimal(X,Y,elephant, Board),
                      										U>1, U<5, V>2, V<6, U>X,
                      										Score is 8.

score_move([[X,Y],[U,V]], Score, Board):- testColor(X,Y,silver, Board),
                      										testAnimal(X,Y,elephant, Board),
                      										U<5, V>3, V<6, U>X,
                      										Score is 8.

score_move([[X,Y],[U,V]], Score,Board):- testColor(X,Y,silver,Board),
                    										testAnimal(X,Y,camel,Board),
                    										U<6, V<3, U>X,
                    										Score is 7.

score_move([[X,Y],[U,V]], Score,Board):- testColor(X,Y,silver,Board),
                    										testAnimal(X,Y,camel,Board),
                    										U<5, V<5, U>X,
                    										Score is 7.

score_move([[X,Y],[U,V]], Score,Board):- testColor(X,Y,silver,Board),
                    										testAnimal(X,Y,horse,Board),
                    										U<5, V<3, U>X,
                    										Score is 5.

score_move([[X,Y],[U,V]], Score,Board):- testColor(X,Y,silver,Board),
                    										testAnimal(X,Y,horse,Board),
                    										U<5, V<5, U>X,
                    										Score is 5.

score_move([[X,Y],[U,V]], Score,Board):- testColor(X,Y,silver,Board),
                    										testAnimal(X,Y,dog,Board),
                    										U<5, V<5, U>X,
                    										Score is 4.

score_move([[X,Y],[U,V]], Score,Board):- testColor(X,Y,silver,Board),
                              					testAnimal(X,Y,dog,Board),
                              					U<5, V<5, U>X,
                              					Score is 4.

score_move([[X,Y],[U,V]], Score,Board):- testColor(X,Y,silver,Board),
                              					testAnimal(X,Y,cat,Board),
                              					U<5, V<5, U>X,
                              					Score is 3.

score_move([[X,Y],[U,V]], Score,Board):- testColor(X,Y,silver,Board),
                              					testAnimal(X,Y,cat,Board),
                              					U<5, V<5, U>X,
                              					Score is 3.

score_move([[X,Y],[U,V]], 0, _).




% CONCATENE LE SCORE AU MOVE APRES L AVOIR CALCULE
calculate_score_move([[X,Y],[U,V]], [[[X,Y],[U,V]],Score], Board):- score_move([[X,Y],[U,V]],Score,Board).

% CALCULE LE SCORE DE TOUS LES MOVES D UNE LISTE
calculate_all_score([], ScoredMoves, _).
calculate_all_score([T|Q], ScoredMoves, Board):- calculate_score_move(T,CurrentScoreMove, Board),
                        												 calculate_all_score(Q,ScoreMove,Board),
                        												 concat([CurrentScoreMove],ScoreMove,ScoredMoves).


% RENVOIE LE MOVE AYANT LE MEILLEUR SCORE
%get_best_score(ListePossibleMoves,BestMove).

get_best_score([], M,M).
get_best_score([[[[X,Y],[U,V]],S]|Q], [[[A,B],[C,D]],T], M):- S>T, get_best_score(Q, [[[X,Y],[U,V]],S], M).
get_best_score([[[[X,Y],[U,V]],S]|Q], [[[A,B],[C,D]],T], M):- T>=S, get_best_score(Q, [[[A,B],[C,D]],T], M).
get_best_score([[[[X,Y],[U,V]],S]|Q],M):- get_best_score(Q,[[[X,Y],[U,V]],S],M).

get_best_move(ListePossibleMoves, Board, Move):- calculate_all_score(ListePossibleMoves,ScoredMoves,Board),
                                              setof(H,get_best_score(ScoredMoves,[[H|[I|O]]]),Move).


% CALCULE LES SCORES DES MOVES POSSIBLES ET RENVOIE CELUI DONT LE SCORE EST LE MEILLEUR
%choose_move(Move,ListePossibleMoves,Board):- calculate_all_score(ListePossibleMoves, ScoredMoves, Board),
%										                    setof([[X,Y],[U,V]],get_best_score(ScoredMoves, [[[X,Y],[U,V]],M]),Move).


choose_move(Moves,[[X,Y],[U,V]],Board):- calculate_all_score(Moves, ScoredMoves, Board),
                    get_best_score(ScoredMoves, [[[X,Y],[U,V]],M]),!.



% PERMET DE METTRE A JOUR LE BOARD
change_board([[ORow,OCol],[NRow,NCol]],Board,NBoard):- push([ORow,OCol],[NRow,NCol],Board,NBoard).

change_board([[ORow,OCol],[NRow,NCol]],Board,NBoard):- pull([ORow,OCol],[NRow,NCol],Board,NBoard).

change_board([[ORow,OCol],[NRow,NCol]],Board,NBoard):- testAnimal(ORow,OCol,A, Board),
                              												 testColor(ORow,OCol,C, Board),
                                                       \+ok_to_pull([[ORow,OCol],[NRow,NCol]],Board),
                              												 replace([ORow,OCol,A,C],[NRow,NCol,A,C],Board,NBoard).





% AJOUTE UN MOVE QUI A PASSE LA SELECTION A LA LISTE

add_move(NbCoups,NewNbCoups,Moves,NewMove, Board, NBoard):- getAllMoves(_,Moves,ListMove, Board),
                                														choose_move(ListMove,Move,Board),
                                                            ok_to_pull(Move,Board),
                                														NewNbCoups is NbCoups + 2,                     % ATTENTION PULL COUTE 2 MOVES
                                														change_board(Move,Board,NBoard).

add_move(NbCoups,NewNbCoups,Moves,NewMoves, Board, NBoard):- getAllMoves(_,Moves,ListMove, Board),
                                                            choose_move(ListMove,[[ORow,OCol],[NRow,NCol]],Board),
                                                            isTrap([A,B]),
                                                            ok_to_push([ORow,OCol],[NRow,NCol],[A,B],Board),
                                                            concat([[[NRow,NCol],[A,B]]],[[[ORow,OCol],[NRow,NCol]]],NewMoves),
                                                            NewNbCoups is NbCoups + 2,                     % ATTENTION PUSH COUTE 2 MOVES
                                                            change_board(NewMove,Board,NBoard).

add_move(NbCoups,NewNbCoups,Moves,NewMove, Board, NBoard):- getAllMoves(_,Moves,ListMove, Board),
                                														choose_move(ListMove,NewMove,Board),
                                														NewNbCoups is NbCoups + 1,
                                														change_board(NewMove,Board,NBoard).



% VERIFIE QUE LE TOUR N EST PAS NUL AKA SI L ANCIEN ET LE NEW BOARD SONT LES MEMES
p([H|T], H, T).
is_same_board([],NBoard):-!.
is_same_board(Board,NBoard):- p(Board, X, Y),
                              member(X, NBoard),
                              retire_el(Board, X, RBoard),
                              is_same_board(RBoard, NBoard).



% ET VOILA ON PEUT ENFIN JOUER TRANQUILLEMENT
play([],_,[],_).
%play(_,_,[],0):- length(Board, A) = length(NBoard, B).
play(_,_,[],4):- %\+is_same_board(Board, NewBoard),
                !.
play(Board,OldMoves, Moves, N):- N<5,
                    						add_move(N, NewNbCoups, OldMoves, NewMove,Board, NewBoard),
                                concat([NewMove],OldMoves,NewMoves),
                    						play(NewBoard, NewMoves, Move, NewNbCoups),
                    						concat([NewMove],Move,Moves).
