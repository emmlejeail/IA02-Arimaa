:- module(bot,
      [  get_moves/3
      ]).
	
% A few comments but all is explained in README of github

% get_moves signature
% get_moves(Moves, gamestate, board).

% Exemple of variable
% gamestate: [side, [captured pieces] ] (e.g. [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ]) 
% board: [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]

% Call exemple:
% get_moves(Moves, [silver, []], [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

% default call
get_moves(Moves, Gamestate, Board):- play(Board, [],Moves,0),!.

force(rabbit,1).
force(cat,2).
force(dog, 3).
force(horse, 4).
force(camel, 5).
force(elephant, 6).

%force dune piece
stronger(X,Y):- force(X, A), force(Y, B), A>B.

%fonction outils pour trouver les cases aux alentours
up([X1,Y1],[X2,Y1]):- X2 is X1-1, X2>=0.
down([X1,Y1],[X2,Y1]):- X2 is X1+1, X2<8.
right([X1,Y1],[X1,Y2]):- Y2 is Y1+1, Y2<8.
left([X1,Y1],[X1,Y2]):- Y2 is Y1-1,Y2>=0.

near(C1,C2):- up(C1,C2); down(C1,C2); right(C1,C2); left(C1,C2). 

piece([X,Y],[[X,Y,T,C]|Q]).
piece([X,Y],[_|Q]):- piece([X,Y],Q).

%board([[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

testExistence([X,Y], Board):- piece([X,Y],Board).
testColor(X,Y,C, Board):- getColor([X,Y,_,C],Board).
testAnimal(X,Y,A, Board):- getAnimal([X,Y,A,_],Board).
testPiece(X,Y,A,C, Board):- testExistence([X,Y], Board), testColor(X,Y,C, Board), testAnimal(X,Y,A, Board).

getColor([X,Y,_,C],[[X,Y,T,C]|Q]).
getColor([X,Y,_,C],[_|Q]):- getColor([X,Y,_,C], Q).

getAnimal([X,Y,A,_],[[X,Y,A,C]|Q]).
getAnimal([X,Y,A,_],[_|Q]):- getAnimal([X,Y,A,_], Q).

isTrap([2,2]).
isTrap([2,5]).
isTrap([5,2]).
isTrap([5,5]).

valid_move([[ORow, OCol],[NRow, NCol]], Moves, Board):- testPiece(ORow,OCol,A,silver, Board),
					near([ORow, OCol],[NRow, NCol]),
					\+isTrap([NRow,NCol]),
					\+testExistence([NRow,NCol], Board),
					\+is_frozen([ORow,OCol,A,silver],Board),
					\+rabbitBackward([[ORow,OCol],[NRow,NCol]],Board),
					\+member([[ORow,OCol],[NRow,NCol]],Moves).

					

getPiece(Row,Col,Piece, Board):-setof((Row,Col,A,C),testPiece(Row,Col,A,C, Board),Piece).

%RETOURNE TOUS LES MOUVEMENTS POSSIBLES%
getAllMoves([X,Y],Moves,ListePossibleMoves, Board):- setof(([X,Y]), valid_move([X,Y], Moves, Board), ListePossibleMoves).


retire_el([],_,[]).
retire_el([X|L],X,L):- !.
retire_el([Y|L],X,[Y|NewL]):- retire_el(L,X,NewL).

change_el([],_,_,[]).
change_el([X|L],X,Y,[Y|L]):-!.
change_el([Y|L],X,_,[Y|NewL]):- change_el(L,X,NewL).

substitue(_,_,[],[]).
substitue(X,Y,[X|R],[Y|R1]) :- substitue(X,Y,R,R1).
substitue(X,Y,[Z|R],[Z|R1]) :- X\==Z, substitue(X,Y,R,R1).

member(X,[X|_]):- !.
member(X,[_|Reste]) :- member(X,Reste).


%add1_move([[ORow,OCol],[NRow,NCol]], OListMove, NListMove):- valid_move([ORow,OCol],[NRow,NCol]),
%						 NListMove = [[[ORow,OCol],[NRow,NCol]]|OListMove].
%						 %change_board([[ORow,OCol],[NRow,NCol]], NBoard).


long([], N). 
long([_|Q], N) :- long(Q, N1), N is N1 + 1. 

%AJOUTE UN MOUVE A LA LISTE%
add_move(NbCoups,NewNbCoups,Moves,NewMove, Board, NBoard):- getAllMoves(_,Moves,ListMove, Board),
														choose_move(ListMove,NewMove,Board),
														NewNbCoups is NbCoups + 1,
														change_board(NewMove,Board,NBoard).



%PERMET DE METTRE A JOUR LE BOARD%
change_board([[ORow,OCol],[NRow,NCol]],Board,NBoard):- testAnimal(ORow,OCol,A, Board),
												testColor(ORow,OCol,C, Board),
												substitue([ORow,OCol,A,C],[NRow,NCol,A,C],Board,NBoard).

play([],_,[],_).
play(_,_,[],4):-!.
play(Board,OldMoves, Moves, N):- N<5,
						add_move(N, NewNbCoups, OldMoves, NewMove,Board, NewBoard),
						concat([NewMove],OldMoves,NewMoves),
						play(NewBoard, NewMoves, Move, NewNbCoups),
						concat([NewMove],Move,Moves).


%PERMET DE CONCAT DEUX LISTES%
concat([],L,L). 
concat([H|T],L2,[H|L3]):- concat(T,L2,L3). 

%TEST SI UNE PIECE EST AMIE DUNE AUTRE%
is_friend([X,Y,A,C],[U,V,W,C]).

%TEST SI UNE PIECE EST PROTEGE%
is_protected([X,Y,A,C], Board):- testPiece(U,V,W,Z,Board),
								near([X,Y],[U,V]),
								is_friend([X,Y,A,C],[U,V,W,Z]).

%TEST SI UNE PIECE EST GELEE%
is_frozen([X,Y,A,silver],Board):- testPiece(U,V,W,gold,Board),
									near([X,Y],[U,V]),
									stronger(W,A),
									\+is_protected([X,Y,A,silver],Board).

is_different_board(Board,NBoard).

%EMPECHER DE RECULER LES LAPINS%
rabbitBackward([[X1,Y1],[X2,Y2]],Board):- up([X1,Y1],[X2,Y2]),
							testAnimal(X1,Y1,rabbit,Board).

%CHOIX MOUVEMENT%
choose_move(Moves,[[X,Y],[U,V]],Board):- calculate_all_score(Moves, ScoredMoves, Board),
										get_best_score(ScoredMoves, [[[X,Y],[U,V]],M]).
				

score_move([[X,Y],[U,V]], Score, Board):- testColor(X,Y, silver, Board),
										testAnimal(X,Y,rabbit,Board),
										down([X,Y],[U,V]), U=7,
										Score is 20.
score_move([[X,Y],[U,V]], Score, Board):- testColor(X,Y, silver, Board),
										testAnimal(X,Y,rabbit,Board),
										down([X,Y],[U,V]),
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
										testAnimal(X,Y,horse,Board),
										U<5, V<3, U>X,
										Score is 5.
score_move([[X,Y],[U,V]], Score,Board):- testColor(X,Y,silver,Board),
										testAnimal(X,Y,camel,Board),
										U<5, V<5, U>X,
										Score is 7.
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

calculate_score_move([[X,Y],[U,V]], [[[X,Y],[U,V]],Score], Board):- score_move([[X,Y],[U,V]],Score,Board).

calculate_all_score([], ScoredMoves, _).
calculate_all_score([T|Q], ScoredMoves, Board):- calculate_score_move(T,CurrentScoreMove, Board),
						calculate_all_score(Q,ScoreMove,Board),
						concat([CurrentScoreMove],ScoreMove,ScoredMoves). 

my_max([], R, R). %end
my_max([X|Xs], WK, R):- X >  WK, my_max(Xs, X, R). %WK is Carry about
my_max([X|Xs], WK, R):- X =< WK, my_max(Xs, WK, R).
my_max([X|Xs], R):- my_max(Xs, X, R). %start

get_score_max([Max],Max):-!.
get_score_max([[[[X,Y],[U,V]],S]|Q], [[[A,B],[C,D]],M]):- get_score_max(Q, [[[A,B],[C,D]],M]), M >= S.
get_score_max([[[[X,Y],[U,V]],S]|Q], [[[A,B],[C,D]],S]):- get_score_max(Q, [[[X,Y],[U,V]],S]), S > M.

get_best_score([], M,M).
get_best_score([[[[X,Y],[U,V]],S]|Q], [[[A,B],[C,D]],T], M):- S>T, get_best_score(Q, [[[X,Y],[U,V]],S], M).
get_best_score([[[[X,Y],[U,V]],S]|Q], [[[A,B],[C,D]],T], M):- T>=S, get_best_score(Q, [[[A,B],[C,D]],T], M).
get_best_score([[[[X,Y],[U,V]],S]|Q],M):- get_best_score(Q,[[[X,Y],[U,V]],S],M).
