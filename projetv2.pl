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

valid_move([[ORow, OCol],[NRow, NCol]], Board):- testExistence([ORow,OCol], Board),
					testColor(ORow,OCol,silver, Board),
					near([ORow, OCol],[NRow, NCol]),
					\+testExistence([NRow,NCol], Board).

%dynamic board/1.
add1_move([[ORow,OCol],[NRow,NCol]], OListMove, NListMove):- valid_move([[ORow,OCol],[NRow,NCol]]),
						 NListMove = [[[ORow,OCol],[NRow,NCol]]|OListMove].
						

getPiece(Row,Col,Piece, Board):-setof((Row,Col,A,C),testPiece(Row,Col,A,C, Board),Piece).

%RETOURNE TOUS LES MOUVEMENTS POSSIBLES%
getAllMoves([X,Y],ListePossibleMoves, Board):- setof(([X,Y]), valid_move([X,Y], Board), ListePossibleMoves).


retire_el([],_,[]).
retire_el([X|L],X,L):- !.
retire_el([Y|L],X,[Y|NewL]):- retire_el(L,X,NewL).

change_el([],_,_,[]).
change_el([X|L],X,Y,[Y|L]):-!.
change_el([Y|L],X,_,[Y|NewL]):- change_el(L,X,NewL).

substitue(_,_,[],[]).
substitue(X,Y,[X|R],[Y|R1]) :- substitue(X,Y,R,R1).
substitue(X,Y,[Z|R],[Z|R1]) :- X\==Z, substitue(X,Y,R,R1).

%dynamic board/1.
%change_board([[ORow,OCol],[NRow,NCol]], OBoard, NBoard):-%retract(board(B)), 
							%getPiece(ORow, OCol, OPiece),
							%new_Piece(OPiece,NRow,NCol,NPiece),
							%substitue(OPiece,NPiece,OBoard,NBoard).
							%asserta(board(NBoard)).

%add1_move([[ORow,OCol],[NRow,NCol]], OListMove, NListMove):- valid_move([ORow,OCol],[NRow,NCol]),
%						 NListMove = [[[ORow,OCol],[NRow,NCol]]|OListMove].
%						 %change_board([[ORow,OCol],[NRow,NCol]], NBoard).


long([], N). 
long([_|Q], N) :- long(Q, N1), N is N1 + 1. 

add4_moves(PossibleMoves, OldListOfMoves, FinalListOfMoves, N):- N<5,
												add1_move(T, OldListOfMoves, ListOfMoves),
												N is N1+1,
												add4_moves(Q, ListOfMoves, FinalListOfMoves, N1).


%get_move récupérer desdebut board
%dynamic board/1.

add_move(NbCoups,NewNbCoups,T, Board, NBoard):- getAllMoves(_,[T|Q], Board),
														%NewMove is T,
														NewNbCoups is NbCoups + 1,
														change_board(T,Board,NBoard).



%PERMET DE METTRE A JOUR LE BOARD%
change_board([[ORow,OCol],[NRow,NCol]],Board,NBoard):- testAnimal(ORow,OCol,A, Board),
												testColor(ORow,OCol,C, Board),
												substitue([ORow,OCol,A,C],[NRow,NCol,A,C],Board,NBoard).

get4Moves(Board, FinalList):-add_move([],NewListe, Board),
								add_move(NewListe,NNewListe, Board),
								add_move(NNewListe,NNNewListe, Board),
								add_move(NNNewListe,FinalList, Board).


play([],[],_).
play(_,[],4):-!.
play(Board, Moves, N):- N<5,
						add_move(N, NewNbCoups, NewMove,Board, NewBoard),
						play(NewBoard, Move, NewNbCoups),
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

%score_case([X,Y,A,C], Score, Board):- testColor(X,Y, silver, Board),
										%testAnimal(X,Y,rabbit,Board),
										%x