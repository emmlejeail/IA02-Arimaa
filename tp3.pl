force(rabbit,1).
force(cat,2).
force(dog, 3).
force(horse, 4).
force(camel, 5).
force(elephant, 6).

%force d'une piece
stronger(X,Y):- force(X, A), force(Y, B), A>B.

%fonction outils pour trouver les cases aux alentours
up([X1,Y1],[X2,Y1]):- X2 is X1-1, X2>=0.
down([X1,Y1],[X2,Y1]):- X2 is X1+1, X2<8.
right([X1,Y1],[X1,Y2]):- Y2 is Y1+1, Y2<8.
left([X1,Y1],[X1,Y2]):- Y2 is Y1-1,Y2>=0.

near(C1,C2):- up(C1,C2); down(C1,C2); right(C1,C2); left(C1,C2). 
piece([X,Y],[[X,Y,T,C]|Q]).
piece([X,Y],[_|Q]):- piece([X,Y],Q).

board([[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

testExistence([X,Y]):- board(B), piece([X,Y],B).
testColor(X,Y,C):- board(B),getColor([X,Y,_,C],B).
testAnimal(X,Y,A):- board(B), getAnimal([X,Y,A,_],B).
testPiece(X,Y,A,C):- board(B), testExistence([X,Y]), testColor(X,Y,C), testAnimal(X,Y,A).
getColor([X,Y,_,C],[[X,Y,T,C]|Q]).
getColor([X,Y,_,C],[_|Q]):- getColor([X,Y,_,C], Q).

getAnimal([X,Y,A,_],[[X,Y,A,C]|Q]).
getAnimal([X,Y,A,_],[_|Q]):- getAnimal([X,Y,A,_], Q).

valid_move([ORow, OCol],[NRow, NCol]):- testExistence([ORow,OCol]),
					testColor(ORow,OCol,silver),
					near([ORow, OCol],[NRow, NCol]),
					\+testExistence([NRow,NCol]).

add1_move([[ORow,OCol],[NRow,NCol]], OListMove, NListMove):- valid_move([ORow,OCol],[NRow,NCol]),
						 NListMove = [[[ORow,OCol],[NRow,NCol]]|OListMove].

%retract et assert pour mettre à jour