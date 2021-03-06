% stupid FlowFree solver
% field is list of rows; each row contains 0 for empty cell and <number> for appropriate endpoint
% i.e. field on flowfree.png may be described as
% B00RO
% 000Y0
% 00Y00
% 0RO0G
% 0BG00
% and entered as list [[1,0,0,2,3],[0,0,0,4,0],[0,0,4,0,0],[0,2,3,0,5],[0,1,5,0,0]]
% usage: solve(+Field,-Solution).

point([H|T],Point):-H\=0,Point=H,!;point(T,Point).

% returns Index of Elem
index__([Elem|_],Elem,PrevIndex,PrevIndex).
index__([_|T],Elem,PrevIndex,Index):-NewIndex is PrevIndex+1,index__(T,Elem,NewIndex,Index).

index_(List,Elem,Index):-index__(List,Elem,1,Index).

% returns Elem by Index
elem_([Elem|_],Index,Index,Elem):-!.
elem_([_|T],Index,PrevIndex,Elem):-CurIndex is PrevIndex+1, elem_(T,Index,CurIndex,Elem).

elem(List,Index,Elem):-elem_(List,Index,1,Elem).

%returns [Row,Col] of Elem on Board
coords_([H|_],Elem,PrevIndex,[PrevIndex,ElemIndex]):-index_(H,Elem,ElemIndex).
coords_([_|T],Elem,PrevIndex,[RowIndex,ElemIndex]):-NewIndex is PrevIndex+1,coords_(T,Elem,NewIndex,[RowIndex,ElemIndex]).

coords(List,Elem,[RowIndex,ElemIndex]):-coords_(List,Elem,1,[RowIndex,ElemIndex]).

%changes element of List on Index to Point
set_list([_|T],Point,1,[Point|T]):-!.
set_list([H|T],Point,Index,[H|OutputTail]):-CurIndex is Index-1,set_list(T,Point,CurIndex,OutputTail).

%changes [Row,Col] on Board to Point
set_board([H|T],Point,[1,Col],[NewRow|T]):-set_list(H,Point,Col,NewRow),!.
set_board([H|T],Point,[Row,Col],[H|OutputTail]):-CurRow is Row-1,set_board(T,Point,[CurRow,Col],OutputTail).

%finds Begin and End of Point on Board
find(Board,Point,Begin,End):-
    coords(Board,Point,Begin),coords(Board,Point,End),
    Begin\=End,!.
    
%returns list of [Number,[BeginRow,BeginCol],[EndRow,EndCol]]
list_of_points_([],_,[]):-!.
list_of_points_([FirstRow|Tail],SkippedRows,[[Elem,[BRow,BCol],[ERow,ECol]]|T]):-
    point(FirstRow,Elem),
    find([FirstRow|Tail],Elem,[FBRow,BCol],[FERow,ECol]),
    BRow is FBRow+SkippedRows,
    ERow is FERow+SkippedRows,
    set_board([FirstRow|Tail],0,[FBRow,BCol],NewBoard),
    set_board(NewBoard,0,[FERow,ECol],ExBoard),
    list_of_points_(ExBoard,SkippedRows,T),!.
list_of_points_([_|Tail],SkippedRows,Points):-NewSkippedRows is SkippedRows+1, list_of_points_(Tail,NewSkippedRows,Points).

list_of_points(Board,Points):-list_of_points_(Board,0,Points).

connect_point_(Board,_,[ERow,ECol],[ERow,ECol],Board):-!.%elem(Board,ERow,Row),elem(Row,ECol,Number),!.

connect_point_(Board,Number,[CurRow,CurCol],[ERow,ECol],NewBoard):-
    elem(Board,CurRow,Row),
    elem(Row,CurCol,0),
    set_board(Board,Number,[CurRow,CurCol],NBoard),
    NewCurRow is CurRow-1,
    connect_point_(NBoard,Number,[NewCurRow,CurCol],[ERow,ECol],NewBoard).

connect_point_(Board,Number,[CurRow,CurCol],[ERow,ECol],NewBoard):-
    elem(Board,CurRow,Row),
    elem(Row,CurCol,0),
    set_board(Board,Number,[CurRow,CurCol],NBoard),
    NewCurCol is CurCol-1,
    connect_point_(NBoard,Number,[CurRow,NewCurCol],[ERow,ECol],NewBoard).
    
connect_point_(Board,Number,[CurRow,CurCol],[ERow,ECol],NewBoard):-
    elem(Board,CurRow,Row),
    elem(Row,CurCol,0),
    set_board(Board,Number,[CurRow,CurCol],NBoard),
    NewCurRow is CurRow+1,
    connect_point_(NBoard,Number,[NewCurRow,CurCol],[ERow,ECol],NewBoard).

connect_point_(Board,Number,[CurRow,CurCol],[ERow,ECol],NewBoard):-
    elem(Board,CurRow,Row),
    elem(Row,CurCol,0),
    set_board(Board,Number,[CurRow,CurCol],NBoard),
    NewCurCol is CurCol+1,
    connect_point_(NBoard,Number,[CurRow,NewCurCol],[ERow,ECol],NewBoard).
    
connect_point(Board,[Number,[BRow,BCol],End],NewBoard):-CurRow is BRow-1, connect_point_(Board,Number,[CurRow,BCol],End,NewBoard).
connect_point(Board,[Number,[BRow,BCol],End],NewBoard):-CurCol is BCol-1, connect_point_(Board,Number,[BRow,CurCol],End,NewBoard).
connect_point(Board,[Number,[BRow,BCol],End],NewBoard):-CurRow is BRow+1, connect_point_(Board,Number,[CurRow,BCol],End,NewBoard).
connect_point(Board,[Number,[BRow,BCol],End],NewBoard):-CurCol is BCol+1, connect_point_(Board,Number,[BRow,CurCol],End,NewBoard).

connect(Board,[],Board):-!.
connect(Board,[Point|Points],NewBoard):-connect_point(Board,Point,PBoard),connect(PBoard,Points,NewBoard).

%true if zero fields are on board
check_([Row|_]):-member(0,Row),!.
check_([_|Tail]):-check_(Tail).
solve(Board,Solution):-list_of_points(Board,Points),connect(Board,Points,Solution),not(check_(Solution)).