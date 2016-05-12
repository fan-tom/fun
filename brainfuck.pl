% simple brainfuck interpreter
% usage: solve(`++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.`,_).
% will print "Hello World!"

%returns elem by index
elem_([Elem|_],Index,Index,Elem):-!.
elem_([_|T],Index,PrevIndex,Elem):-CurIndex is PrevIndex+1, elem_(T,Index,CurIndex,Elem).
elem(List,Index,Elem):-elem_(List,Index,1,Elem).

replace([_|T],1,Sym,[Sym|T]):-!.
replace([Elem|T],Index,Sym,[Elem|NT]):-NIndex is Index-1, replace(T,NIndex,Sym,NT),!.

inc([Elem|T],1,[NElem|T]):-NElem is Elem+1,!.
inc([Elem|T],Index,[Elem|NList]):-NIndex is Index-1,inc(T,NIndex,NList),!.

dec([Elem|T],1,[NElem|T]):-NElem is Elem-1,!.
dec([Elem|T],Index,[Elem|NList]):-NIndex is Index-1,dec(T,NIndex,NList),!.

expand(List,-1,[0|List],1).
expand(List,1,NList,0):-append(List,[0],NList),!.


nested_(Target,_,_,Prog,ProgInd,1,ProgInd):-elem(Prog,ProgInd,Target),!.
nested_(Target,Compl,Direction,Prog,ProgInd,NestLevel,RProgInd):-elem(Prog,ProgInd,Target),
                                                             NNestLevel is NestLevel-1,
                                                             NProgInd is ProgInd+Direction,
                                                             nested_(Target,Compl,Direction,Prog,NProgInd,NNestLevel,RProgInd),!.

nested_(Target,Compl,Direction,Prog,ProgInd,NestLevel,RProgInd):-elem(Prog,ProgInd,Compl),
                                                            NNestLevel is NestLevel+1,
                                                            NProgInd is ProgInd+Direction,
                                                            nested_(Target,Compl,Direction,Prog,NProgInd,NNestLevel,RProgInd),!.

nested_(Target,Compl,Direction,Prog,ProgInd,NestLevel,RProgInd):-NProgInd is ProgInd+Direction,
                                                                 nested_(Target,Compl,Direction,Prog,NProgInd,NestLevel,RProgInd),!.

find_loop_begin(Prog,ProgInd,NProgInd):-nested_(0'[,0'],-1,Prog,ProgInd,0,NProgInd),!.
find_loop_end(Prog,ProgInd,NProgInd):-nested_(0'],0'[,+1,Prog,ProgInd,0,NProgInd),!.

prepare_tape(Tape,TapeInd,Tape,TapeInd):-length(Tape,N),TapeInd=<N,TapeInd>0,!.
prepare_tape(Tape,0,NTape,TapeIndInc):-expand(Tape,-1,NTape,TapeIndInc),!.
prepare_tape(Tape,TapeInd,NTape,TapeInd):-expand(Tape,1,NTape,_),!.

calc(Prog,Tape,ProgInd,_,Tape):-length(Prog,N),ProgInd>N,!.%посмотреть если в конце ']'

calc(Prog,Tape,ProgInd,TapeInd,Solution):-elem(Prog,ProgInd,0'+),
                                          inc(Tape,TapeInd,NTape),
                                          NProgInd is ProgInd+1,
                                          calc(Prog,NTape,NProgInd,TapeInd,Solution),!.

calc(Prog,Tape,ProgInd,TapeInd,Solution):-elem(Prog,ProgInd,0'-),
                                          dec(Tape,TapeInd,NTape),
                                          NProgInd is ProgInd+1,
                                          calc(Prog,NTape,NProgInd,TapeInd,Solution),!.

calc(Prog,Tape,ProgInd,TapeInd,Solution):-elem(Prog,ProgInd,0'>),
                                          NTapeInd is TapeInd+1,
                                          prepare_tape(Tape,NTapeInd,NTape,CorrTapeInd),
                                          NProgInd is ProgInd+1,
                                          calc(Prog,NTape,NProgInd,CorrTapeInd,Solution),!.

calc(Prog,Tape,ProgInd,TapeInd,Solution):-elem(Prog,ProgInd,0'<),
                                          NTapeInd is TapeInd-1,
                                          prepare_tape(Tape,NTapeInd,NTape,CorrTapeInd),
                                          NProgInd is ProgInd+1,
                                          calc(Prog,NTape,NProgInd,CorrTapeInd,Solution),!.

calc(Prog,Tape,ProgInd,TapeInd,Solution):-elem(Prog,ProgInd,0'[),
                                          elem(Tape,TapeInd,0),
                                          find_loop_end(Prog,ProgInd,NProgInd),
                                          calc(Prog,Tape,NProgInd,TapeInd,Solution),!.

calc(Prog,Tape,ProgInd,TapeInd,Solution):-elem(Prog,ProgInd,0'[),
                                          elem(Tape,TapeInd,_),
                                          NProgInd is ProgInd+1,
                                          calc(Prog,Tape,NProgInd,TapeInd,Solution),!.

calc(Prog,Tape,ProgInd,TapeInd,Solution):-elem(Prog,ProgInd,0']),
                                          elem(Tape,TapeInd,0),
                                          NProgInd is ProgInd+1,
                                          calc(Prog,Tape,NProgInd,TapeInd,Solution),!.

calc(Prog,Tape,ProgInd,TapeInd,Solution):-elem(Prog,ProgInd,0']),
                                          elem(Tape,TapeInd,_),
                                          find_loop_begin(Prog,ProgInd,NProgInd),
                                          calc(Prog,Tape,NProgInd,TapeInd,Solution),!.

calc(Prog,Tape,ProgInd,TapeInd,Solution):-elem(Prog,ProgInd,0'.),
                                          elem(Tape,TapeInd,Sym),
                                          format('~c',Sym),
                                          NProgInd is ProgInd+1,
                                          calc(Prog,Tape,NProgInd,TapeInd,Solution),!.

calc(Prog,Tape,ProgInd,TapeInd,Solution):-elem(Prog,ProgInd,0',),
                                          get_code(NSym),
                                          replace(Tape,TapeInd,NSym,NTape),
                                          NProgInd is ProgInd+1,
                                          calc(Prog,NTape,NProgInd,TapeInd,Solution),!.
                                          
solve(Prog,Output):-calc(Prog,[0],1,1,Output),!.