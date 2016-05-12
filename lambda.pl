% primitive untyped lambda calculus with call-by-value evaluation strategy
% AND RIGHT-ASSOCIATIVE APPLICATION interpreter
% usage: eval(<filename>).
% one simple example in file boolean.l
% Author: F@nT0M
% Date: 29.04.2016

%"abc"==[a,b,c]
:- set_prolog_flag(double_quotes, chars).
%`abc` is string
:- set_prolog_flag(back_quotes, string).
%read backslash as normal character
:- set_prolog_flag(character_escapes, false).
%:- encoding(utf8).

:-dynamic(binding/2).

%expand_query(Q,Q,B,B):-write(Q),write(B).%,atom_string(Q,EQ),atom_string(B,EB).
%:- clause(expand_query(Q,Q,B,B),TBody),term_string(TBody,Body),
%    string_concat(`expand_query(Q,Q,B,B):-`,Body,SPred),
%    term_string(Pred,SPred),
%    assertz((Pred)).

%utility
%whitespaces
ws --> [W], { char_type(W, space) }, ws,!.
ws --> [],!.

%identifiers match [a-zA-Z][a-zA-Z0-9]* pattern
identifier(Ident) --> [A], { char_type(A, alpha) }, symbol_r(As), {atom_string([A|As],Ident)}.

symbol_r([A|As]) --> [A], { char_type(A, alnum) }, symbol_r(As).
symbol_r([])     --> [].

%make string from list of strings
stringlist_concat([],``).
stringlist_concat([H|T],Str):-stringlist_concat(T,StrT),string_concat(H,StrT,Str).

%convert list of chars to string
to_string(Charlist,Str):-string_chars(Str,Charlist).

%make charlist representation of term
to_chars(var{name:V},C):-string_chars(V,C).

to_chars(app{function:Func, argument:Arg},Str):-to_chars(Func,FuncStr),to_chars(Arg,ArgStr),
                                                 ((Arg=app{function:_, argument:_};Arg=def{parameter:_,body:_})->append(["(",ArgStr,")"],NArgStr)
                                                 ; NArgStr=ArgStr),
                                                 (Func=def{parameter:_, body:_}-> append(["(",FuncStr,")"],NFuncStr)
                                                 ; NFuncStr=FuncStr),
                                                 append([NFuncStr," ",NArgStr],Str).%atom_string(Str1,FuncStr),atom_string(Str2,ArgStr),append(["(",[Str1],[Str2],")"],Str3), atom_string(Str3,Str).
to_chars(def{parameter:Par, body:Body},Str):-to_chars(Par,ParStr),to_chars(Body,BodyStr),append(["λ",ParStr,".",BodyStr],Str).%atom_string(Str1,ParStr), atom_string(Str2, BodyStr), append(["(\",[Str1],".",[Str2],")"],Str3), atom_string(Str3,Str).


%parser avoiding left recursion in application Term=Term Term rule


term(E)-->variable(E).
term(E)-->braced(E).
term(E)-->abstraction(E).
term(E)-->application(E).


term_(app{function:F, argument:A})-->term(F)," ", term_(A).
term_(E)-->term(E).

application_braced(app{function:F, argument:A})-->braced(F), ws, term_(A).
application_abstraction(app{function:F, argument:A})-->abstraction(F), " ", term_(A).
application_variable(app{function:F, argument:A})-->variable(F), " ", term_(A).

application(E)-->application_braced(E);
                 application_abstraction(E);
                 application_variable(E).
%application(app{function:F, argument:A})-->"(", term(F), term(A), ")".

%local_var is just identifier that is free variable in current query
local_var(var{name:X})-->identifier(X),!, {atom_string(Name,X), current_predicate(binding/2)->(\+binding(Name,_));true}.

%if varable is external, we need to load its representation first
external_var(Term)-->identifier(X),!, {atom_string(Name,X), current_predicate(binding/2), binding(Name,Term)}.

%variable is new local name or previously defined term
variable(Var)-->local_var(Var)|external_var(Var).

%term in parentheses
braced(E)-->"(", term(E), ")".

%lambda-abstraction
abstraction(def{parameter:V, body:B})-->("λ";"\"), variable(V), ".", term(B).

assign(``)-->parse_bind(Name,Body), {bind_add(Name,Body)}.
expr(Str)-->term(Tree), {reduce(Tree,Red), to_chars(Red,Chars), to_string(Chars,Str)}.

%interpreter

%make lists of bound and free variables in term
vars(var{name:N},[Vars],[var{name:N}|Vars]).
vars(app{function:Term1, argument:Term2}, Vars, Res):-vars(Term1, V1), vars(Term2, V2), append([V1,V2|Vars],Res).

vars(var{name:V},Bound,Bound,[var{name:V}]):- \+member(var{name:V},Bound),!.
vars(var{name:_},Bound,Bound,[]):-!.
vars(app{function:Term1,argument:Term2},Bound,NBound,FV):-vars(Term1,Bound,NBound1,FV1),vars(Term2,NBound1,NBound,FV2),append(FV1,FV2,FV).
vars(def{parameter:Parameter,body:Body},Bound,NBound,FV):-append(Bound,[Parameter],NBound1),vars(Body,NBound1,NBound,FV).

%get list of all free variables in term
free_vars(Term,FV):-vars(Term,[],_,FV),!.

%get list of all bound variables in term
bound_vars(Term,Bound):-vars(Term,[],Bound,_),!.

%rename by just appending stroke
new_name(var{name:V},var{name:Name}):-string_concat(V,'`',Name).%atom_string(L,V),append([L],"'",NV),atom_string(NV,Name).

%alpha-conversion rules
rename(var{name:V},var{name:V},NV,NV).
rename(var{name:T},var{name:_},_,var{name:T}).
rename(app{function:Term1,argument:Term2},Parameter,NParameter,app{function:NTerm1,argument:NTerm2}):-rename(Term1,Parameter,NParameter,NTerm1),rename(Term2,Parameter,NParameter,NTerm2).
rename(def{parameter:Parameter1,body:Body},Parameter,NParameter,def{parameter:Parameter1,body:NBody}):-rename(Body,Parameter,NParameter,NBody).

%substitution with alpha-conversion
subst(var{name:Parameter},var{name:Parameter},T,T):-!.
subst(var{name:V},var{name:_},_,var{name:V}):-!.
subst(app{function:Term1,argument:Term2},Parameter,T,app{function:STerm1,argument:STerm2}):-subst(Term1,Parameter,T,STerm1), subst(Term2,Parameter,T,STerm2),!.
subst(def{parameter:Parameter1,body:Body},Parameter,T,def{parameter:NParameter1,body:Res}):-vars(T,[],BV,FV), append(FV,BV,Vars),
                                                                                          (member(Parameter1,Vars)->(new_name(Parameter1,NParameter1),
                                                                                                                   rename(Body,Parameter1,NParameter1,NBody))
                                                                                                                   ;
                                                                                                                   (NBody = Body, NParameter1 = Parameter1)),
                                                                                          subst(NBody,Parameter,T,Res),
                                                                                          !.

%application as substitution
make_app(def{parameter:Parameter,body:Body},Arg,Res):-subst(Body,Parameter,Arg,Res),!.

%substitution in var is var itself
make_app(Var,Arg,var{name:StrArg}):-to_chars(app{function:Var, argument:Arg},CharArg),to_string(CharArg,StrArg),!.

%beta-reduction with call-by-value evaluation strategy
reduce(app{function:Func,argument:Arg}, Res):-reduce(Func,FuncValue),reduce(Arg,ArgValue),make_app(FuncValue,ArgValue,App),reduce(App,Res),!.

%no reduction in abstraction
reduce(def{parameter:P,body:B},def{parameter:P,body:B}):-!.

%no reduction in variable
reduce(var{name:T},var{name:T}):-!.


%get parse tree representation
parse_tree(Term,Tree):-phrase(term(Tree),Term,[]),!.

%get parse tree after reduce
solve(Term, Res):-parse_tree(Term,Tree), reduce(Tree,Res),!.

%get charlist representation of solution
solve_chars(Term,Chars):-solve(Term,Res), to_chars(Res,Chars),!.

%get string representation of solution
solve_str(Term,Str):-solve_chars(Term,Chars), to_string(Chars,Str),!.

%separate <name>=<term> into name and term
parse_bind(Name,Body)-->identifier(Name),"=",term(Body).

%parse bind, build binding predicate from name and body, convert to term and append to base
bind_add(Name,Body):-term_string(Body,CharsBody,[back_quotes(string)]),
                      stringlist_concat([`binding`,`(`,Name,`,`,CharsBody,`)`],StrBind),
                      term_string(Binding,StrBind,[back_quotes(string)]),
                      assertz(Binding).

bind([]):-!.
bind([H|T]):-(string(H)->string_chars(H,Chars);atom(H)->atom_chars(H,Chars);Chars=H),
              phrase(parse_bind(Name,Body),Chars,[]),
              bind_add(Name,Body),
              bind(T),!.

%program consists only of assignments or terms
program-->[].
program-->(assign(Str);expr(Str)), ";",
/*semicolon is sync token, so we know that here we have parsed all right*/
          {Str\=``->write(Str),nl;true},
          ws,
          program.

eval(File):-open(File,read,Src),
            read_string(Src,"","",_,Str),
            close(Src),
            string_chars(Str,Chars),
            phrase(program,Chars).


%some examples
%true or false->true
%solve_str("((\l.\r.(l l)r)(\x.\y.x))(\x.\y.y)",R).
%or place it in file and eval
%fls=λx.λy.y;
%tru=λx.λy.x;
%and=λl.λr.((l r) l);
%((and tru) fls);