%  for music parsing

% runs under sicstus Prolog  4.2 on DICE

% start Prolog ( command ' sicstus '  on DICE ),  and consult these files:
%
%  | ?- [readfile].
%  | ?- [miNew].
%
%  example input in file, jig.abc using abc notation.
%
% show results up to 100 depth terms:

?- set_prolog_flag(toplevel_print_options,[quoted(true),numbervars(true),portrayed(true),max_depth(100)]).

% make dynamic;
% need to make similar declarations for any new grammar symbols
% that are introduced.

:- dynamic tune/2, line/2, bar1/2, bar/2, bar4/2, tonic/2, dominant/2.
:- dynamic tonicA/2,tonicB/2,tonicC/2,tonicD/2.
:- dynamic subdominant/2, ton/3, by_ton/3, dom/3, by_dom/3, subd/3, by_subd/3.
:- dynamic tonicA/1, tonicB/2, tonicC/2, tonicD/2.
:- dynamic sum_int/3.

% grammar allowing rhythmic variation

tune --> line, line.
line --> bar1, bar, bar, bar4.

bar1 --> tonic.
bar  --> tonic.
bar  --> subdominant.
bar  --> dominant.
bar4 --> tonic.

tonic --> tonicA.
tonic --> tonicB.
tonic --> tonicC.
tonic --> tonicD.

% parametrised by duration;
% unary notation allows randomisation of output to work:
% " s(1) " for 2, "s(s(1))" for 3, etc (up to 6).
%
%  Use constraints on durations using special sum_int/2 predicate as shown
%  in curly braces " {..} ". 

tonicA --> ton(1),by_ton(1),ton(1),ton(1),by_ton(1),ton(1),[bl].
tonicB --> ton(1),by_ton(1),ton(1), ton(A),ton(B), [bl], { sum_int(A, B, s(s(1))) }.
tonicC --> ton(s(1)),by_ton(1),ton(s(s(1))),[bl].
tonicD --> ton(1),by_ton(1),ton(1),ton(s(s(1))),[bl].
dominant --> dom(1),by_dom(1),dom(1),dom(1),by_dom(1),dom(1),[bl].
subdominant --> subd(1),by_subd(1),subd(1),subd(1),by_subd(1),subd(1),[bl].

ton(X) --> [a(X)].
ton(X) --> [d(X)].
ton(X) --> [f(X)].
ton(X) --> ['A'(X)].
by_ton(X) --> [b(X)].
by_ton(X) --> ['B'(X)].
by_ton(X) --> [e(X)].
by_ton(X) --> ton(X).

dom(X) --> [a(X)].
dom(X) --> [c(X)].
dom(X) --> [e(X)].
dom(X) --> ['A'(X)].
by_dom(X) --> [f(X)].
by_dom(X) --> dom(X).

subd(X) --> [b(X)].
subd(X) --> [d(X)].
subd(X) --> [g(X)].
subd(X) --> ['B'(X)].
by_subd(X) --> [e(X)].
by_subd(X) --> subd(X).

%% end of grammar

%% Need addition table explicitly up to 6 (= s(s(s(s(s(1))))) ),
%% to allow random generation to work.

sum_int( 1, 1, s(1) ).
sum_int( s(1), 1, s(s(1)) ).
sum_int( 1, s(1), s(s(1)) ).
sum_int( 1, s(s(1)), s(s(s(1))) ).
sum_int( s(1), s(1), s(s(s(1))) ).
sum_int( s(s(1)), 1, s(s(s(1))) ).
sum_int( 1, s(s(s(1))), s(s(s(s(1)))) ).
sum_int( s(1), s(s(1)), s(s(s(s(1)))) ).
sum_int( s(s(1)), s(1), s(s(s(s(1)))) ).
sum_int( s(s(s(1))), 1, s(s(s(s(1)))) ).
sum_int( 1, s(s(s(s(1)))), s(s(s(s(s(1))))) ).
sum_int( s(1), s(s(s(1))), s(s(s(s(s(1))))) ).
sum_int( s(s(1)), s(s(1)), s(s(s(s(s(1))))) ).
sum_int( s(s(s(1))), s(1), s(s(s(s(s(1))))) ).
sum_int( s(s(s(s(1)))), 1, s(s(s(s(s(1))))) ).



% Next procedure for random generation of melody following grammar;
%  NB: only works when each grammar symbol has a corresponding
%  "dynamic" declaration, as given above for the initial grammar

% randomised meta-interpreter
% runs under Sicstus 4.2

% NB: the clauses of object programs must be declared with
% the dynamic directive, eg " :- dynamic tune/2. "

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% random path through the search space --
% no back-tracking!

:- use_module(library(random)).
:- use_module(library(lists)).

ran_solve( true ) :- !.
ran_solve( (A,B) ) :- !, ran_solve(A), ran_solve(B).
ran_solve( P ) :- predicate_property(P, built_in), !, call(P).
ran_solve( HH ) :-  random_clause( HH, B ),
                    ran_solve(B).
random_clause( H, B ) :- setof( (H,B), clause(H,B), S),
                         random_pick( (H,B), S).
random_pick(X,List) :- length(List,N), rp(X,List,N).
rp(X,[X],_) :- !.
rp(X,List,N) :- random(0,N,Nth), nth0(Nth,List,X).

% special version for grammar top-level symbol.

random_tune( X, Y ) :- ran_solve( tune(X,Y) ).

% procedures to output query results to file
%

saveResult(Result,File) :-
    tell(File), write(Result), nl, told, nl,
    write('Result saved in '), write(File), write(' .'), nl.

% how to use:
%  the second argument is normally a sequence of alphanumeric characters,
% starting with a lower case letter;  to do anything else, put
% single quotes before and after the filename: eg  '~/results/r1' .
%
% for the first argument, use the variable that you use for the
% result you are computing, as below.

%  to generate new random melody from grammar, using
%  abc header from some appropriate abc melody:
% 

make_new_with_header( AbcFile, NewAbcFile ) :-
      get_write_header( AbcFile, NewAbcFile ),
      random_tune(X,[]),
      write_abc( NewAbcFile, X ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sample queries:

% to check input is parsed by this grammar:
% ?-  get_notes('jig.abc',Input), tune(Input,[]).
%  Input = [d(1),f(1),f(1),a(1),f(1),f(1),bl,a(1),b(1),...
%
% for generation:
%
% |  ?- tune(X,[]).
% X = [a(1),b(1),a(1),a(1),b(1),a(1),bl,a(1),b(1),a(1),a(1),...

% to call randomised:

% | ?- random_tune(X,[]).
% X = [a(1),'B'(1),f(1),a(1),'A'(1),d(1),bl,'B'(1),e(1),'B'(1),d(1),e(1), ...

% ?- random_tune(X,[]), saveResult(X,result1).
%
% Result saved in result1 .

% to output as abc;  need appropriate abc header, eg from some given abc file;
%
% | ?- get_write_header( 'jig.abc', 'new.abc' ), random_tune(X,[]),
%             write_abc( 'new.abc', X ).
% X = [d(1),e(1),d(1),'A'(1),e(1),a(1),bl,g(1),e(1),d(1),...
%
%  -- or more simply:
% | ?- make_new_with_header( 'jig.abc', 'new.abc' ).
%
%  Now the output file in abc can be converted to lilypond notation
%  using abc2ly ,  and typeset using the command
%
% =>   lilypond new.ly
%
%  Now the music can be viewed, and played with the generated midi.
%
%   abc2ly and lilypond are neither installed on DICE, but are freely available.
