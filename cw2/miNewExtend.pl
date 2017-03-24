%% CW2
%
%This grammar is modified.
%
%%  for music parsing

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
%

:- dynamic tonicA/2,tonicB/2,tonicC/2,tonicD/2.
:- dynamic subdominantA/2, ton/3, by_ton/3, dom/3, by_dom/3, subd/3, by_subd/3.
:- dynamic tune/2, line/2, line2/2, bar1/2, bar/2, bar4/2, tonic/2, dominantA/2.
:- dynamic tonicA/1, tonicB/2, tonicC/2, tonicD/2.
:- dynamic sum_int/3.

% grammar allowing rhythmic variation

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The following lines have been modified to make the grammar more restrictive by
% forcing the tune to end on a perfect cadence.
% See bellow for discussion. Modified lines marked with MOD.
% See the ext1 section for desctription of this extension.

tune --> line, line2. %MOD
line --> bar1, bar, bar, bar4.
line2 --> bar1, bar, bar3, bar4. %MOD


bar1 --> tonic.
bar  --> tonic.
bar  --> subdominant.
bar  --> dominant.
bar3 -->dominant. % MOD
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
dominantA --> dom(1),by_dom(1),dom(1),dom(1),by_dom(1),dom(1),[bl].
subdominantA --> subd(1),by_subd(1),subd(1),subd(1),by_subd(1),subd(1),[bl].

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%]
% Examples accepted by this grammar.
%
% The following are two examples accepted by the grammar in its original form.
%
% Ex1 = [d(1),'B'(1),f(1),f(s(s(1))),bl,e(1),f(1),e(1),a(1),a(1),a(1),bl,
% 'A'(s(1)),e(1),f(s(s(1))),bl,a(s(1)),'B'(1),d(s(s(1))),bl,
% a(1),'B'(1),d(1),'A'(1),b(1),d(1),bl,g(1),d(1),'B'(1),g(1),e(1),g(1),bl,
% d(1),e(1),b(1),'B'(1),e(1),b(1),bl,'A'(s(1)),e(1),'A'(s(s(1))),bl].
%
% Note that the penultimate bar is on the subdominant. This is not permitted
% in the updated grammar. Additionally, all bars that are not on the tonic
% consist only of quavers. The grammar is extended to allow 
%
% Ex2 = ['A'(1),'A'(1),f(1),a(s(s(1))),bl,a(1),d(1),d(1),d(s(s(1))),bl,
% 'A'(1),c(1),e(1),a(1),f(1),c(1),bl,f(s(1)),b(1),a(s(s(1))),bl,
% f(s(1)),e(1),a(s(s(1))),bl,e(1),f(1),'A'(1),e(1),f(1),'A'(1),bl,
% 'B'(1),'B'(1),'B'(1),b(1),d(1),b(1),bl,a(s(1)),b(1),'A'(s(s(1))),bl].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOT ACCEPTED BY THIS GRAMMAR.

%
% This grammar only accepts tunes whose chord sequence uses the I, IV and V
% chords. These are the most important chords for a major key, being all of the
% major chords. However, many tunes will use other chords.
%
% This grammar only accepts melodies with rhythmic variation when on the tonic 
% chord. It could be argued that it is stylistic to only have longer notes on
% the tonic, but this is seen as tenuous.

% This grammar only accepts tunes in 6/8. Although this time signature is fairly
% common, it is still a very small subset of tunes. It was decided to not change
% this in this grammar, but this was done in miExtend. This means this grammar 
% stays true to the jig style which is typically in 6/8.
%
% Below are three extensions to the grammar, each with an example melody that 
% is accepted by the extended grammar, but not the original.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ext 1.
%
% Extension 1 is more of a limitation. It is made by modifications above and
% described here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Extension 1 modifies the grammar so that the penultimate bar must be a
% dominant bar. This gives a perfect cadence at the end of the piece.
% An example of a melody permitted by the grammar without this particular
% extension.
%
% Ex3 = ['A'(1),'B'(1),'A'(1),d(1),b(1),d(1),bl,b(1),c(1),g(1),b(s(s(1))),bl,
% b(1),e(1),d(1),g(s(1)),g(1),bl,a(1),'B'(1),f(1),f(1),f(1),d(1),bl,
% d(1),'A'(1),'A'(1),'A'(s(s(1))),bl,c(s(1)),a(1),a(s(s(1))),bl,
% 'B'(1),e(1),b(1),g(s(s(1))),bl,a(1),e(1),f(1),'A'(s(1)),a(1),bl].
%
% Note the penultimate bar is on the supertonic. This is quite unstylistic.
% 
% The following example is accepted both with or without this modification:
%
% Ex4 = [d(1),e(1),'A'(1),'A'(1),b(1),'A'(1),bl,b(1),c(1),b(1),'B'(s(1)),g(1),bl,
% f(1),e(1),f(1),d(s(s(1))),bl,a(1),'A'(1),f(1),f(1),'B'(1),a(1),bl,
% 'A'(1),'A'(1),f(1),'A'(1),b(1),d(1),bl,c(1),f(1),'A'(1),a(s(s(1))),bl,
% e(1),e(1),a(1),'A'(1),a(s(1)),bl,a(s(1)),b(1),f(s(s(1))),bl].
%
% Note that the penultimate bar is dominant.
%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ext 2.
%
%Here is an extension to allow chord ii, the supertonic.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Simple melody with supertonici (Ex5).
input(Super) :-
  Line1 = [d(1),f(1),f(1),d(1),b(1),d(1),bl,e(1),e(1),g(1),b(1),g(1),g(1),bl],
  Line3 = [d(1),g(1),'B'(1),d(1),e(1),d(1),bl,d(1),f(1),'A'(1),d(1),b(1),d(1),bl],
  Line2 = [d(1),f(1),f(1),d(1),b(1),d(1),bl,e(1),e(1),g(1),b(1),g(1),g(1),bl],
  Line4 = [d(1),g(1),'B'(1),d(1),e(1),d(1),bl,d(1),f(1),f(1),d(1),b(1),d(1),bl],
  append(Line1,Line2,Line3),
  append(Line3,Line4,Super).


:- dynamic supertonic/2, supt/3, by_supt/3.

bar --> supertonic.

supertonic --> supt(1),by_supt(1),supt(1),supt(1),by_supt(1),supt(1),[bl].

supt(X) --> [e(X)].
supt(X) --> [g(X)].
supt(X) --> [b(X)].
supt(X) --> ['B'(X)].
by_supt(X) --> [c(X)].
by_supt(X) --> supt(X).

%Allowing a minor chord adds colour.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ext 3.
%
%Here is an extension rhytmic variation not just on the tonic.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The following melody is accepted by this grammar but not the original
% Ex6 = ['A'(1),'B'(1),'A'(1),d(1),b(1),d(1),bl,b(1),c(1),g(1),b(s(s(1))),bl,
% b(1),e(1),d(1),g(s(1)),g(1),bl,a(1),'B'(1),f(1),f(1),f(1),d(1),bl,
% d(1),'A'(1),'A'(1),'A'(s(s(1))),bl,c(s(1)),a(1),a(s(s(1))),bl,
% 'B'(1),e(1),b(1),g(s(s(1))),bl,a(1),e(1),f(1),'A'(s(1)),a(1),bl].
%
:- dynamic supertonicB/2,supertonicC/2,supertonicD/2.
:- dynamic dominantB/2,dominantC/2,dominantD/2.
:- dynamic subdominantB/2,subdominantC/2,subdominantD/2.

dominant --> dominantA.
dominant --> dominantB.
dominant --> dominantC.
dominant --> dominantD.

subdominant --> subdominantA.
subdominant --> subdominantB.
subdominant --> subdominantC.
subdominant --> subdominantD.

supertonic --> supertonicA.
supertonic --> supertonicB.
supertonic --> supertonicC.
supertonic --> supertonicD.

dominantB --> dom(1),by_dom(1),dom(1), dom(A),dom(B), [bl], { sum_int(A, B, s(s(1))) }.
dominantC --> dom(s(1)),by_dom(1),dom(s(s(1))),[bl].
dominantD --> dom(1),by_dom(1),dom(1),dom(s(s(1))),[bl].

subdominantB --> subd(1),by_subd(1),subd(1), subd(A),subd(B), [bl], { sum_int(A, B, s(s(1))) }.
subdominantC --> subd(s(1)),by_subd(1),subd(s(s(1))),[bl].
subdominantD --> subd(1),by_subd(1),subd(1),subd(s(s(1))),[bl].

supertonicB --> supt(1),by_supt(1),supt(1), supt(A),supt(B), [bl], { sum_int(A, B, s(s(1))) }.
supertonicC --> supt(s(1)),by_supt(1),supt(s(s(1))),[bl].
supertonicD --> supt(1),by_supt(1),supt(1),supt(s(s(1))),[bl].

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
%   abc2ly and lilypond are neither installed on DICE, but
%   are freely available.
