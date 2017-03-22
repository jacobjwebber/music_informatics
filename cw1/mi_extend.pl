% for music parsing

% runs under sicstus Prolog on DICE

% start Prolog ( command ' sicstus '  on DICE ),  and consult this file:
%
%  | ?- [mi_extend].
%

% This is a simple exampl in 6/8 time that was provided

input(Simple) :-
  Line1 = [d,f,f,d,b,d,bl,d,f,a,f,f,a,bl,d,g,'B',d,e,d,bl,d,f,'A',d,b,d,bl],
  Line2 = [d,f,f,d,b,d,bl,d,f,a,f,f,a,bl,d,g,'B',d,e,d,bl,d,f,'A',d,b,d,bl],
  append(Line1,Line2,Simple).


% show results up to 100 depth terms:

?- set_prolog_flag(toplevel_print_options,[quoted(true),numbervars(true),portrayed(true),max_depth(0)]).

% make dynamic

:- dynamic tune/2, line/2, bar1/2, bar/2, bar4/2, tonic/2, dominant/2.
:- dynamic subdominant/2, supertonic/2, ton/2, by_ton/2, dom/2, by_dom/2, subd/2, by_subd/2, supt/2, by_supt/2.



% This grammar was provided.

tune --> line, line.
line --> bar1, bar, bar, bar4.

bar1 --> tonic.
bar  --> tonic.
bar  --> subdominant.
bar  --> dominant.
bar4 --> tonic.

tonic --> ton,by_ton,ton,ton,by_ton,ton,[bl].
dominant --> dom,by_dom,dom,dom,by_dom,dom,[bl].
subdominant --> subd,by_subd,subd,subd,by_subd,subd,[bl].

ton --> [a].
ton --> [d].
ton --> [f].
ton --> ['A'].
by_ton --> [b].
by_ton --> ton.

dom --> [a].
dom --> [c].
dom --> [e].
dom --> ['A'].
by_dom --> [f].
by_dom --> dom.

subd --> [b].
subd --> [d].
subd --> [g].
subd --> ['B'].
by_subd --> [e].
by_subd --> subd.

%% end of grammar

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Two further examples accepted by unextended grammar
%
% 1. ['A',b,a,f,f,a,bl,f,b,f,a,d,'A',bl,c,e,a,e,e,a,bl,a,b,d,f,b,'A',bl,
% 'A',b,f,'A',b,'A',bl,b,d,g,d,g,'B',bl,g,b,'B',g,e,d,bl,f,b,'A',d,b,f,bl]
%
% 2.  ['A',f,f,'A',b,d,bl,a,f,e,e,f,e,bl,b,e,d,'B',b,b,bl,f,b,'A',a,b,f,bl,
% d,b,a,a,b,d,bl,g,b,d,d,e,b,bl,a,b,f,d,'A',f,bl,a,b,a,a,f,f,bl]
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Two examples not accepted by unextended grammar.
%
% 1. [z,b,z,f,z,a,bl,f,b,f,a,d,'A',bl,c,e,a,e,e,a,bl,a,b,d,f,b,'A',bl,
% 'A',b,f,'A',b,'A',bl,b,d,g,d,g,'B',bl,g,b,'B',g,e,d,bl,f,b,'A',d,b,f,bl]
% 
% I this case there are 'z' characters present. The letter 'z' does not 
% represent a pitch in Western musical notation. This example is obviously
% not a valid tune.
%
% 2. ['A',b,a,f,bl,f,b,f,a,bl,c,e,a,e,bl,a,b,d,f,bl,
% 'A',b,f,'A',bl,b,d,g,d,bl,g,b,'B',g,e,d,bl,f,b,'A',d,b,f,bl]
%
% In this sequence, some bars have only 4 notes. Having bars of different 
% duration is not typical of a tune and is therefore not parsed.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOT ACCEPTED BY THIS GRAMMAR.

%
% This grammar only accepts tunes whose chord sequence uses the I, IV and V
% chords. These are the most important chords for a major key, being all of the
% major chords. However, many tunes will use other chords.

% This grammar only accepts tunes in 6/8. Although this time signature is fairly
% common, it is still a very small subset of tunes.
%
% Below are two extensions to the grammar, each with an example melody that 
% is accepted by the extended grammar, but not the original.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ext 1.
%
%Here is an extension to allow chord ii, the supertonic.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Simple melody with supertonic
input(Super) :-
  Line1 = [d,f,f,d,b,d,bl,e,e,g,b,g,g,bl,d,g,'B',d,e,d,bl,d,f,'A',d,b,d,bl],
  Line2 = [d,f,f,d,b,d,bl,d,f,a,f,f,a,bl,d,g,'B',d,e,d,bl,d,f,'A',d,b,d,bl],
  append(Line1,Line2,Super).


bar --> supertonic.

supertonic --> supt,by_supt,supt,supt,by_supt,supt,[bl].

supt --> [e].
supt --> [g].
supt --> [b].
supt --> ['B'].
by_supt --> [c].
by_supt --> supt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ext 2.
%
% Here is an extension to allow for tunes in duple time.


% Each bar now consists of 2 beats. May be quaver chord note followed by quaver 
% non chord note or crochet. Crochet non chord notes on beat 2 are not permitted,
% although this could be considered stylistic in some cases.
% Note that while it would be simpler to just add duple features to previously defined
% bars, this would enable frequent switching between time signatures, which is not typical.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Simple melody in Duple time - an example not accepted by previous grammar.

input(Duple) :-
  Line1 = [d,a,f,d,bl,'2',b,d,g,bl,f,b,'2',a,bl,'2',d,a,b,bl],
  Line2 = [d,f,'2',a,bl,'2',d,'2','A',bl,b,e,g,g,bl,d,a,'A','A',bl],
  append(Line1,Line2,Duple).

% Test with:
% ?- input(Duple), tune(Duple, []).
% yes

%Make dynamic so random generation works.
:- dynamic line_duple/2, bar1_duple/2, bar_duple/2, bar4_duple/2, tonic_duple/2, dom_duple/2, subd_duple/2.
:- dynamic ton_beat/2, dom_beat/2, subd_beat/2.

tune --> line_duple, line_duple.
line_duple --> bar1_duple, bar_duple, bar_duple, bar4_duple.

bar1_duple --> tonic_duple.
bar_duple --> tonic_duple.
bar_duple --> dom_duple.
bar_duple --> subd_duple.
bar4_duple --> tonic_duple.

tonic_duple --> ton_beat,ton_beat,[bl].
dom_duple --> dom_beat, dom_beat,[bl].
subd_duple --> subd_beat, subd_beat,[bl].

ton_beat --> ton,by_ton.
ton_beat --> ['2'],ton. %Allow crochets according to abcnotation.
dom_beat --> dom,by_dom.
dom_beat --> ['2'],dom.
subd_beat --> subd,by_subd.
subd_beat --> ['2'],subd.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ALLOW GENERATION OF COMPLIANT TUNES.

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

% procedure to output query results to file

saveResult(Result,File) :-
    tell(File), write(Result), nl, told, nl,
    write('Result saved in '), write(File), write(' .'), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sample queries:

% to check input is parsed by this grammar:
% | ?- input(Input), tune(Input,[]).
% Input = [d,f,f,d,b,d,bl,d,f,a, ...
% yes
%
% for generation:
%
% | ?- tune( X, [] ).
% X = [a,b,a,a,b,a,bl,a,b,a,a,b,a,bl,a,b,a,a,b,a,bl,a,b,a,a,b,a,bl,a,b,a,a,b,a,bl,a,b,a,a,b,a,bl,a,b,a,a,b,a,bl,a,b,a,a,b,a,bl] ? ;
% X = [a,b,a,a,b,a,bl,a,b,a,a,b,a,bl,a,b,a,a,b,a,bl,a,b,a,a,b,a,bl,a,b,a,a,b,a,bl,a,b,a,a,b,a,bl,a,b,a,a,b,a,bl,a,b,a,a,b,d,bl]


% to call randomised:

% ?- random_tune( X, [] ).
% X = ['A',b,a,f,f,a,bl,f,b,f,a,d,'A',bl,c,e,a,e,e,a,bl,a,b,d,f,b,'A',bl,'A',b,f,'A',b,'A',bl,b,d,g,d,g,'B',bl,g,b,'B',g,e,d,bl,f,b,'A',d,b,f,bl] ? 
% yes
