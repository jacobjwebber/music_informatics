%  interaction with abc files, translations back and forth
%  from Prolog grammar representation, and extraction of header info

%   Tested for sicstus 4.3.2 and SWI Prolog swipl 7.2.3

% getWriteHeader( Filename, HeaderFilename )
% reads from file Filename and writes header to file HeaderFilename

%% NB comment out next line if actually using sicstus ...

:- expects_dialect( sicstus ).

get_write_header( Filename, HeaderFileName ) :-
	open( Filename, read, Str ),
	get_header( Str, Header ),
	close( Str ),
	open( HeaderFileName, write, Str2 ),
        put_header( Str2, Header ),
	put_code( Str2, 10 ),             % new line
	close( Str2 ).

% next to select header info Header from input Stream (1st argument)

get_header( Str, [] ) :-  peek_char( Str, end_of_file ), !.
get_header( Str, Header) :- read_line( Str, ThisLine ),
	                    ( (ThisLine=[]; ThisLine=[_];
			           \+ ThisLine=[_,58|_])
			        -> Header=[]
			     ; process_line( ThisLine, NewLine ),
			       get_header( Str, RestHeader ),
			       Header=[NewLine|RestHeader]
			    ).


process_line( [84,58|Rest], [84,58|NewRest] ) :- !,
	                 atom_codes( 'based on: ', New ),
			 append( New, Rest, NewRest ).
process_line( L, L ).

put_header( _Str, [] ).
put_header( Str, [H] ) :-
	put_codes( Str, H ), !.
put_header( Str, [H|T] ) :-
	put_codes( Str, H ), nl(Str), put_header( Str, T ).
put_codes( _, [] ).
put_codes( Str, [H|T] ) :- put_code( Str, H ), put_codes( Str, T ).
	

% get note info from abc file, convert to grammar input format
% ignoring all the header info.

get_notes( Filename, Notes ) :- open( Filename, read, Str ),
	                   get_note_info( Str, Codes ),
			   close( Str ),
                           codes_to_notes( Codes, Notes ).

% getting raw Notes from input Stream (fst arg)

get_note_info( Str, [] ) :- peek_char( Str, end_of_file ), !.
get_note_info( Str, Notes ) :- read_line( Str, ThisLine ),
			( ThisLine = [_,58|_]  % 58 = ":"
			     -> get_note_info( Str, Notes )   % ignore ThisLine
			  ; (   get_note_info( Str, Rest ),   % else recurse and include
				append( ThisLine,Rest, Notes )
			    )                             
			).

% convert midi to notes as used in grammar; 

codes_to_notes( Codes, Notes ) :- c2n( Codes, NRev, [] ), reverse( NRev, Notes ).
c2n( [], Acc, Acc ).
c2n( [124|Rest],Res, Acc ) :- !, c2n( Rest, Res, [bl|Acc] ).
c2n( [X,Y|Rest],Res, Acc ) :- alpha(X), digit(Y), !,
	                      char2dig(Y,N), atom_codes(F,[X]), T =.. [F,N],
			      c2n( Rest, Res, [T|Acc] ).
c2n( [X|Rest],Res, Acc ) :- alpha(X), !,
	                    atom_codes(F,[X]), T =.. [F,1],
			    c2n( Rest, Res, [T|Acc] ).
c2n( [_|Rest],Res, Acc ) :- c2n( Rest, Res, Acc ).   % ignore everything else

char2dig( C, N ) :- digit(C)
	               -> (
			    NDig is C - "0",
			    dig2base(NDig,N)
			  )
                    ; (write( 'expecting digit char, saw '),
		       write(C), nl, fail).

alpha( X ) :- ("a" =< X , X =< "z"); ("A" =< X, X =< "Z").
digit( X ) :-  "0" =< X, X =< "9".

%% next for mode (+,?)
dig2base( 1, 1 ) :- !.
dig2base( ND, s(Y) ) :- N is ND - 1, dig2base( N, Y ).

base2dig( 1, 1 ) :- !.
base2dig( s(X), N ) :- base2dig( X, M ), N is M + 1.

% convert grammar notation for note sequences back to abc;
%
%  assumptions:
%  -- time unit is quaver (eighth note)
%  -- implicit c# and f#, no other accidentals

gram_to_abc( _, [] ).
gram_to_abc( Str, [H|T] ) :- g_note_to_abc( H, ABC ),
	                     put_codes( Str, ABC ),
			     gram_to_abc( Str, T ).

g_note_to_abc( bl, [124] ) :- !.
g_note_to_abc( Note, ABC ) :- Note =..[Pitch,Dur],
                              atom_codes( Pitch, P ),
			      base2dig( Dur, Dig ),
			      digit_to_code( Dig, DC ),
			      append( P, [DC], ABC ).

% write back to abc from abd header file and Prolog grammar notes
%
%  HeaderFile with appropriate abc header info;
%  GNotes list of notes as grammar terminals;
%  abc version of notes appended to the header file.

write_abc( HeaderFile, GNotes ) :-
                 	open( HeaderFile, append, Stream ),	
	                gram_to_abc( Stream, GNotes ),
			close( Stream ).

digit_to_code( Dig, C ) :- C is 48 + Dig.
