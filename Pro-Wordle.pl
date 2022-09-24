main:-
      
      write('Welcome to Pro-Wordle!'),nl,
	  write('----------------------'),nl,
	  build_kb,play.
	  


	  

build_kb:-
      write('Please enter a word and its category on separate lines:'),nl,
	  read(X),
	  (
	  X == done,nl,write("Done building the words database..."),nl,nl;
	  read(Y),
	  assert(word(X,Y)),
	  build_kb
	  ).
	  
	  
is_category(C):-
            word(_,C).
			


categories(L):-
            setof(C,is_category(C),L).

available_length(L):-
                 word(A,_),
				 atom_chars(A, X),
				 length(X,L).

pick_word(W,L,C):-
                word(W,C),
				atom_chars(W,X),
				length(X,L).

correct_letters(L1,[H|T],[H|T1]):-
                         member(H,L1),
						 correct_letters(L1,T,T1).
						 
correct_letters(L1,[H|T],L3):-
                         \+member(H,L1),
						 correct_letters(L1,T,L3).
correct_letters(_,[],[]).
						 
correct_positions([H|T],[H|T1],[H|T2]):-
                   correct_positions(T,T1,T2).
correct_positions([H|T],[X|T1],L3):-
                     H \= X,
                     correct_positions(T,T1,L3).
correct_positions([],[_],[]).
correct_positions([_],[],[]).
correct_positions([],[],[]).



choose_category(Category):-
		write('Choose a category:'),nl,
		read(C),
		(
		(is_category(C),Category=C)
		;
		(write('This category does not exist.'),nl,choose_category(Category))
		).
		
choose_length(Len,C,Word):-
		write('Choose a length'),nl,
		read(L),
		(
		(pick_word(Word,L,C),Len=L)
		;
		(write('There are no words of this length.'),nl,choose_length(Len,C,Word))
		).

		




guess(Word,Len,G,Status):-
		G > 0,
		write("Enter a word composed of "),write(Len),write(" letters:"),nl,
		read(Guess),atom_chars(Guess,GuessL),length(GuessL,LenGuess),
		(
		(LenGuess \== Len, 
		write("Word is not composed of "),write(Len),write(" letters. Try again."),nl,
		write("Remaining Guesses are "),write(G),nl,nl,guess(Word,Len,G,Status))
		
		;
		(
				(G1 is G - 1),
		(
		(Word==Guess,Status=win,write("You Won!"),nl)
		;
		(Word \== Guess,
		( (G1 == 0, Status=lose,write("You lost!"),nl)
		;
		((G1 > 0),atom_chars(Word,WordL),atom_chars(Guess,GuessL),
		correct_letters(WordL,GuessL,CL),
		correct_positions(WordL,GuessL,CP),
		write("Correct letters are: "),write(CL),nl,
		write("Correct letters in correct positions are: "),write(CP),nl,
		write("Remaining Guesses are "),write(G1),nl,nl,
		guess(Word,Len,G1,Status)
		) )	
		)
		)
		)
		).

play:-
			write("The available categories are: "),categories(L),write(L),nl,
			choose_category(C),choose_length(Len,C,Word),
			G is Len+1,write("Game started. You have "),write(G),write(" guesses."),nl,nl,guess(Word,Len,G,Status).