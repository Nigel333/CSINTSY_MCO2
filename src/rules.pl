% initializing dynamic predicates
:- dynamic gold/2.
:- dynamic player/2.
:- dynamic curr_gold/1.
:- dynamic safe_cells/2.
:- dynamic breeze_cells/2.
:- dynamic sus_cells/2. 

% initializing map and other static predicates
size(5).
home(1, 1).
player(1, 1).
gold(2, 2).
gold(2, 5).
gold(5, 1).
gold(4, 4).
pit(1, 4).
pit(3, 3).
pit(5, 3).
curr_gold(0).

% checks if the current cell is a breeze and adds it in breeze_cells if it isnt a part of it yet 
breeze(X, Y) :-
    adjacent(X, Y, AdjX, AdjY),
    pit(AdjX, AdjY),
    \+ breeze_cells(X, Y), 
    assertz(breeze_cells(X, Y)).

% checks if the current cell contains gold
glitter(X, Y) :-
    gold(X, Y).

% checks if the player has fallen into a pit and ends game if true
dead(X, Y) :- 
    player(X, Y), 
    pit(X, Y),
    write('You have fallen into a pit! Game over!'), nl,
    retractall(curr_gold(_, _)),
    retractall(safe_cells(_, _)),
    retractall(breeze_cells(_, _)),
    retractall(sus_cells(_, _)),
    process_move(exit),
    !.

% checks if the current cell is safe, specifically if its adjacent to home
safe(X, Y) :-
    within_map(X, Y), 
    adj_to_home(X, Y), 
    (\+ safe_cells(X, Y) -> assertz(safe_cells(X, Y)) ; true).

% checks if the current cell is safe, specifically if its a breeze, if it is mark the surrounding cells as suspicious 
% for potential pits (done using sus_cells to keep track)
safe(X, Y) :- 
    within_map(X, Y),
    breeze(X, Y), 
    (\+ sus_cells(NewX, Y), NewX is X + 1 -> assertz(sus_cells(NewX, Y)) ; true),
    (\+ sus_cells(NewX, Y), NewX1 is X - 1 -> assertz(sus_cells(NewX1, Y)) ; true),
    (\+ sus_cells(X, NewY), NewY is Y + 1 -> assertz(sus_cells(X, NewY)) ; true),
    (\+ sus_cells(X, NewY), NewY1 is Y - 1 -> assertz(sus_cells(X, NewY1)) ; true),
    !.

% checks if the current cell is safe, specifically by checking it its a pit. If its not a pit, then it is safe
% which is why it removes any sus_cells with the same coordinates
safe(X, Y) :-
    within_map(X, Y), 
    \+ pit(X, Y),
    (\+ safe_cells(X, Y) -> assertz(safe_cells(X, Y)) ; true),
    (sus_cells(X, Y) -> retract(sus_cells(X, Y)) ; true).

% this is a check that allows the code to continue even if it encounters an unsafe cell, this is specifically for the
% checking of adjacent cells of the current cell so it wont stop
safe(X, Y) :-
    pit(X, Y),
    (\+ sus_cells(X, Y) -> assertz(sus_cells(X, Y)) ; true).

% checks if its current cell is adjacent to another
adjacent(X, Y, AdjX, AdjY) :-          
    (AdjX is X + 1, AdjY is Y);         
    (AdjX is X - 1, AdjY is Y);         
    (AdjX is X, AdjY is Y + 1);          
    (AdjX is X, AdjY is Y - 1).     

% checks if the current cell is adjacent to home
adj_to_home(X, Y) :-
    home(HX, HY),                    
    adjacent(HX, HY, X, Y).   

% checks if the current cell is within the map
within_map(X, Y) :-
    size(Size),
    X >= 1, X =< Size,
    Y >= 1, Y =< Size.

% the next 4 move clauses checks if player wants to move up, left, down, right, itll check if the new cell is safe and if it is,
% itll move the player to that cell. itll also check the adjacent cells, then display the grid   
move(w) :-
    player(X, Y),             
    NewX is X-1,           
    NewY is Y,  
    safe(NewX, NewY),            
    retract(player(X, Y)),     
    assertz(player(NewX, NewY)),
    adj_safe(NewX, NewY),
    display_grid,
    flush_output.

move(a) :-  
    player(X, Y),             
    NewX is X,           
    NewY is Y-1,  
    safe(NewX, NewY),               
    retract(player(X, Y)),     
    assertz(player(NewX, NewY)),
    adj_safe(NewX, NewY),
    display_grid,
    flush_output.

move(s) :-
    player(X, Y),             
    NewX is X+1,           
    NewY is Y,  
    safe(NewX, NewY),               
    retract(player(X, Y)),     
    assertz(player(NewX, NewY)),
    adj_safe(NewX, NewY),
    display_grid,
    flush_output.

move(d) :-
    player(X, Y),             
    NewX is X,           
    NewY is Y+1,  
    safe(NewX, NewY), 
    retract(player(X, Y)),     
    assertz(player(NewX, NewY)),
    adj_safe(NewX, NewY),
    display_grid,
    flush_output.

% checks if the adjacent cells of the current cell are safe
adj_safe(X, Y) :-
    forall((adjacent(X, Y, AdjX, AdjY), within_map(AdjX, AdjY)),
           safe(AdjX, AdjY)).

% checks if the player is on a cell with gold, if it is, itll remove the gold from the cell and add it to the curr_gold count
% the clause after is used if the cell doesnt have gold
grab(g) :-
    player(X, Y),
    glitter(X, Y),
    retract(gold(X, Y)),
    curr_gold(Count),
    NewCount is Count + 1,
    retract(curr_gold(Count)),
    assertz(curr_gold(NewCount)),
    write('Gold grabbed!'), nl,
    display_grid.

grab(g) :-
    player(X, Y),
    \+ gold(X, Y),                   
    write('No gold here to grab!'), nl,
    display_grid.

% checks if the player is on the home cell and has at least 2 gold coins, if it does, itll end the game, else itll display error message
leave(l) :-
    player(X, Y),
    home(X, Y),
    curr_gold(Count), 
    Count >= 2,               
    write('You successfully collected at least 2 gold coins and left the game!'), nl,
    retractall(curr_gold(_, _)),
    retractall(safe_cells(_, _)),
    retractall(breeze_cells(_, _)),
    retractall(sus_cells(_, _)),
    !. 

leave(l) :-
    player(X, Y),
    home(X, Y),
    curr_gold(Count),
    Count < 2,                 
    write('You cannot leave yet; you need at least 2 gold coins!'), nl,
    display_grid,
    !,
    fail.

leave(l) :-
    write('You must return to Home to leave!'), nl,
    display_grid,
    !,
    fail.

% call this to reset game state, its static so change it if new game state is needed
reset :-
    retractall(gold(_, _)),
    retractall(safe_cells(_, _)),
    retractall(curr_gold(_)),
    retractall(player(_, _)),
    retractall(breeze_cells(_, _)),
    retractall(sus_cells(_, _)),
    assertz(player(1, 1)),
    assertz(gold(2, 2)),
    assertz(gold(2, 5)),
    assertz(gold(5, 1)),
    assertz(gold(3, 4)),
    assertz(curr_gold(0)).


% this is used to display the grid with the player, home, gold, safe, breeze, and sus/unknown cells
display_grid :-
    size(Size),
    forall(between(1, Size, Row),     
        (forall(between(1, Size, Col), 
            display_cell(Row, Col)),   
         nl)).                         

display_cell(X, Y) :- 
    player(X, Y), 
    breeze_cells(X, Y), 
    glitter(X, Y),
    write(' P,B,G ').  

display_cell(X, Y) :- 
    player(X, Y), 
    breeze_cells(X, Y), 
    write(' P,B  ').  

display_cell(X, Y) :- 
    player(X, Y), 
    glitter(X, Y), 
    write(' P,G  ').  

display_cell(X, Y) :-
    player(X, Y),
    write(' P    ').     

display_cell(X, Y) :-
    home(X, Y),
    write(' H    '). 

display_cell(X, Y) :- 
    safe_cells(X, Y),
    breeze_cells(X, Y),
    write(' S,B  ').  

display_cell(X, Y) :- 
    breeze_cells(X, Y), 
    write(' S    ').

display_cell(X, Y) :-
    player(X, Y),
    sus_cells(X, Y),
    safe_cells(X, Y),
    write(' P  '),
    retract(sus_cells(X, Y)).

display_cell(X, Y) :- 
    sus_cells(X, Y),
    write(' ?    ').

display_cell(X, Y) :-
    safe_cells(X, Y), 
    write(' S    ').

display_cell(_, _) :-
    write(' .    ').


% starts the game by initializing safe cells, displaying the grid, and starting the game loop. Call to start the game
start :- 
    reset,
    initialize_safe_cells,
    display_grid,
    game_loop.

game_loop :- 
    (player(X, Y), pit(X, Y)  ->  dead(X, Y)) ;
    write('Enter your move (w, a, s, d, g, l, exit): '),
    read(Move),
    process_move(Move).

process_move(Move) :- 
    (Move == w; Move == a; Move == s; Move == d), 
    move(Move),   
    game_loop.                              

process_move(Move) :- 
    Move == g,               
    grab(g),                
    game_loop.                   

process_move(Move) :- 
    Move == l,             
    leave(l),                
    !.     

process_move(exit) :- 
    write('Exiting game...'), nl.

process_move(_) :- 
    write('Invalid move!'), nl,
    game_loop.


% initializes safe cells adjacent to home
initialize_safe_cells :-
    home(HX, HY),
    forall((adjacent(HX, HY, X, Y), within_map(X, Y)),
           safe(X, Y)).


% used to print the safe and sus cells for manual checking
printsc :-
    write('Safe cells:'), nl,
    forall(safe_cells(X, Y), 
           format('~w, ~w~n', [X, Y])).

printsus :-
    write('sus cells:'), nl,
    forall(sus_cells(X, Y), 
           format('~w, ~w~n', [X, Y])).