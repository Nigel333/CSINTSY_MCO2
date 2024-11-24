:- dynamic gold/2.
:- dynamic player/2.
:- dynamic curr_gold/1.
:- dynamic cell_state/1.

size(5).
home(1, 1).
player(1, 1).
gold(2, 2).
gold(2, 5).
gold(5, 1).
gold(3, 4).
pit(3, 1).
pit(1, 4).
pit(4, 3).
curr_gold(0).

breeze(X, Y) :-
    adjacent(X, Y, AdjX, AdjY),
    pit(AdjX, AdjY).

glitter(X, Y) :-
    gold(X, Y).

safe(X, Y) :-
    (adjacent(X, Y, _, _), \+ pit(X, Y));
    adj_to_home(X, Y),
    !.

safe(_, _) :-
    write('You are in danger! Go back!'), nl,
    fail.

adjacent(X, Y, AdjX, AdjY) :-          
    (AdjX is X + 1, AdjY is Y);         
    (AdjX is X - 1, AdjY is Y);         
    (AdjX is X, AdjY is Y + 1);          
    (AdjX is X, AdjY is Y - 1).     

adj_to_home(X, Y) :-
    home(HX, HY),                    
    adjacent(HX, HY, X, Y).   

move(w) :-
    player(X, Y),             
    NewX is X-1,           
    NewY is Y,  
    safe(NewX, NewY),              
    retract(player(X, Y)),     
    assertz(player(NewX, NewY)).

move(a) :-
    player(X, Y),             
    NewX is X,           
    NewY is Y-1,  
    safe(NewX, NewY),              
    retract(player(X, Y)),     
    assertz(player(NewX, NewY)).

move(s) :-
    player(X, Y),             
    NewX is X+1,           
    NewY is Y,  
    safe(NewX, NewY),              
    retract(player(X, Y)),     
    assertz(player(NewX, NewY)).

move(d) :-
    player(X, Y),             
    NewX is X,           
    NewY is Y+1,  
    safe(NewX, NewY),              
    retract(player(X, Y)),     
    assertz(player(NewX, NewY)).

grab(g) :-
    player(X, Y),
    glitter(X, Y),
    retract(gold(X, Y)),
    curr_gold(Count),
    NewCount is Count + 1,
    retract(curr_gold(Count)),
    assertz(curr_gold(NewCount)),
    write('Gold grabbed!'), nl.

grab(g) :-
    player(X, Y),
    \+ gold(X, Y),                   
    write('No gold here to grab!'), nl.

leave(l) :-
    player(X, Y),
    home(X, Y),
    curr_gold(Count), 
    Count >= 2,               
    write('You successfully collected at least 2 gold coins and left the game!'), nl,
    retractall(player(_, _)),   
    retractall(curr_gold(_, _)). 

leave(l) :-
    player(X, Y),
    home(X, Y),
    curr_gold(Count),
    Count < 2,                 
    write('You cannot leave yet; you need at least 2 gold coins!'), nl,
    fail.

leave(l) :-
    write('You must return to Home to leave!'), nl,
    fail.

display_grid :-
    size(Size),
    forall(between(1, Size, Row),     
        (forall(between(1, Size, Col), 
            display_cell(Row, Col)),   
         nl)).                         

display_cell(X, Y) :-
    player(X, Y),
    write(' P ').     

display_cell(X, Y) :-
    home(X, Y), 
    write(' H ').   

display_cell(X, Y) :-
    write(' . ').

