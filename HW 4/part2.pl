% Knowledge Base

schedule(canakkale, erzincan, 6). 
schedule(erzincan, antalya, 3). 
schedule(antalya, diyarbakir, 4).
schedule(antalya, izmir, 2).
schedule(diyarbakir, ankara, 8).
schedule(izmir, ankara, 6).
schedule(izmir, istanbul, 2).
schedule(ankara, istanbul, 1).
schedule(ankara, rize, 5).
schedule(ankara, van, 4).
schedule(istanbul, rize, 4).
schedule(van, gaziantep, 3).
schedule(rize, ankara, 5).

% Rules

connection(X, Y, C) :- 
    shortest(X, Y, C). 

connection(X, Y, C) :-
    shortest(Y, X, C).
%%%
shortest(X, Y, C) :-     
    path(X, Y, C),      
    \+ (path(X, Y, C1),  
    C1 < C).            

path(X, Y, C) :- path(X, Y, C, []). 

path(X, Y, C, Visited) :-
    \+ member(X, Visited),     
    schedule(X, Y, C).            
    
% Next rule works recursively.
path(X, Y, C, Visited) :-
    \+ member(X, Visited),        
    schedule(X, Z, C1),               
    path(Z, Y, C2, [X|Visited]),    
    C is C1 + C2.                  