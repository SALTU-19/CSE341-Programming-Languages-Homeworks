% Knowledge Base

schedule(canakkale, erzincan, 6). 
schedule(erzincan, antalya, 3). 
schedule(antalya, diyarbakir, 4).
schedule(antalya, izmir, 2).
schedule(diyarbakir, ankara, 8).
schedule(van, gaziantep, 3).
schedule(rize, ankara, 5).
schedule(corum,ankara, 2).
schedule(izmir, ankara, 6).
schedule(izmir, istanbul, 2).
schedule(ankara, istanbul, 1).
schedule(ankara, rize, 5).
schedule(ankara, van, 4).
schedule(istanbul, rize, 4).
schedule(yozgat,corum, 2).

% Rules

connection(X, Y, C) :- 
    shortest_way(X, Y, C). 

connection(X, Y, C) :-
    shortest_way(Y, X, C).
%%%
shortest_way(X, Y, C) :-     
    way(X, Y, C),      
    \+ (way(X, Y, C1),  
    C1 < C).            

way(X, Y, C) :- way(X, Y, C, []). 

way(X, Y, C, Travalled) :-
    \+ member(X, Travalled),     
    schedule(X, Y, C).            
    

way(X, Y, C, Travalled) :-
    \+ member(X, Travalled),        
    schedule(X, Z, C1),               
    way(Z, Y, C2, [X|Travalled]),    
    C is C1 + C2.                  