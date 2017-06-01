win(X,[[X,X,X]|_]).
win(X,[[X|_],[X|_],[X|_]]).
win(X, [[X,_,_],[_,X,_],[_,_,X]]).
win(X,[[_,_,X],[_,X,_],[X,_,_]]).
win(X,[_|T]):- win(X,T).
win(X,[[_|T],[_|B],[_|C]]):- win(X,[T,B,C]).