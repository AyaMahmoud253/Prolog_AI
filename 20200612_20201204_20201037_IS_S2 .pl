:-use_module(data,[friend/2]).
member1(X,[X|_]).

member1(X,[_|T]) :-
    member1(X,T).
list_length([],0).

list_length([_|TAIL],N) :-
    list_length(TAIL,N1),
    N is N1 + 1.

%Task1
is_friend(X, Y) :-
    friend(X, Y).
is_friend(X, Y) :-
    friend(Y, X).
%------------------------------------------------------------------------
%Task 2
friendList(X, L):-
    friendList(X, [], L).

friendList(X, Tmp, L):-
    friend(X, Y),
   \+ hasFriend(Y, Tmp),
    addToTmp(Y, Tmp, NewTmp),
    friendList(X, NewTmp, L).
friendList(_, L, L).

hasFriend(_, []):- fail.
hasFriend(X, [Y|_]):- X = Y, !.
hasFriend(X, [_|T]):- hasFriend(X, T).

addToTmp(X, [], [X]).
addToTmp(X, [H|T], [H|NewT]):-
    addToTmp(X, T, NewT).
%------------------------------------------------------------------------
%Task 3
friendList(X, L) :-
    friendList(X, [], L).

friendList(X, Tmp, L) :-
    friend(X, Y),
   \+ hasFriend(Y, Tmp),
    addToTmp(Y, Tmp, NewTmp),
    friendList(X, NewTmp, L).
friendList(_, L, L).

hasFriend(_, []) :- fail.
hasFriend(X, [Y|_]) :- X = Y, !.
hasFriend(X, [_|T]) :- hasFriend(X, T).

addToTmp(X, [], [X]).
addToTmp(X, [H|T], [H|NewT]) :-
    addToTmp(X, T, NewT).
friendListCount(Person,N):- friendList(Person,List),friendListCount(List,0,N).

%This predicate to count the result from the list
friendListCount([],Accu,Accu).
friendListCount([_|T],Accu,N):-
    Accu1 is Accu + 1,
    friendListCount(T,Accu1,N).
%------------------------------------------------------------------------
% Task 4
peopleYouMayKnow(X,N):-
  is_friend(X,Y),
  is_friend(Y,Z),
  \+is_friend(X,Z),
  \+ X=Z ,
  N = Z.
%---------------------------------------------------------------------
%Task 5
peopleYouMayKnow(P, N, SuggestedFriend) :-
    is_friend(P, MutualFriend),
    is_friend(MutualFriend, SuggestedFriend),
    \+is_friend(P, SuggestedFriend),
    SuggestedFriend \= P,
    mutual(P, SuggestedFriend, [], MutualFriends),
    list_length(MutualFriends, Count),
    Count >= N.
mutual(X, Y, Acc, MutualFriends) :-
    is_friend(X, Z),
    is_friend(Y, Z),
    \+ member1(Z, Acc),
    mutual(X, Y, [Z|Acc], MutualFriends).
mutual(_, _, MutualFriends, MutualFriends).
%---------------------------------------------------------------------
%Task 6
peopleYouMayKnowList(X, List):-
    peopleYouMayKnowList(X, [], List).

peopleYouMayKnowList(X,T,List):-
     is_friend(X, Z),
     is_friend(Z, Y),
     \+ is_friend(X, Y),
     \+ Y == X ,
     \+ member1(Y, T),
     !,
     peopleYouMayKnowList(X, [Y|T], List).

peopleYouMayKnowList(_, List, List).
%-------------------------------------------------------------------
%bonus Task
peopleYouMayKnow_indirect(Y,X):-
    friend(Y,M),
    friend(M,Z),
    friend(Z,X),
    \+ is_friend(Y,X),
    \+ peopleYouMayKnow(Y,X).
