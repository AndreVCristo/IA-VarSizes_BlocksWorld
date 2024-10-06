:- use_module(library(clpfd)).



can(move(Block, From, To), [on(Block,From)|Conditions]) :-
    block(Block, Size),
    place(From),
    valid_region(To, Size),
    From \== To,
    clear_above(From, Size, [], ClearList),
    stable(To, Size, OccpList),
    append(ClearList, OccpList, Conditions).

adds(move(Block, From, To), [on(Block, To)|Conditions]) :-
    block(Block, Size),
    occ_positions(To, Size, [], OccList),       %occupied(To)
    clear_positions(From, Size, [], ClearList), %clear(From)
    append(OccList, ClearList, Conditions).

deletes(move(Block, From, To), [on(Block, From)|Conditions]) :-
    block(Block, Size),
    occ_positions(From, Size, [], OccList),   %occupied(From)
    clear_positions(To, Size, [], ClearList), %clear(To)
    append(OccList, ClearList, Conditions).







%gets the positions occupied by a block being placed
occ_positions(_, 0, List, List).
occ_positions((X,Y), Size, List, OccList) :-
    X2 #= X + 1,
    Size2 #= Size - 1,
    occ_positions((X2,Y), Size2, [occupied((X,Y))|List], OccList).

%gets the positions cleared by a block being moved
clear_positions(_, 0, List, List).
clear_positions((X,Y), Size, List, ClearList) :-
    X2 #= X + 1,
    Size2 #= Size - 1,
    clear_positions((X2,Y), Size2, [clear((X,Y))|List], ClearList).

%checks if a given region is within the grid
valid_region((X, Y), Size) :-
    X2 #= X + Size - 1,
    place((X2, Y)).

%gets the list of positions that must be clear for a block to move
clear_above(_, 0, List, List).
clear_above((X,Y), Size, List, ClearList) :-
    X2 #= X + 1,
    Y2 #= Y + 1,
    Size2 #= Size - 1,
    clear_above((X2,Y), Size2, [clear((X,Y2))|List], ClearList).

%gets the list of positions that must be occupied for a block to be placed
stable((X,Y), 1, [occupied((X,Y2))]) :-
    Y2 #= Y - 1.

stable((X,Y), 2, [occupied((X,Y2)), occupied((X2,Y2))]) :-
    Y2 #= Y - 1,
    X2 #= X + 1.

stable((X,Y), 3, [occupied((X2,Y2))]) :-
    Y2 #= Y - 1,
    X2 #= X + 1.
stable((X,Y), 3, [occupied((X,Y2)), occupied((X2,Y2))]) :-
    Y2 #= Y - 1,
    X2 #= X + 2.
    






% blocks world

block(a, 1).
block(b, 1).
block(c, 2).
block(d, 3).

% 6x4 grid
place((X, Y)) :-
    X >= 1,
    X =< 6,
    Y >= 1,
    Y =< 4.

% A possible representation for a state in the blocks world
%      4
%      3
%      2    ddd
%      1 cc a b 
%        123456
%  

state1([occupied((1,1)),
         clear((1,2)),
         clear((1,3)),
         clear((1,4)),
         
         occupied((2,1)),
         clear((2,2)),
         clear((2,3)),
         clear((2,4)),
         
         clear((3,1)),
         clear((3,2)),
         clear((3,3)),
         clear((3,4)),
         
         occupied((4,1)),
         occupied((4,2)),
         clear((4,3)),
         clear((4,4)),
         
         clear((5,1)),
         occupied((5,2)),
         clear((5,3)),
         clear((5,4)),
         
         occupied((6,1)),
         occupied((6,2)),
         clear((6,3)),
         clear((6,4)),
         
         occupied((1,0)),
         occupied((2,0)),
         occupied((3,0)),
         occupied((4,0)),
         occupied((5,0)),
         occupied((6,0)),
         
         clear((1,5)),
         clear((2,5)),
         clear((3,5)),
         clear((4,5)),
         clear((5,5)),
         clear((6,5)),
         
         on(a,(4,1)),
         on(b,(6,1)),
         on(c,(1,1)),
         on(d,(4,2))
       ]).





goal1([on(d,(1,2))]).








%----------------------------------------------------------
% plan(State, Goals, Plan, FinalState)
plan(State, Goals, [], State):-
    satisfied(State,Goals).
plan(State, Goals, Plan, FinalState):-
    append(Plan,_,_),
    append(PrePlan,[Action|PostPlan],Plan),
    select(State,Goals,Goal),
    achieves(Action,Goal),
    can(Action,Condition),
    plan(State,Condition,PrePlan,MidState1),
    apply(MidState1,Action,MidState2),
    plan(MidState2,Goals,PostPlan,FinalState).

%----------------------------------------------------------
% satisfied(State, goal): Goals are true in State
satisfied(_,[]).
satisfied(State,[Goal|Goals]):-
    member(Goal,State),
    satisfied(State,Goals).                 

%----------------------------------------------------------
select(State, Goals, Goal):-
    member(Goal,Goals),
    \+ member(Goal,State).                   % goal not true


%----------------------------------------------------------
% achieves(Action, Goal): goal is in add-list of Action 
achieves(Action, Goal):-
    adds(Action,Goals),
    member(Goal, Goals).

%----------------------------------------------------------
% apply(State, Action, NewState): execution of Action at 
%                                 State produces NewState
apply(State, Action, NewState):-
    deletes(Action, DelList), % get properties to be deleted
    delete_all(State,DelList, State1), !,
    adds(Action, AddList),
    append(AddList, State1, NewState).

%----------------------------------------------------------
% delete_all(L1,L2,Diff) Diff is set-difference of L1 and L2
delete_all([],_,[]).
delete_all([X|L1],L2,Diff):-
    member(X,L2), !,
    delete_all(L1,L2,Diff).
delete_all([X|L1],L2,[X|Diff]):-
    delete_all(L1,L2,Diff).


member(X,[X|_]).
member(X,[_|T]):-
     member(X,T).

delete(X,[X|Tail],Tail).
delete(X,[Y|Tail],[Y|Tail1]):- 
   delete(X, Tail,Tail1).