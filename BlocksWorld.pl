:- use_module(library(clpfd)).

%movimentos
%-----------------------------------------------------------------------
%mover um bloco de tamanho 1
can(move1(Block, From, To), [on(Block,From)|Conditions]) :-
    block(Block, 1),
    place(From),
    place(To),
    From \== To,
    clear_above(From, 1, [], ClearList),
    stable(To, From, 1, OccpList),
    append([clear(To)|ClearList], OccpList, Conditions).

%mover um bloco de tamanho 2
can(move2(Block, From, To), [on(Block,From)|Conditions]) :-
    block(Block, 2),
    place(From),
    valid_region(To, 2),
    From \== To,
    clear_above(From, 2, [], ClearList1),
    clear_positions(To, 2, [], ClearList2),
    append(ClearList1, ClearList2, ClearList),
    stable(To, From, 2, OccpList),
    append(ClearList, OccpList, Conditions).

%mover um bloco de tamanho 3
can(move3(Block, From, To), [on(Block,From)|Conditions]) :-
    block(Block, 3),
    place(From),
    valid_region(To, 3),
    From \== To,
    clear_above(From, 3, [], ClearList1),
    clear_positions(To, 3, [], ClearList2),
    append(ClearList1, ClearList2, ClearList),
    stable(To, From, 3, OccpList),
    append(ClearList, OccpList, Conditions).




adds(move1(Block, From, To), [on(Block, To), occupied(To), clear(From)]).

adds(move2(Block, From, To), [on(Block, To)|Conditions]) :-
    occ_positions(To, 2, [], OccList),       %occupied(To)
    clear_positions(From, 2, [], ClearList), %clear(From)
    append(OccList, ClearList, Conditions).

adds(move3(Block, From, To), [on(Block, To)|Conditions]) :-
    occ_positions(To, 3, [], OccList),       %occupied(To)
    clear_positions(From, 3, [], ClearList), %clear(From)
    append(OccList, ClearList, Conditions).




deletes(move1(Block, From, To), [on(Block, From), occupied(From), clear(To)]).

deletes(move2(Block, From, To), [on(Block, From)|Conditions]) :-
    occ_positions(From, 2, [], OccList),   %occupied(From)
    clear_positions(To, 2, [], ClearList), %clear(To)
    append(OccList, ClearList, Conditions).

deletes(move3(Block, From, To), [on(Block, From)|Conditions]) :-
    occ_positions(From, 3, [], OccList),   %occupied(From)
    clear_positions(To, 3, [], ClearList), %clear(To)
    append(OccList, ClearList, Conditions).
%-----------------------------------------------------------------------





%gets the positions occupied by a block being placed
occ_positions(_, 0, List, List).
occ_positions((X,Y), Size, List, OccList) :-
    Size >= 1,
    X2 #= X + 1,
    Size2 is Size - 1,
    occ_positions((X2,Y), Size2, [occupied((X,Y))|List], OccList).

%gets the positions cleared by a block being moved
clear_positions(_, 0, List, List).
clear_positions((X,Y), Size, List, ClearList) :-
    Size >= 1,
    X2 #= X + 1,
    Size2 is Size - 1,
    clear_positions((X2,Y), Size2, [clear((X,Y))|List], ClearList).

%checks if a given region is within the grid
valid_region((X, Y), Size) :-
    X2 #= X + Size - 1,
    place((X2, Y)).

%gets the list of positions that must be clear for a block to move
clear_above(_, 0, List, List).
clear_above((X,Y), Size, List, ClearList) :-
    Size >= 1,
    X2 #= X + 1,
    Y2 #= Y + 1,
    Size2 is Size - 1,
    clear_above((X2,Y), Size2, [clear((X,Y2))|List], ClearList).

%gets the list of positions that must be occupied for a block to be placed
stable((X,Y), (Xblock,Yblock), 1, [occupied((X,Y2))]) :-
    Y2 #= Y - 1,
    %a block cannot be placed above itself
    (Xblock #\= X;
     Yblock #\= Y2).

stable((X,Y), (Xblock,Yblock), 2, [occupied((X,Y2)), occupied((X2,Y2))]) :-
    Y2 #= Y - 1,
    X2 #= X + 1,
    Xblock2 #= Xblock + 1,
     %the left support of the block cannot be its own left
    (Xblock #\= X;
     Yblock #\= Y2),
    %the right support of the block cannot be its own left
    (Xblock #\= X2;
     Yblock #\= Y2),
    %the left support of the block cannot be its own right
    (Xblock2 #\= X;
     Yblock #\= Y2).

stable((X,Y), (Xblock,Yblock), 3, [occupied((X2,Y2))]) :-
    Y2 #= Y - 1,
    X2 #= X + 1,
    Xblock2 #= Xblock + 1,
    Xblock3 #= Xblock2 + 1,
    %the support of the block cannot be its own left
    (Xblock #\= X2;
     Yblock #\= Y2),
    %the support of the block cannot be its own middle
    (Xblock2 #\= X2;
     Yblock #\= Y2),
    %the support of the block cannot be its own right
    (Xblock3 #\= X2;
     Yblock #\= Y2).
stable((X,Y), (Xblock,Yblock), 3, [occupied((X,Y2)), occupied((X2,Y2))]) :-
    Y2 #= Y - 1,
    X2 #= X + 2,
    Xblock2 #= Xblock + 1,
    Xblock3 #= Xblock2 + 1,
    %the left support of the block cannot be its own left
    (Xblock #\= X;
     Yblock #\= Y2),
    %the left support of the block cannot be its own middle
    (Xblock2 #\= X;
     Yblock #\= Y2),
    %the left support of the block cannot be its own right
    (Xblock3 #\= X;
     Yblock #\= Y2),

    %the right support of the block cannot be its own left
    (Xblock #\= X2;
     Yblock #\= Y2),
    %the right support of the block cannot be its own middle
    (Xblock2 #\= X2;
     Yblock #\= Y2).
    






% blocks world

block(a, 1).
block(b, 1).
block(c, 2).
block(d, 3).

% 6x4 grid
place((1, 1)).
place((1, 2)).
place((1, 3)).
place((1, 4)).

place((2, 1)).
place((2, 2)).
place((2, 3)).
place((2, 4)).

place((3, 1)).
place((3, 2)).
place((3, 3)).
place((3, 4)).

place((4, 1)).
place((4, 2)).
place((4, 3)).
place((4, 4)).

place((5, 1)).
place((5, 2)).
place((5, 3)).
place((5, 4)).

place((6, 1)).
place((6, 2)).
place((6, 3)).
place((6, 4)).

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





goal1([on(a,(1,2)),
        on(b,(6,1)),
        on(c,(1,1)),
        on(d,(3,1))
      ]).








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