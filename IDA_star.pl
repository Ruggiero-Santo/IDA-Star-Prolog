% Path              current search path (acts like a stack)
% Node              current node (last node in current path)
% G                 the cost to reach current node
% F                 estimated cost of the cheapest path (root..node..goal)
% h(node)           estimated cost of the cheapest path (node..goal)
% cost(node, succ)  step cost function
% successors(node)  node expanding function, expand nodes ordered by g + h(node)
% ida_star(root)    return either NOT_FOUND or a pair with the best path and its cost

%Graph
% edge(<id_start>, <id_end>, <weight>)
% Huge graph Example
% edge(32, 45, 7). edge(29, 45, 1). edge(17, 39, 8). edge(8, 34, 8).
% edge(21, 44, 6). edge(25, 31, 0). edge(6, 28, 9).  edge(2, 38, 5).
% edge(3, 39, 2).  edge(28, 46, 0). edge(23, 31, 2). edge(8, 25, 2).
% edge(27, 38, 7). edge(9, 35, 5).  edge(3, 22, 3).  edge(14, 21, 0).
% edge(18, 24, 2). edge(0, 28, 0).  edge(9, 25, 9).  edge(3, 25, 0).
% edge(7, 38, 4).  edge(21, 35, 9). edge(41, 48, 2). edge(12, 22, 9).
% edge(1, 4, 2).   edge(18, 33, 3). edge(10, 29, 4). edge(34, 40, 0).
% edge(24, 26, 6). edge(2, 40, 0).  edge(41, 46, 4). edge(1, 16, 2).
% edge(45, 48, 7). edge(3, 42, 8).  edge(14, 19, 1). edge(0, 25, 4).
% edge(25, 30, 8). edge(6, 23, 9).  edge(7, 48, 4).  edge(9, 21, 6).
% edge(10, 39, 4). edge(1, 45, 2).  edge(10, 21, 9). edge(22, 24, 6).
% edge(18, 49, 2). edge(17, 18, 4). edge(20, 28, 2). edge(27, 42, 8).
% edge(27, 32, 4). edge(7, 24, 0).  edge(4, 38, 5).  edge(6, 12, 5).
% edge(10, 27, 0). edge(6, 27, 0).  edge(5, 46, 6).  edge(20, 40, 1).
% edge(12, 43, 5). edge(12, 33, 2). edge(32, 42, 2). edge(32, 41, 7).
% edge(3, 14, 4).  edge(2, 21, 4).  edge(6, 10, 9).  edge(9, 49, 7).
% edge(38, 48, 3). edge(8, 16, 2).  edge(26, 41, 0). edge(7, 17, 6).
% edge(40, 48, 9). edge(31, 34, 1). edge(16, 19, 0). edge(17, 32, 5).
% edge(14, 29, 8). edge(5, 45, 7).  edge(15, 21, 1).
% Precomputed Path test

% ida_star(6, 5, Path, Cost);
% P = [6, 27, 10, 29, 45, 5],
% C = 13 

% ida_star(6, 34, Path, Cost);
% P = [6, 27, 10, 39, 3, 25, 31, 34],
% C = 8 

% ida_star(20, 4, Path, Cost);
% P = [20, 40, 2, 21, 14, 19, 16, 1, 4],
% C = 11  

% ida_star(1, 42, Path, Cost);
% P = [1, 45, 32, 42],
% C = 12 


% Mini Graph for manual test
edge(1,2,4). edge(1,3,3). edge(4,1,1). 
edge(2,3,1). edge(2,5,6). edge(3,4,3).
edge(3,5,3). edge(4,5,4). edge(5,6,2).


%%%%%%% Graph Functions %%%%%%%

% True if nodes(X, Y) are connected
%   connected(+X, +Y)
%connected(X,Y) :- edge(X,Y,_); edge(Y,X,_).
connected(X,Y) :- edge(X,Y,_). % Grafo Direzionato


% Get cost/weight of edge between two node, if exist
%   cost(+X, +Y, -Weight)
%cost(X, Y, Weight):- edge(X,Y,Weight); edge(Y,X,Weight).
cost(X, Y, Weight):- edge(X,Y,Weight). % Grafo Direzionato

% Get list of successors of Node
successors(Node, List):-
    setof(Y, connected(Node, Y), List).



%%%%%%%%%%% Utility %%%%%%%%%%%

% Take min of X and Y, inf is the infinite value
%   min(+X, +Y, - Min)
min(inf, inf, Min):- Min = inf.
min(inf, Y, Min):- Min is Y.
min(X, inf, Min):- Min is X.
min(X, Y, Min):- Min is min(X,Y).

% Make a reverse of list
%   reverse(+List, -List_Rev)
reverse([],Z,Z).
reverse([X | List], List_Rev, Acc) :- reverse(List, List_Rev, [X|Acc]).
reverse(List, List_Rev):- reverse(List, List_Rev, []).

% True if Elem is in List
%   member(+Elem, +List)
member(_, []):- false.
member(Elem, Elem):- true.
member(Elem, [Elem| _]):- true.
member(Elem, [_ | List]):- member(Elem, List).

% Make a exclusion filter, remove Element from List that respect the Goal
%   exclude(+Goal, +List, -Res_List)
exclude(Goal, List, Res_List):-
    exclude_(Goal, List, Res_List).
    
exclude_(_, [], []).
exclude_(Goal, [X | List], Res_List) :-
    (   call(Goal, X)
    ->  Res_List = Tmp_List
    ;   Res_List = [X | Tmp_List]),
    exclude_(Goal, List, Tmp_List).

% True if list contain Elem
%   have(+List, +Elem)
have(List, Elem):- member(Elem, List).

% Append at the List the Element 
%   append(+List, + Elem, -New_List)
append(List, Elem, New_List):-
    reverse(List, Tmp_List),
    reverse([Elem| Tmp_List], New_List).

% Get last element of the List
%   get_last(+List, -Elem)
get_last(List, Last):-
    reverse(List, [Last | _]).


%%%%%%%%%%%  Search  %%%%%%%%%%%

% H function, is an Heuristic, estimated cost of the cheapest path (node..goal)
% No particular heuristics have been implemented so the estimated cost is always 1
%   h(+Node, +Goal, -Result)
h(_, _, Result):- Result is 1.

% F function, estimated cost of the cheapest path (root..node..goal), use h function
%   f(+Bound, +Node, -F) 
f(Bound, Node, Goal, F):-
    h(Node, Goal, H), F is Bound + H.

% Start function to compute ida_star algoritm.
ida_star(Root, End, Path, Cost):-
    h(Root, End, Bound),
    ida_star_([Root], End, Bound, _, Path, Cost).


ida_star_(P, _, T, Old_T, P, Old_T):- T == f, !.
ida_star_(Path, End, Bound, _, Res_Path, Res_Cost):-
    search_next(Path, End, 0, Bound, N_Path, N_Bound),
    ida_star_(N_Path, End, N_Bound, Bound, Res_Path, Res_Cost).


search_next(Path, End, G, Bound, N_Path, T):-
    get_last(Path, Last),
    search_(Path, End, Last, G, Bound, N_Path, T).

search_(_, End, Last, G, Bound, _, F):-
    f(G, Last, End, F), F > Bound, !.
search_(P, End, End, _, _, P, T):-
    T = f, !.
search_(Path, End, Last, G, Bound, N_Path, T):-
    successors(Last, Succ),
    exclude(have(Path), Succ, Successors),
    n_ss(Successors, End, Last, G, Path, Bound, N_Path, T).


n_ss(Successors, End, Last, G, Path, Bound, Res_Path, T):-
    ss_(Successors, End, Last, G, Path, Bound, inf, Res_Path, T).

ss_([], _, _, _, Path, _, Min, Path, Min):- !. % search for all Succ
ss_([], _, _, _, _, _, _, _, _):- !. %found
ss_([Node | List], End, Last, G, Path, B, Min, New_Path, T):-
    cost(Last, Node, W),
    append(Path, Node, Temp_Path),
    search_next(Temp_Path, End, G+W, B, Res_Path, R),
    (   R == f
    ->  List1 = [], var(New_Path), T = R, New_Path = Res_Path
    ;   List1 = List, min(Min, R, Min1)),
    ss_(List1, End, Last, G, Path, B, Min1, New_Path, T).