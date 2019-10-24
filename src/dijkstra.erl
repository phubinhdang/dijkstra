%%%-------------------------------------------------------------------
%%% @author Phu Binh Dang (Dabi)
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Oct 2019 18:32
%%%-------------------------------------------------------------------
-module(dijkstra).
-author("dabi").

%% API
-export([dijkstra/3, lt/2]).

%% main function
%% Filename without extension
%% StartVertex : a vertex in graph
%% DU : directed(d) or undirected
%% function call example : dijkstra(graph_03,11,d)
dijkstra(Filename,StartVertex,DU)->
  G = adtgraph:importG(Filename,DU),
  L = createVertexAttrList(G,StartVertex,[]),
  ExistUnknownVertex = existUnknownVertex(L),
  findPath(ExistUnknownVertex,G,L)

.

%%%%%%%%%%%%%%%%%%%%%%% help method %%%%%%%%%%%%%%%%%%%

findPath(true, G, L)->
TupleOfVertex = findTupleOfClosestVertex(L),
  {Vertex,_,_,_} = TupleOfVertex,
  NewL = markVertexAsKnown(L,Vertex),
  Targets = adtgraph:getTarget(G,Vertex),
  updateOptimalDistance(G,NewL,Targets,TupleOfVertex)
;

% if found all shortest way to all vertexs (no unknown vertex) -> return result
findPath(false, _, L)-> extractResult(L).

% update optimal distances so far for all targets of a vertex
updateOptimalDistance(G,L,Targets,TupleOfVertex)->
  updateIterator(G,L,Targets,TupleOfVertex)
  .

% if all the targets vertex are checked,
% go back to outer loop (check and update all target of next vertex)
updateIterator(G,L,[],_TupleOfVertex)->
  findPath(existUnknownVertex(L),G,L);

% check and update if meet requirement to all the targets of examining vertex
updateIterator(G,L,[H|Tail],{Vertex,Known,Dist,Path})->
  Target = H,
  % determine distances
  StartToTarget = getDistance(L,Target),
  StartToVertex = Dist,
  VertexToTarget = adtgraph:getValE(G,Vertex,Target,weight),
  case StartToTarget > add(StartToVertex, VertexToTarget) of
    % if find shorter way
    true -> UpdatedL = updateDistAndPath(L,Target,add(StartToVertex, VertexToTarget),Vertex),
            updateIterator(G,UpdatedL,Tail,{Vertex,Known,Dist,Path});
    % else
    false -> updateIterator(G,L,Tail,{Vertex,Known,Dist,Path})
  end
.

% addition function
add(Dist1, Dist2) ->
  case (util:type_is(Dist1) == atom ) or (util:type_is(Dist2) == atom) of
    % if one of them is atom(infinity) -> infinity; bcs infinity + (num /or infinity) =  infinity
    true -> infinity;
    false -> Dist1 + Dist2
  end.

% update distance and path of a vertex, if shorter distance is found
% always findable
updateDistAndPath([],Vertex,_,_)->
  io:fwrite("not found ~0p in updateDistAndPath ~n",[Vertex]);
updateDistAndPath([{Vertex,Known,_,_}|Tail],Vertex,NewDist,NewPath)->
  [{Vertex,Known,NewDist,NewPath}|Tail];
updateDistAndPath([{VertexA,Known,Dist,Path}|Tail],VertexB,NewDist,NewPath)->
  [{VertexA,Known,Dist,Path}|updateDistAndPath(Tail,VertexB,NewDist,NewPath)]
.
% get distance of a vertex to start through its tuple
% always findable
getDistance([],Vertex)->
  io:fwrite("not found ~0p in getDistance ~n",[Vertex]);
getDistance([{Vertex,_,Dist,_}|_],Vertex)->
  Dist;
getDistance([{_,_,_,_}|Tail],Vertex)->
  getDistance(Tail,Vertex)
.

% return a result list
% [{vertex, known, distance, path},...] -> [{vertex, distance, path},...]
extractResult(L)-> extractResultHelper(L,[]).

extractResultHelper([],ResultList)-> ResultList;
extractResultHelper([{Vertex,_,Distance,Path}|Tail],[])->
  extractResultHelper(Tail,[{Vertex,Distance,Path}]);
extractResultHelper([{Vertex,_,Distance,Path}|Tail],[H|Rest])->
  extractResultHelper(Tail,[{Vertex,Distance,Path},H|Rest]).

% mark a vertex when the optimal way from start to it is known
markVertexAsKnown([],_)->
  [];
markVertexAsKnown([{Vertex,_,Dist,Path}|Tail],Vertex)->
  [{Vertex,true,Dist,Path}|Tail];
markVertexAsKnown([{VertexA,UnKnownA,DistanceA,PathA}|Tail],VertexB)->
  [{VertexA,UnKnownA,DistanceA,PathA}|markVertexAsKnown(Tail,VertexB)]
  .

% find a unknown vertex and it related attribute (known, cost, path)
% which is closest to start vertex so far
findTupleOfClosestVertex(L)->
findTupleHelper(L,{}).

% termination condition
findTupleHelper([], Tuple )-> Tuple;
% unknown closestVertex := first unknown vertex occurrence
findTupleHelper([{Vertex,false,Distance,Path}|Tail], {} )->
  findTupleHelper(Tail,{Vertex,false,Distance,Path});
findTupleHelper([{_,_,_,_}|Tail], {} )->
  findTupleHelper(Tail,{});

% unknown closestVertex = the next unknown vertex occurrence if its cost is smaller current cost
findTupleHelper([{Vertex1,false,Distance1,Path1}|Tail], {Vertex2,false,Distance2,Path2} )->
  case lt(Distance1,Distance2 ) of
    true -> findTupleHelper(Tail,{Vertex1,false,Distance1,Path1});
    false -> findTupleHelper(Tail,{Vertex2,false,Distance2,Path2})
  end;
findTupleHelper([{_,_,_,_}|Tail], {Vertex,false,Distance,Path} )->
  findTupleHelper(Tail,{Vertex,false,Distance,Path})
.

% less than function
lt(Dist1, Dist2) ->
  case (util:type_is(Dist1) == atom ) and (util:type_is(Dist2) == atom) of
    % if both distance is infinity (initial)
    true -> false;
    % if one of them is determined
    % in erlang : int < atom
    false -> Dist1 < Dist2
  end.

% check if there are still vertex, which is not examined for the optimal way
existUnknownVertex([]) ->
  false;
existUnknownVertex([{_,false,_,_} |_]) ->
  true;
existUnknownVertex([ _|Tail]) ->
  existUnknownVertex(Tail).

% create a list of tuples
% every tuple in form : {Vertex, Known, Distance, Path/Previous Vertex}
% Known : true/false; whether the optimal way from start vertex to this vertex is determined
% Distance : the cost to get there(Vertex) from start vertex
% Path : the last Vertex is gone through until Vertex is reached (from start vertex)
createVertexAttrList(G,StartVertex, List) ->
  VL = adtgraph:getVertices(G),
  util:globalvar(contains), util:setglobalvar(contains, contains(VL, StartVertex)),
  Contains = util:getglobalvar(contains),
  case Contains of
    true ->   setStartVetex(createVertexAttrHelper(VL,List),StartVertex);
    false -> io:fwrite("Sorry, ~0p is not in graph~n",[StartVertex]),exit("Error: StartVertex not in graph")
  end.

% create first tuple
createVertexAttrHelper([Vertex|Tail], []) ->
  createVertexAttrHelper(Tail,[{Vertex,false,infinity,-1}]);
% create all other tuples
createVertexAttrHelper([Vertex|Tail], [VertexAttr|Rest]) ->
  createVertexAttrHelper(Tail,[{Vertex,false,infinity,-1},VertexAttr|Rest]);
% termination condition
createVertexAttrHelper([], List) ->
  List.


%% set distance to start vertex to itself is 0
%% set prev vertex of start vertex is itself
setStartVetex([], _StartVertex) ->
  [];
setStartVetex([{StartVertex,Unknown,_,_}|T], StartVertex) ->
  [{StartVertex,Unknown,0,StartVertex}|T];
setStartVetex([{Vertex,Known,Cost,Path}|T], StartVertex) ->
  [{Vertex,Known,Cost,Path}|setStartVetex(T,StartVertex)].

% check if a vertex in vertexlist
contains([], _Vertex) ->
  false;
contains([Vertex|_], Vertex) ->
  true;
contains([_|T], Vertex) ->
  contains(T,Vertex)
.

