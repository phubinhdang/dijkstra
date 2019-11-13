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
-export([dijkstra/3]).

%% main function
%% Filename without extension
%% StartVertex : a vertex in graph
%% DU : directed(d) or undirected
%% function call example : dijkstra(graph_03,11,d)
dijkstra(Filename,StartVertex,DU)->

  case util:type_is(Filename) == atom  of
%%     Main, filename is a filename
    true -> G = adtgraph:importG(Filename, DU),
      L = createL(G, StartVertex, []),
      Exist = existUnknownNode(L),
      findPath(Exist, G, L);
    false ->
%%      Filename is already a graph, just for runtime tests
      L = createL(Filename, StartVertex, []),
      Exist = existUnknownNode(L),
      findPath(Exist, Filename, L)
  end
.
%%%%%%%%%%%%%%%%%%%%%%% help method %%%%%%%%%%%%%%%%%%%

findPath(true, G, L)->
  TupleOfVertex = findClosestUnknownNode(L),
  {Vertex,_,_,_} = TupleOfVertex,
  NewL = markNodeAsKnown(L,Vertex),
  Targets = adtgraph:getTarget(G,Vertex),
  updateOptimalDistance(G,NewL,Targets,TupleOfVertex)
;

% if found all shortest way to all nodes (no unknown node) -> return result
findPath(false, _, L)-> extractResult(L).

% update optimal distances so far for all targets of a node
updateOptimalDistance(G,L,Targets,TupleOfVertex)->
  updateIterator(G,L,Targets,TupleOfVertex)
.

% if all the target-nodes are checked,
% go back to outer loop (check and update all target-nodes of next node)
updateIterator(G,L,[],_TupleOfVertex)->
  findPath(existUnknownNode(L),G,L);

% check and update if meet requirement to all the target-nodes of examining node
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
markNodeAsKnown([],_)->
  [];
markNodeAsKnown([{Vertex,_,Dist,Path}|Tail],Vertex)->
  [{Vertex,true,Dist,Path}|Tail];
markNodeAsKnown([{VertexA,UnKnownA,DistanceA,PathA}|Tail],VertexB)->
  [{VertexA,UnKnownA,DistanceA,PathA}| markNodeAsKnown(Tail,VertexB)]
.
% find a unknown vertex and it related attribute (known, cost, path)
% which is closest to start vertex so far
findClosestUnknownNode(L)->
  findTupleHelper(L,{}).

% termination condition
findTupleHelper([], Tuple )-> Tuple;
% unknown closestNode := first unknown node occurrence
findTupleHelper([{Vertex,false,Distance,Path}|Tail], {} )->
  findTupleHelper(Tail,{Vertex,false,Distance,Path});
findTupleHelper([{_,_,_,_}|Tail], {} )->
  findTupleHelper(Tail,{});

% unknown closestNode = the next unknown node occurrence if its cost is smaller than current nodes cost
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
existUnknownNode([]) ->
  false;
existUnknownNode([{_,false,_,_} |_]) ->
  true;
existUnknownNode([ _|Tail]) ->
  existUnknownNode(Tail).

% create a auxiliary list from graph for this algorithms
createL(G,StartVertex, List) ->
  VL = adtgraph:getVertices(G),
  util:globalvar(contains), util:setglobalvar(contains, contains(VL, StartVertex)),
  Contains = util:getglobalvar(contains),
  case Contains of
    true ->   setStartNode(createVertexAttrHelper(VL,List),StartVertex);
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


%% set distance to start node to itself is 0
%% set prev node of start node is itself
setStartNode([], _StartVertex) ->
  [];
setStartNode([{StartVertex,Unknown,_,_}|T], StartVertex) ->
  [{StartVertex,Unknown,0,StartVertex}|T];
setStartNode([{Vertex,Known,Cost,Path}|T], StartVertex) ->
  [{Vertex,Known,Cost,Path}| setStartNode(T,StartVertex)].

% check if a vertex in vertexlist
contains([], _Vertex) ->
  false;
contains([Vertex|_], Vertex) ->
  true;
contains([_|T], Vertex) ->
  contains(T,Vertex)
.

