%%%-------------------------------------------------------------------
%%% @author Phu Binh Dang, Thomas Jaßen
%%% @doc
%%%
%%% @end
%%% Created : 11. Oct 2019 11:12
%%%-------------------------------------------------------------------
-module(adtgraph).
-author("dabi").

%% API
-export([addVertex/2, createG/1, addEdge/3, deleteEdge/3, deleteVertex/2, setAtE/5, setAtV/4, getValE/4, getValV/3, getIncident/2, getAdjacent/2, getTarget/2, getSource/2, getEdges/1, getVertices/1, importG/2, exportG/2, printG/2, calcTime/3, gentestgraph/3, testTimeAll/4]).
%%VL: [ {<VertexID von A>,[<Name von A >,<Value von A>, <ggf. weitere Attribute von A>] }]
%EL[ { {VertexID von A, VertexID von B}, [<Name von AB>, <Value von AB>,<ggf. weitere Attribute von AB>] }]

%%createG: [d|ud] → graph                                                                    /createG(<d|ud>)
createG(ud) -> util:globalvar(yndirected), util:setglobalvar(yndirected, nondirected), {ud,[],[]};
createG(d) -> util:globalvar(yndirected), util:setglobalvar(yndirected, directed), {d,[],[]}.

%%addVertex: graph × vertex → graph   /addVertex(<graph>,<vertex>)
addVertex({DU, VL, EL}, Vertex) ->
  {DU,addVertexHelper(VL,Vertex),EL}.

%%helper method
%%if vertex already in VL
addVertexHelper([{Vertex,Attr}|Tail], Vertex) ->
  [{Vertex,Attr}|Tail];
%%if vertex already not in VL
addVertexHelper([{Vertex,Attr}|Tail], OVertex) ->
  [{Vertex,Attr}|addVertexHelper(Tail,OVertex)];
%%if VL empty (initial)
addVertexHelper([], Vertex) ->
  [{Vertex,[]}].

%%deleteVertex: graph × vertex → graph /deleteVertex(<graph>,<vertex>)
%% delete all related edges of Vertex
deleteVertex({DU, VL, EL}, Vertex) ->
  {DU,deleteVertexHelper(VL,Vertex), deleteRelatedEdges(EL,Vertex)}.

%%helper method
%%if vertex in VL
deleteVertexHelper([{Vertex,_}|Tail], Vertex) ->
  Tail;
%%if vertex not in VL
deleteVertexHelper([{Vertex,Attr}|Tail], OVertex) ->
  [{Vertex,Attr}|deleteVertexHelper(Tail,OVertex)];
%%if VL empty (initial)
deleteVertexHelper([], _) ->
  [].

%%helper method
%% empty VL
deleteRelatedEdges([],_Vertex) -> [];
% vertex as source
%% vertex =  source node
deleteRelatedEdges([{{Vertex,_},_}|Tail],Vertex)->
  deleteRelatedEdges(Tail,Vertex);
% vertex as target
%% vertex =  target node
deleteRelatedEdges([{{_,Vertex},_}|Tail],Vertex) ->
  deleteRelatedEdges(Tail,Vertex);
% not source not equal
%% vertex != source & vertex != target
deleteRelatedEdges([{{VertexA,VertexB},Attr}|Tail],Vertex) ->
  [{{VertexA,VertexB},Attr}|deleteRelatedEdges(Tail,Vertex)].


%%addEdge: graph × vertex × vertex → graph
%% add Edge
addEdge({DU, VL, EL}, Vertex , OVertex ) ->
  %% add vertexs to VL if needed
  G = addVertex(addVertex({DU, VL, EL}, Vertex), OVertex),
  %new DU, new VL, new EL
  {NDU,NVL, NEL} = G,
  {NDU,NVL,addEdgeUndirectedHelper(NDU,NEL, Vertex, OVertex)}
  .
% empty EL
addEdgeUndirectedHelper(_,[],Vertex, OVertex)->
  [{{Vertex,OVertex},[]}];
% same edge same order
addEdgeUndirectedHelper(_,[{{VertexA,VertexB},Attr}|Tail],VertexA, VertexB)->
  [{{VertexA,VertexB},Attr}|Tail];
% same edge in reverse order, directed -> add new edge
addEdgeUndirectedHelper(d,[{{VertexA,VertexB},Attr}|Tail],VertexB, VertexA)->
[{{VertexA,VertexB},Attr}|addEdgeUndirectedHelper(d,Tail, VertexB, VertexA)];
% same edge in reverse order, undirected -> do not add new edge
addEdgeUndirectedHelper(_,[{{VertexA,VertexB},Attr}|Tail],VertexB, VertexA)->
  [{{VertexA,VertexB},Attr}|Tail];
% dont match
addEdgeUndirectedHelper(DU,[{{VertexA,VertexB},Attr}|Tail],Vertex, OVertex)->
  [{{VertexA,VertexB},Attr}|addEdgeUndirectedHelper(DU,Tail, Vertex, OVertex)].

%%deleteEdge: graph × vertex × vertex → graph
deleteEdge({UD,VL,EL},Vertex, OVertex)->
  {UD,VL,deleteEdgeHelper(EL,Vertex,OVertex)}
  .
%%empty EL
deleteEdgeHelper([],_,_) ->
  [];
%% match both
deleteEdgeHelper([{{VertexA,VertexB},_}|Tail],VertexA,VertexB) ->
  Tail;
%% else
deleteEdgeHelper([{Vertexs,Attr}|Tail],Vertex,OVertex) ->
  [{Vertexs,Attr}|deleteEdgeHelper(Tail,Vertex,OVertex)].

%%setAtE: graph × vertex × vertex × name × value → graph
setAtE({DU,VL,EL}, Vertex, OVertex, Name, Value)->
  {DU,VL,setAtEdgeHelper(EL,Vertex, OVertex, Name, Value)}
.
% empty EL
setAtEdgeHelper([], _Vertex, _OVertex, _Name, _Value) ->
  [];
% match both
setAtEdgeHelper([{{Vertex,OVertex},_}|Tail], Vertex, OVertex, Name, Value) ->
  [setEdgeNewAttributes({Vertex,OVertex}, Name, Value)|Tail];
% else
setAtEdgeHelper([{Vertexs,Attr}|Tail], Vertex, OVertex, Name, Value) ->
  [{Vertexs,Attr}|setAtEdgeHelper(Tail,Vertex, OVertex, Name, Value)].

% set /overwrite old attributes
setEdgeNewAttributes({Vertex,OVertex}, Name, Value)->
  {{Vertex,OVertex},[Name,Value]}.

%%setAtV: graph × vertex × name × value → graph
setAtV({DU, VL, EL}, Vertex, Name, Value) ->
  {DU,setAtVertexHelper(VL,Vertex, Name, Value),EL}.
% empty VL
setAtVertexHelper([],_Vertex, _Name, _Value)->
  [];
% vertex in VL
setAtVertexHelper([{Vertex,_}|Tail],Vertex, Name, Value)->
  [setVertexNewAttributes(Vertex,Name, Value)|Tail];
% else
setAtVertexHelper([{Vertex,Attr}|Tail],OVertex, Name, Value)->
  [{Vertex,Attr}|setAtVertexHelper(Tail, OVertex,Name, Value)].

% set /overwrite old attributes
setVertexNewAttributes(Vertex,Name, Value)->
  {Vertex,[Name,Value]}.

%%getValE: graph × vertex × vertex × name → value
getValE({DU,_VL,EL}, Vertex, OVertex, Name)->
  getValueEdgeHelper(DU,EL,Vertex,OVertex,Name).

% empty VL
getValueEdgeHelper(_,[],_Vertex,_OVertex,_Name) ->
  nil;
% match source, target and name
getValueEdgeHelper(d,[{{Vertex,OVertex},[Name,Value]}|_Tail],Vertex,OVertex,Name) ->
  Value;
getValueEdgeHelper(ud,[{{Vertex,OVertex},[Name,Value]}|_Tail],Vertex,OVertex,Name) ->
  Value;
getValueEdgeHelper(ud,[{{Vertex,OVertex},[Name,Value]}|_Tail],OVertex,Vertex,Name) ->
  Value;
%else
getValueEdgeHelper(DU,[{_,_}|Tail],Vertex,OVertex,Name) ->
  getValueEdgeHelper(DU,Tail,Vertex,OVertex,Name).


%%getValV: graph × vertex × name → value
getValV({_DU,VL,_EL}, Vertex, Name)->
  getValueVertexHelper(VL,Vertex,Name)
.

% empty VL
getValueVertexHelper([],_Vertex,_Name)->
  nil;
% match vertex and name
getValueVertexHelper([{Vertex,[Name,Value]}|_Tail],Vertex,Name)->
  Value;

%else
getValueVertexHelper([{_,_}|Tail],Vertex,Name)->
  getValueVertexHelper(Tail,Vertex,Name).

%%getIncident: graph × vertex → vertexlist
%% alle benachbarte Knoten
% empty EL
getIncident({_DU,_VL,[]}, _Vertex)->
    [];
% directed
getIncident({d,_VL,EL}, Vertex)->
  getIncidentDirectedHelper(EL,Vertex,[]);

% undirected
getIncident({ud,_VL,EL}, Vertex)->
  getIncidentUndirectedHelper(EL,Vertex,[]).

%empty EL
getIncidentDirectedHelper([],_Vertex,VertexList)->
  VertexList;
%  match second vertex
getIncidentDirectedHelper([{{VertexA, Vertex },_}|Tail],Vertex,VertexList)->
  getIncidentDirectedHelper(Tail,Vertex,[VertexA,Vertex|VertexList]);
getIncidentDirectedHelper([{{Vertex, VertexB },_}|Tail],Vertex,VertexList)->
  getIncidentDirectedHelper(Tail,Vertex,[Vertex,VertexB|VertexList]);

% dont match
getIncidentDirectedHelper([{{_, _ },_}|Tail],Vertex,VertexList)->
  getIncidentDirectedHelper(Tail,Vertex,VertexList).

%empty EL
getIncidentUndirectedHelper([],_Vertex,VertexList)->
  VertexList;
% match first vertex
getIncidentUndirectedHelper([{{Vertex, VertexB },_}|Tail],Vertex,VertexList)->
  getIncidentUndirectedHelper(Tail,Vertex,[Vertex,VertexB|VertexList]);
%  match second vertex
getIncidentUndirectedHelper([{{VertexA, Vertex },_}|Tail],Vertex,VertexList)->
  getIncidentUndirectedHelper(Tail,Vertex,[Vertex,VertexA|VertexList]);
% dont match
getIncidentUndirectedHelper([{{_, _ },_}|Tail],Vertex,VertexList)->
  getIncidentUndirectedHelper(Tail,Vertex,VertexList).

%%getAdjacent: graph × vertex → vertexlist
% independent of direction
% empty EL
getAdjacent({_DU,_VL,[]}, _Vertex)->
  [];
% Adjacent in both cases is actually Incident in undirected graph
getAdjacent({_DU,_VL,EL}, Vertex)->
  getAdjacentHelper(EL,Vertex,[]).

getAdjacentHelper([],_Vertex,VertexList)->
  VertexList;
% match first vertex
getAdjacentHelper([{{Vertex, VertexB },_}|Tail],Vertex,VertexList)->
  getAdjacentHelper(Tail,Vertex,addToSet(VertexList,VertexB));
%  match second vertex
getAdjacentHelper([{{VertexA, Vertex },_}|Tail],Vertex,VertexList)->
  getAdjacentHelper(Tail,Vertex,addToSet(VertexList,VertexA));
% dont match
getAdjacentHelper([{{_, _ },_}|Tail],Vertex,VertexList)->
  getAdjacentHelper(Tail,Vertex,VertexList).

%%getTarget:  graph × vertex → vertexlist
getTarget({DU,_VL,EL}, Vertex)->
  getTargetHelper(DU,EL, Vertex).

%empty EL
getTargetHelper(_DU,[], _Vertex)->
  [];
% undirected
getTargetHelper(ud, EL, Vertex)->
  getAdjacentHelper(EL,Vertex,[]);
getTargetHelper(d, EL, Vertex)->
  % getIncident and delete all source nodes(vertex)
  getTargetDirectedHelper(EL,Vertex,[]).

%empty EL
getTargetDirectedHelper([],_Vertex,VertexList)->
  VertexList;
% match first vertex
getTargetDirectedHelper([{{Vertex, VertexB },_}|Tail],Vertex,VertexList)->
  getTargetDirectedHelper(Tail,Vertex,[VertexB|VertexList]);
%  match second vertex
% dont match
getTargetDirectedHelper([{{_, _ },_}|Tail],Vertex,VertexList)->
  getTargetDirectedHelper(Tail,Vertex,VertexList).

%%getSource: graph × vertex → vertexlist
getSource({DU,_VL,EL}, Vertex)->
  getSourceHelper(DU,EL, Vertex).
% empty EL
getSourceHelper(_DU,[], _Vertex)->
  [];
% undirected
getSourceHelper(ud, EL, Vertex)->
  getAdjacentHelper(EL,Vertex,[]);
getSourceHelper(d, EL, Vertex)->
  getSourceDirectedHelper(EL,Vertex,[]).

%empty EL
getSourceDirectedHelper([],_Vertex,VertexList)->
  VertexList;
% match first vertex
getSourceDirectedHelper([{{VertexA, Vertex },_}|Tail],Vertex,VertexList)->
  getSourceDirectedHelper(Tail,Vertex,[VertexA|VertexList]);
%  match second vertex
% dont match
getSourceDirectedHelper([{{_, _ },_}|Tail],Vertex,VertexList)->
  getSourceDirectedHelper(Tail,Vertex,VertexList).

%%getEdges: graph → vertexlist
% empty VL

getEdges({_DU,_VL,EL})->
  getEdgesHelper(EL,[]).

getEdgesHelper([],VertexList)->
  VertexList;
getEdgesHelper([{{VertexA,VertexB},_Attr}|Tail],VertexList)->
  getEdgesHelper(Tail,[VertexA,VertexB|VertexList]).

%%getVertices: graph → vertexlist
getVertices({_DU,VL,_EL})->
  getVerticesHelper(VL,[]).

getVerticesHelper([],VertexList)->
  VertexList;
getVerticesHelper([{Vertex,_Attr}|Tail],VertexList)->
  getVerticesHelper(Tail,[Vertex|VertexList]).

%%importG: filename × [d|ud] → graph
importG(Filename,DU) ->
  L = util:readlist(util:attachEnding(Filename, 'graph')),
  importGHelper(L,createG(DU))
.
% ok from util:writelist

% empty graph
importGHelper([],G)->
  G;
importGHelper([VertexA,VertexB,Weight|Tail],G)->
  importGHelper(Tail, setAtE(addEdge(G,VertexA,VertexB),VertexA,VertexB,weight,Weight))
.

% create Edge -> set Attribute -> recursive until List = []-> return graph
%%exportG: graph × filename → file
exportG({_DU,_VL,EL},Filename) ->
%%  create List then write it into file
  util:writelist(createList(EL,[]), util:attachEnding(Filename,graph)).

createList([],List) -> List;
% with attribute , write A, then B, then Value
createList([{{VertexA,VertexB},[_Name,Value]}|Tail],List)->
%%  createList(Tail,addToList(addToList(addToList(List,VertexA),VertexB),Value))
  createList(Tail,[VertexA,VertexB,Value|List])
  ;
% without attribute
createList([{{VertexA,VertexB},[]}|Tail],List)->
%%  createList(Tail,addToList(addToList(addToList(List,VertexA),VertexB),1))
  createList(Tail,[VertexA,VertexB,1|List])
  .

%%printG: graph × filename → dot
printG({DU,_VL,EL},Filename)->
    printHeader(DU, util:attachEnding(Filename,dot)),
    printGBody(DU,EL, util:attachEnding(Filename,dot)),
    printFoot(util:attachEnding(Filename,dot))
.

printHeader(DU, Filename)->
  util:logging(Filename, graphType(DU) ++ " tree" ++ "\n{\n" ).

printFoot(Filename)-> util:logging(Filename, "}\n" ).

graphType(ud) -> "graph";
graphType(d) -> "digraph".

delimeter(ud) -> "--";
delimeter(d) -> "->".

% end of dot file
printGBody(_DU,[],Filename)->
  util:logging(Filename,"");
% with attribute
printGBody(DU,[{{VertexA,VertexB},[_Name, Value]}|Tail],Filename)->
  printLine(DU,VertexA,VertexB,Value,Filename),
  printGBody(DU,Tail,Filename);
% without attribute
printGBody(DU,[{{VertexA,VertexB},[]}|Tail],Filename)->
  printLine(DU,VertexA,VertexB,1,Filename),
  printGBody(DU,Tail,Filename).

printLine(DU, VertexA,VertexB,Value,Filename)->
  util:logging(Filename,
              util:to_String(VertexA)
              ++ " "
              ++ delimeter(DU)
              ++ " "
              ++ util:to_String(VertexB)
              ++ " [label = "
              ++ util:to_String(Value)
              ++ "];\n"
              )
  .


%%%%%%%%%%%%% Helper method %%%%%%%%%%%%
%%%%%%%%%%%%% Helper method %%%%%%%%%%%%

% not allow duplicate
addToSet([], E) -> [E];
addToSet([H|T], H) -> [H|T];
addToSet([H|T], E) -> [H|addToSet(T,E)].


%time measure getEdges, setAtE, getTarge
%ReOp times of repeat operation
%FileNameStart = VerticesNum
testTimeAll(FileNameEnd,FileNameEnd,_Step,_ReOp) -> "ok/n"
;
testTimeAll(FileNameStart, FileNameEnd, Step,ReOp) ->
  util:logging('timelog.txt',"---------Test with graph "
  ++  util:to_String(FileNameStart)
    ++ " vertices -----------"
    " \n"),
  calcTime(getEdges,FileNameStart, ReOp),
  calcTime(setAtE,FileNameStart, ReOp),
  calcTime(getTarget,FileNameStart, ReOp),
  testTimeAll(FileNameStart + Step, FileNameEnd, Step, ReOp )
  .

calcTime(Operation, Filename, Wiederholtrate) ->

  calcTimeHelper(importG(Filename,d), Operation, Wiederholtrate ).

calcTimeHelper(G, setAtE, Wiederholrate)->
  loopSetAtE(G, Wiederholrate, 0.0);
calcTimeHelper(G, getEdges, Wiederholrate)->
  loopGetEdges(G, Wiederholrate, 0.0);
calcTimeHelper(G, getTarget, Wiederholrate)->
  loopGetTarget(G, Wiederholrate, 0.0).

%time measure getTarget
loopGetTarget(_G,0,Time)->
    util:logging('timelog.txt', util:to_String(Time) ++ " getTarget\n")
;
loopGetTarget(G,Wiederholrate,Time)->
  [Vertex] = util:randomliste(1,0,300),
  Start = erlang:timestamp(),
  getTarget(G, Vertex),
  Ende = erlang:timestamp(),
  Diffms = timer:now_diff(Ende,Start)/1000,
  loopGetTarget(G,Wiederholrate-1,Time +  Diffms)
.

%time measure getEdges
loopGetEdges(_G,0,Time)->
  util:logging('timelog.txt', util:to_String(Time) ++ " getEdges\n")
;
loopGetEdges({DU,VL,EL},Wiederholrate,Time)->
  Start = erlang:timestamp(),
  getEdges({DU,VL,EL}),
  Ende = erlang:timestamp(),
  Diffms = timer:now_diff(Ende,Start)/1000,
  loopGetEdges({DU,VL,EL},Wiederholrate-1,Time +  Diffms)
.

%time measure setAtE
loopSetAtE(_G,0,Time)->
util:logging('timelog.txt', util:to_String(Time) ++ " setAtE\n")
;
loopSetAtE({DU,VL,EL},Wiederholrate,Time)->
[VertexA,VertexB,Name,Value] = util:randomliste(4,0,300),
Start = erlang:timestamp(),
setAtE({DU,VL,EL}, VertexA,VertexB,Name,Value),
Ende = erlang:timestamp(),
Diffms = timer:now_diff(Ende,Start)/1000,
  loopSetAtE({DU,VL,EL},Wiederholrate-1,Time +  Diffms)
.


gentestgraph(_StartEdgesNum, _Step, 0) -> "ok/n"
;
gentestgraph(VertexNum, Step, GraphNum) ->
  gengraph:gengraph(VertexNum,1,7, util:attachEnding(VertexNum,'graph')),
  gentestgraph(VertexNum+Step,Step, GraphNum-1)
.