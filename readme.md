## Dijkstra

Dijkstra’s algorithm of finding optimal paths in Erlang. See how this algorithm works on this 
[page](https://www.cs.usfca.edu/~galles/visualization/Dijkstra.html)

### Prerequisites

To run the code you need Erlang/OTP installed.
Check out this for details 
[link](http://erlang.org/doc/installation_guide/INSTALL.html)
 
## Built and Run


clone project

`clone https://github.com/phubinhdang/dijkstra.git`

change to src directory

`cd src`

open erlang shell

`erl`

compile related modules

`c(util).`

`c(adtgraph).`

`c(dijkstra).`

function call

dijkstra:dijkstra(Filename,StartVertex,DirectedorUndirected).

**Example call**

`dijkstra:dijkstra(test_0ud,0,ud).`

**Parameters**

+ Filename : without extension name, your graph file muss locate in *src* directory and its name muss end with *.graph*

+ StartVertex : start vertex, from that you want to find the optimal path to every vertexs in graph
+  *ud* (undirected graph) or *d* (directed graph)

**.graph file**

.graph files should contain only numbers, separated by " , " (comma) 

e.g: 0,4,3,2,4,5... is interpreted as triples (a triple represents an  edge in graph)
 {___vertex,vertex,weight___}

0,4,3 <=>  0 -> 3 ; weight = 3
 
2,4,5 <=> 2 -> 3 ; weight = 5


**Result**

[{11,0,11}, {33,5,11}, {Vertex,Distance,Path},…] with Startvertex = 11;

+ Distance : distance from Startvertex to vertex, distance from Startvertex to itself is 0
+ Path(previous vertex ): second last vertex of the path from Startvertex to Vertex, path of Startvertex to itself is Startvertex 



## Authors

* **Phu Binh Dang** - *Initial work* - [phubinhdang](https://github.com/phubinhdang)


