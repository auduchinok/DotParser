// Eugene Auduchinok, 2016

namespace DotParser

type Attributes = Map<string, string>

type GraphData =
    { IsDirected: bool
      IsStrict: bool
      Subgraphs: GraphData list
      Nodes: Map<string, Attributes>
      Edges: Map<string * string, Attributes list>
      GraphAttributes: Attributes
      NodeAttributes: Attributes
      EdgeAttributes: Attributes }


[<AutoOpen>]
module GraphData =
    [<CompiledName("EmptyGraph")>]
    let emptyGraph d s =
        { IsDirected = d
          IsStrict = s
          Subgraphs = []
          Nodes = Map.empty
          Edges = Map.empty
          GraphAttributes = Map.empty
          NodeAttributes = Map.empty
          EdgeAttributes = Map.empty }

    let copyAttrs (g: GraphData) =
        { g with Nodes = Map.empty; Edges = Map.empty ; Subgraphs = []}


    let merge = Map.fold (fun acc key value -> Map.add key value acc)

    let addAttributes (g: GraphData) (key: string) (a: Attributes) =
        match key with
        | "graph" -> { g with GraphAttributes = merge g.GraphAttributes a }
        | "node"  -> { g with NodeAttributes  = merge g.NodeAttributes a }
        | "edge"  -> { g with EdgeAttributes  = merge g.EdgeAttributes a }
        | _ -> failwithf "DotParser: parser error: wrong attribute key: %s" key


    let addNode (g: GraphData) (n: string) (a: Attributes) =
        let a =
            match g.Nodes.TryFind n with
            | Some attributes -> merge attributes a
            | None -> a
        { g with Nodes = Map.add n (merge g.NodeAttributes a) g.Nodes }, [n]


    let addEdge (g: GraphData) (n1: string) (n2: string) (a: Attributes) =
        let edge = if (not <| g.IsDirected) && n2 < n1 then n2, n1 else n1, n2
        let newEdges =
            match g.Edges.TryFind edge, g.IsStrict with
            | Some oldEdges, true  -> [merge (merge (List.head oldEdges) g.EdgeAttributes) a]
            | Some oldEdges, false -> (merge g.EdgeAttributes a):: oldEdges
            | _ -> [merge g.EdgeAttributes a]

        { g with Edges = Map.add edge newEdges g.Edges }


    let addEdges (g: GraphData) (ns1: string list) (ns2: string list) (a: Attributes) =
        (List.fold (fun acc n1 -> List.fold (fun acc n2 -> addEdge acc n2 n1 a) acc ns2) g ns1), ns2

    let addEdgesForList (g: GraphData) (ns: string list list) (a: Attributes) =
        List.fold (fun (g, n1) n2 -> addEdges g n1 n2 a) (g, ns.Head) ns.Tail |> fst

    let addSubgraph (g: GraphData) (s: GraphData) =
        let addSubgraph g = {g with Subgraphs = s :: g.Subgraphs}
        let addNodes g = Map.fold (fun acc n attr -> fst <| addNode acc n attr) g s.Nodes
        let addParallelEdges n1 n2 = List.fold (fun acc x -> addEdge acc n1 n2 x)
        let addEdges g = Map.fold (fun acc (n1, n2) attr -> addParallelEdges n1 n2 acc attr) g s.Edges
        
        g |> addSubgraph |> addNodes |> addEdges, s.Nodes |> Map.toList |> List.map fst