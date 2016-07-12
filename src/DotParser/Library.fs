// Eugene Auduchinok, 2016

module DotParser

open Yard.Generators.RNGLR.Parser
open Yard.Generators.Common.AST
open Parser
open Lexer
open GraphData
open System.Collections.Generic
open Microsoft.FSharp.Text

let parse str =
    let translateArgs = {
            tokenToRange = fun _ -> Unchecked.defaultof<_>, Unchecked.defaultof<_>
            zeroPosition = Unchecked.defaultof<_>
            clearAST = false
            filterEpsilons = false
        }

    let lexbuf = Lexing.LexBuffer<_>.FromString str
    let tokens = seq { while not lexbuf.IsPastEndOfStream do yield tokenize lexbuf }

    let parsedGraphDataList : GraphData list =
        match buildAst tokens with
        | Error (pos, token, msg, _, _) -> failwithf "DotParser: error on position %d, token %A: %s" pos token msg
        | Success (ast, _, errors) -> translate translateArgs ast errors

    match parsedGraphDataList with
    | data :: _ -> data
    | [] -> failwith "DotParser: could not translate ast"
