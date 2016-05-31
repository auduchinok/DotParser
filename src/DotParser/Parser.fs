
# 2 "Parser.fs"
module Parser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST

# 3 "Grammar.yrd"

open GraphData
open Option
open System.Collections.Generic
open System.Linq

let map = Map.ofList
let wrongEdge = failwithf "DotParser: unexpected token: %s"

let inline getOrElse v =
    function
    | Some x -> x
    | None -> v

# 24 "Parser.fs"
type Token =
    | ASSIGN of (string)
    | COLON of (string)
    | COMMA of (string)
    | DIEDGE of (string)
    | DIGRAPH of (string)
    | EDGE of (string)
    | GRAPH of (string)
    | ID of (string)
    | LBRACE of (string)
    | LBRACK of (string)
    | NODE of (string)
    | RBRACE of (string)
    | RBRACK of (string)
    | RNGLR_EOF of (string)
    | SEMI of (string)
    | STRICT of (string)
    | SUBGRAPH of (string)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | ASSIGN x -> box x
    | COLON x -> box x
    | COMMA x -> box x
    | DIEDGE x -> box x
    | DIGRAPH x -> box x
    | EDGE x -> box x
    | GRAPH x -> box x
    | ID x -> box x
    | LBRACE x -> box x
    | LBRACK x -> box x
    | NODE x -> box x
    | RBRACE x -> box x
    | RBRACK x -> box x
    | RNGLR_EOF x -> box x
    | SEMI x -> box x
    | STRICT x -> box x
    | SUBGRAPH x -> box x

let numToString = function
    | 0 -> "a_list"
    | 1 -> "attr_list"
    | 2 -> "attr_stmt"
    | 3 -> "edge_rhs"
    | 4 -> "edge_stmt"
    | 5 -> "edgeop"
    | 6 -> "error"
    | 7 -> "graph"
    | 8 -> "graph_type"
    | 9 -> "node_id"
    | 10 -> "node_stmt"
    | 11 -> "nodes"
    | 12 -> "port"
    | 13 -> "stmt"
    | 14 -> "stmt_list"
    | 15 -> "subgraph"
    | 16 -> "yard_exp_brackets_1"
    | 17 -> "yard_exp_brackets_2"
    | 18 -> "yard_exp_brackets_3"
    | 19 -> "yard_exp_brackets_4"
    | 20 -> "yard_exp_brackets_5"
    | 21 -> "yard_exp_brackets_6"
    | 22 -> "yard_exp_brackets_7"
    | 23 -> "yard_exp_brackets_8"
    | 24 -> "yard_opt_1"
    | 25 -> "yard_opt_10"
    | 26 -> "yard_opt_11"
    | 27 -> "yard_opt_12"
    | 28 -> "yard_opt_13"
    | 29 -> "yard_opt_14"
    | 30 -> "yard_opt_15"
    | 31 -> "yard_opt_16"
    | 32 -> "yard_opt_2"
    | 33 -> "yard_opt_3"
    | 34 -> "yard_opt_4"
    | 35 -> "yard_opt_5"
    | 36 -> "yard_opt_6"
    | 37 -> "yard_opt_7"
    | 38 -> "yard_opt_8"
    | 39 -> "yard_opt_9"
    | 40 -> "yard_start_rule"
    | 41 -> "ASSIGN"
    | 42 -> "COLON"
    | 43 -> "COMMA"
    | 44 -> "DIEDGE"
    | 45 -> "DIGRAPH"
    | 46 -> "EDGE"
    | 47 -> "GRAPH"
    | 48 -> "ID"
    | 49 -> "LBRACE"
    | 50 -> "LBRACK"
    | 51 -> "NODE"
    | 52 -> "RBRACE"
    | 53 -> "RBRACK"
    | 54 -> "RNGLR_EOF"
    | 55 -> "SEMI"
    | 56 -> "STRICT"
    | 57 -> "SUBGRAPH"
    | _ -> ""

let tokenToNumber = function
    | ASSIGN _ -> 41
    | COLON _ -> 42
    | COMMA _ -> 43
    | DIEDGE _ -> 44
    | DIGRAPH _ -> 45
    | EDGE _ -> 46
    | GRAPH _ -> 47
    | ID _ -> 48
    | LBRACE _ -> 49
    | LBRACK _ -> 50
    | NODE _ -> 51
    | RBRACE _ -> 52
    | RBRACK _ -> 53
    | RNGLR_EOF _ -> 54
    | SEMI _ -> 55
    | STRICT _ -> 56
    | SUBGRAPH _ -> 57

let isLiteral = function
    | ASSIGN _ -> false
    | COLON _ -> false
    | COMMA _ -> false
    | DIEDGE _ -> false
    | DIGRAPH _ -> false
    | EDGE _ -> false
    | GRAPH _ -> false
    | ID _ -> false
    | LBRACE _ -> false
    | LBRACK _ -> false
    | NODE _ -> false
    | RBRACE _ -> false
    | RBRACK _ -> false
    | RNGLR_EOF _ -> false
    | SEMI _ -> false
    | STRICT _ -> false
    | SUBGRAPH _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|23; 22; 21; 20; 19; 19; 18; 18; 18; 17; 16; 16; 31; 31; 12; 29; 29; 30; 30; 15; 28; 28; 9; 5; 5; 11; 11; 27; 27; 3; 26; 26; 4; 25; 25; 39; 39; 10; 38; 38; 37; 37; 0; 36; 36; 1; 2; 13; 13; 13; 13; 13; 33; 33; 35; 35; 34; 34; 14; 32; 32; 8; 24; 24; 7; 40|]
let private rules = [|42; 48; 57; 30; 12; 12; 55; 43; 47; 51; 46; 13; 34; 35; 47; 45; 23; 42; 48; 31; 22; 48; 29; 49; 14; 52; 21; 48; 28; 46; 44; 9; 15; 3; 5; 11; 27; 1; 11; 3; 26; 1; 20; 48; 39; 25; 0; 19; 48; 41; 48; 37; 38; 0; 50; 36; 53; 18; 1; 10; 4; 2; 15; 48; 41; 48; 17; 14; 55; 33; 56; 32; 16; 48; 8; 24; 49; 14; 52; 7|]
let private rulesStart = [|0; 2; 4; 5; 6; 7; 8; 9; 10; 11; 14; 15; 16; 16; 17; 20; 20; 21; 21; 22; 26; 26; 27; 29; 30; 31; 32; 33; 33; 34; 37; 37; 38; 41; 41; 42; 42; 43; 46; 46; 47; 47; 48; 53; 53; 54; 57; 59; 60; 61; 62; 63; 66; 66; 67; 67; 68; 68; 69; 70; 70; 71; 73; 73; 74; 79; 80|]
let startRule = 65

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 75; 79; 3; 74; 4; 5; 6; 7; 8; 9; 36; 72; 39; 40; 41; 33; 34; 43; 45; 46; 47; 63; 64; 10; 26; 30; 31; 11; 12; 13; 14; 15; 17; 16; 18; 19; 20; 21; 24; 25; 22; 23; 27; 32; 70; 28; 29; 35; 68; 37; 67; 38; 44; 42; 48; 49; 50; 51; 52; 55; 57; 53; 54; 56; 58; 59; 60; 61; 62; 65; 66; 69; 71; 73; 76; 77; 78|]
let private small_gotos =
        [|4; 458752; 524289; 2097154; 3670019; 131074; 1572868; 3145733; 196609; 3211270; 262162; 131079; 262152; 589833; 655370; 720907; 851980; 917517; 983054; 1114127; 1179664; 1441809; 1900562; 2162707; 3014676; 3080213; 3145750; 3342359; 3735576; 589828; 196633; 327706; 2883611; 3014684; 655363; 65565; 1703966; 3276831; 851971; 32; 2359329; 3145762; 983041; 3473443; 1114113; 2687012; 1179649; 3145765; 1245188; 1245222; 2424871; 2818088; 3604521; 1376259; 42; 2490411; 3145762; 1703943; 589833; 720940; 983085; 1441809; 1900562; 3145774; 3735576; 1769477; 196655; 327706; 1769520; 2883611; 3014684; 2228225; 3211313; 2293778; 131079; 262152; 589833; 655370; 720907; 851980; 917554; 983054; 1114127; 1179664; 1441809; 1900562; 2162707; 3014676; 3080213; 3145750; 3342359; 3735576; 2359298; 2228275; 3604532; 2424851; 131079; 262152; 589833; 655370; 720907; 851980; 917557; 983054; 1114127; 1179664; 1441809; 1900562; 2162707; 2293814; 3014676; 3080213; 3145750; 3342359; 3735576; 2686978; 65591; 3276831; 3080199; 786488; 1310777; 1376314; 1835067; 2555964; 2687037; 2752574; 3407875; 65599; 1638464; 3276831; 3604481; 3145793; 3735553; 3145794; 3801091; 1507395; 2031684; 2752581; 3997697; 3145798; 4194306; 1966151; 3145800; 4456449; 3407945; 4587524; 786506; 1376314; 1835067; 2752574; 4718593; 3407947; 4915203; 1048652; 2949197; 3080270|]
let gotos = Array.zeroCreate 80
for i = 0 to 79 do
        gotos.[i] <- Array.zeroCreate 58
cur <- 0
while cur < small_gotos.Length do
    let i = small_gotos.[cur] >>> 16
    let length = small_gotos.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_gotos.[cur + k] >>> 16
        let x = small_gotos.[cur + k] &&& 65535
        gotos.[i].[j] <- lists_gotos.[x]
    cur <- cur + length
let private lists_reduces = [|[|49,1|]; [|48,1|]; [|25,1|]; [|47,1|]; [|32,2|]; [|31,1|]; [|32,3|]; [|44,1|]; [|45,3|]; [|42,3|]; [|41,1|]; [|42,4|]; [|39,1|]; [|42,5|]; [|5,1|]; [|4,1|]; [|29,2|]; [|28,1|]; [|29,3|]; [|24,1|]; [|23,1|]; [|26,1|]; [|16,1|]; [|9,1|]; [|9,2|]; [|55,1|]; [|50,1; 26,1|]; [|50,1|]; [|53,1|]; [|46,2|]; [|58,1|]; [|9,3|]; [|8,1|]; [|6,1|]; [|22,1|]; [|37,1; 22,1|]; [|37,1|]; [|2,1|]; [|3,1; 2,1|]; [|3,1|]; [|36,1|]; [|21,1|]; [|22,2|]; [|37,2|]; [|34,1|]; [|37,3|]; [|51,3|]; [|14,2|]; [|13,1|]; [|14,3|]; [|0,2|]; [|7,1|]; [|1,1|]; [|1,2|]; [|18,1|]; [|57,1|]; [|19,4|]; [|64,5|]; [|63,1|]; [|61,2|]; [|11,1|]; [|10,1|]; [|60,1|]|]
let private small_reduces =
        [|327688; 3014656; 3080192; 3145728; 3211264; 3342336; 3407872; 3604480; 3735552; 393224; 3014657; 3080193; 3145729; 3211265; 3342337; 3407873; 3604481; 3735553; 458762; 2883586; 3014658; 3080194; 3145730; 3211266; 3276802; 3342338; 3407874; 3604482; 3735554; 524296; 3014659; 3080195; 3145731; 3211267; 3342339; 3407875; 3604483; 3735555; 655368; 3014660; 3080196; 3145732; 3211268; 3342340; 3407876; 3604484; 3735556; 720904; 3014661; 3080197; 3145733; 3211269; 3342341; 3407877; 3604485; 3735557; 786440; 3014662; 3080198; 3145734; 3211270; 3342342; 3407878; 3604486; 3735558; 917505; 3473415; 1048584; 3014664; 3080200; 3145736; 3211272; 3342344; 3407880; 3604488; 3735560; 1245185; 3473417; 1310722; 3145738; 3473418; 1376257; 3473419; 1441793; 3473420; 1507329; 3473421; 1572866; 3145742; 3473422; 1638402; 3145743; 3473423; 1769481; 3014672; 3080208; 3145744; 3211280; 3276816; 3342352; 3407888; 3604496; 3735568; 1835017; 3014673; 3080209; 3145745; 3211281; 3276817; 3342353; 3407889; 3604497; 3735569; 1900553; 3014674; 3080210; 3145746; 3211282; 3276818; 3342354; 3407890; 3604498; 3735570; 1966083; 3145747; 3211283; 3735571; 2031619; 3145748; 3211284; 3735572; 2097162; 2883605; 3014677; 3080213; 3145749; 3211285; 3276821; 3342357; 3407893; 3604501; 3735573; 2162689; 3211286; 2359297; 3407895; 2424833; 3407896; 2490369; 3407897; 2555913; 2883605; 3014682; 3080219; 3145755; 3211291; 3342363; 3407899; 3604507; 3735579; 2621441; 3407900; 2752520; 3014685; 3080221; 3145757; 3211293; 3342365; 3407901; 3604509; 3735581; 2818049; 3407902; 2883585; 3407903; 2949121; 3276832; 3014657; 3276833; 3080201; 2883618; 3014691; 3080228; 3145764; 3211300; 3342372; 3407908; 3604516; 3735588; 3145738; 2883621; 3014694; 3080231; 3145767; 3211303; 3276839; 3342375; 3407911; 3604519; 3735591; 3211273; 3014696; 3080232; 3145768; 3211304; 3276840; 3342376; 3407912; 3604520; 3735592; 3276810; 2883625; 3014697; 3080233; 3145769; 3211305; 3276841; 3342377; 3407913; 3604521; 3735593; 3342346; 2883626; 3014698; 3080234; 3145770; 3211306; 3276842; 3342378; 3407914; 3604522; 3735594; 3407880; 3014699; 3080235; 3145771; 3211307; 3342379; 3407915; 3604523; 3735595; 3473416; 3014700; 3080236; 3145772; 3211308; 3342380; 3407916; 3604524; 3735596; 3538952; 3014701; 3080237; 3145773; 3211309; 3342381; 3407917; 3604525; 3735597; 3670024; 3014702; 3080238; 3145774; 3211310; 3342382; 3407918; 3604526; 3735598; 3801098; 2883631; 3014703; 3080239; 3145775; 3211311; 3276847; 3342383; 3407919; 3604527; 3735599; 3866634; 2883632; 3014704; 3080240; 3145776; 3211312; 3276848; 3342384; 3407920; 3604528; 3735600; 3932170; 2883633; 3014705; 3080241; 3145777; 3211313; 3276849; 3342385; 3407921; 3604529; 3735601; 4063242; 2883634; 3014706; 3080242; 3145778; 3211314; 3276850; 3342386; 3407922; 3604530; 3735602; 4128769; 3276851; 4194305; 3211316; 4259841; 3211317; 4325377; 3211318; 4390919; 3014711; 3080247; 3145783; 3211319; 3342391; 3407927; 3735607; 4521994; 2883640; 3014712; 3080248; 3145784; 3211320; 3276856; 3342392; 3407928; 3604536; 3735608; 4587530; 2883618; 3014690; 3080226; 3145762; 3211298; 3276834; 3342370; 3407906; 3604514; 3735586; 4653066; 2883621; 3014693; 3080229; 3145765; 3211301; 3276837; 3342373; 3407909; 3604517; 3735589; 4784129; 3539001; 4849665; 3211322; 4980738; 3145787; 3211323; 5046274; 3145788; 3211324; 5111810; 3145789; 3211325; 5177346; 2949182; 3080254|]
let reduces = Array.zeroCreate 80
for i = 0 to 79 do
        reduces.[i] <- Array.zeroCreate 58
cur <- 0
while cur < small_reduces.Length do
    let i = small_reduces.[cur] >>> 16
    let length = small_reduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_reduces.[cur + k] >>> 16
        let x = small_reduces.[cur + k] &&& 65535
        reduces.[i].[j] <- lists_reduces.[x]
    cur <- cur + length
let private lists_zeroReduces = [|[|59|]; [|62|]; [|15|]; [|58; 52|]; [|30|]; [|43|]; [|40|]; [|38|]; [|27|]; [|56|]; [|58; 55; 54; 52|]; [|20|]; [|35; 20|]; [|35|]; [|33|]; [|12|]; [|17|]|]
let private small_zeroReduces =
        [|2; 2949120; 3080192; 131073; 3211265; 262146; 3211266; 3407875; 655368; 3014660; 3080196; 3145732; 3211268; 3342340; 3407876; 3604484; 3735556; 851969; 3473413; 1245186; 3145734; 3473414; 1376257; 3473415; 1703937; 3211266; 1769481; 3014664; 3080200; 3145736; 3211272; 3276808; 3342344; 3407880; 3604488; 3735560; 2293762; 3211266; 3407875; 2359303; 3014665; 3080201; 3145737; 3211273; 3342345; 3407881; 3735561; 2424834; 3211266; 3407882; 3080202; 2883595; 3014668; 3080205; 3145741; 3211277; 3276813; 3342349; 3407885; 3604493; 3735565; 3407880; 3014670; 3080206; 3145742; 3211278; 3342350; 3407886; 3604494; 3735566; 3801098; 2883599; 3014671; 3080207; 3145743; 3211279; 3276815; 3342351; 3407887; 3604495; 3735567; 4194305; 3211280; 4587530; 2883595; 3014667; 3080203; 3145739; 3211275; 3276811; 3342347; 3407883; 3604491; 3735563|]
let zeroReduces = Array.zeroCreate 80
for i = 0 to 79 do
        zeroReduces.[i] <- Array.zeroCreate 58
cur <- 0
while cur < small_zeroReduces.Length do
    let i = small_zeroReduces.[cur] >>> 16
    let length = small_zeroReduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_zeroReduces.[cur + k] >>> 16
        let x = small_zeroReduces.[cur + k] &&& 65535
        zeroReduces.[i].[j] <- lists_zeroReduces.[x]
    cur <- cur + length
let private small_acc = [1]
let private accStates = Array.zeroCreate 80
for i = 0 to 79 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 54
let errorIndex = 6
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(66, new Nodes([||])), null)), null); null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(58, new Nodes([|box (new AST(new Family(52, new Nodes([||])), null))|])), null)), null); null; null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(62, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(27, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(20, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(59, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(52, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(56, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(54, new Nodes([||])), [|new Family(55, new Nodes([|box (new AST(new Family(58, new Nodes([|box (new AST(new Family(52, new Nodes([||])), null))|])), null))|]))|])), null); new Tree<_>(null,box (new AST(new Family(43, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(40, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(38, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(35, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(66, new Nodes([||])), null)), null); null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(58, new Nodes([|box (new AST(new Family(52, new Nodes([||])), null))|])), null)), null); null; null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(62, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(27, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(20, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(59, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(52, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(56, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(54, new Nodes([||])), [|new Family(55, new Nodes([|box (new AST(new Family(58, new Nodes([|box (new AST(new Family(52, new Nodes([||])), null))|])), null))|]))|])), null); new Tree<_>(null,box (new AST(new Family(43, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(40, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(38, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(35, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_a_list * '_rnglr_type_attr_list * '_rnglr_type_attr_stmt * '_rnglr_type_edge_rhs * '_rnglr_type_edge_stmt * '_rnglr_type_edgeop * '_rnglr_type_error * '_rnglr_type_graph * '_rnglr_type_graph_type * '_rnglr_type_node_id * '_rnglr_type_node_stmt * '_rnglr_type_nodes * '_rnglr_type_port * '_rnglr_type_stmt * '_rnglr_type_stmt_list * '_rnglr_type_subgraph * '_rnglr_type_yard_exp_brackets_1 * '_rnglr_type_yard_exp_brackets_2 * '_rnglr_type_yard_exp_brackets_3 * '_rnglr_type_yard_exp_brackets_4 * '_rnglr_type_yard_exp_brackets_5 * '_rnglr_type_yard_exp_brackets_6 * '_rnglr_type_yard_exp_brackets_7 * '_rnglr_type_yard_exp_brackets_8 * '_rnglr_type_yard_opt_1 * '_rnglr_type_yard_opt_10 * '_rnglr_type_yard_opt_11 * '_rnglr_type_yard_opt_12 * '_rnglr_type_yard_opt_13 * '_rnglr_type_yard_opt_14 * '_rnglr_type_yard_opt_15 * '_rnglr_type_yard_opt_16 * '_rnglr_type_yard_opt_2 * '_rnglr_type_yard_opt_3 * '_rnglr_type_yard_opt_4 * '_rnglr_type_yard_opt_5 * '_rnglr_type_yard_opt_6 * '_rnglr_type_yard_opt_7 * '_rnglr_type_yard_opt_8 * '_rnglr_type_yard_opt_9 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with COLON _rnglr_val -> [_rnglr_val] | a -> failwith "COLON expected, but %A found" a )
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 74 "Grammar.yrd"
                                             
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_8) 
# 262 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SUBGRAPH _rnglr_val -> [_rnglr_val] | a -> failwith "SUBGRAPH expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_15) (g: GraphData)
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 72 "Grammar.yrd"
                                       
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_7) 
# 284 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun name ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_port) 
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 55 "Grammar.yrd"
                                                          
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_6) 
# 304 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun n ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_port) 
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 55 "Grammar.yrd"
                                                          
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_5) 
# 324 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun k -> fun v ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SEMI _rnglr_val -> [_rnglr_val] | a -> failwith "SEMI expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 53 "Grammar.yrd"
                                             
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_4) 
# 344 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun k -> fun v ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with COMMA _rnglr_val -> [_rnglr_val] | a -> failwith "COMMA expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 53 "Grammar.yrd"
                                                         
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_4) 
# 364 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with GRAPH _rnglr_val -> [_rnglr_val] | a -> failwith "GRAPH expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 48 "Grammar.yrd"
                               "graph" 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_3) 
# 384 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with NODE _rnglr_val -> [_rnglr_val] | a -> failwith "NODE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 48 "Grammar.yrd"
                                                  "node" 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_3) 
# 404 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with EDGE _rnglr_val -> [_rnglr_val] | a -> failwith "EDGE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 48 "Grammar.yrd"
                                                                    "edge" 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_3) 
# 424 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt) g
             |> List.iter (fun (s) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_4) (g: GraphData) s
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_opt_5) (g: GraphData) s
                 |> List.iter (fun (s1) -> 
                  _rnglr_cycle_res := (
                    
# 37 "Grammar.yrd"
                                                                     getOrElse s s1 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_2) 
# 448 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun s ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with GRAPH _rnglr_val -> [_rnglr_val] | a -> failwith "GRAPH expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 34 "Grammar.yrd"
                                                  false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_1) 
# 468 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun s ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DIGRAPH _rnglr_val -> [_rnglr_val] | a -> failwith "DIGRAPH expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 34 "Grammar.yrd"
                                                                      true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_1) 
# 488 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 74 "Grammar.yrd"
                    None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 74 "Grammar.yrd"
               : '_rnglr_type_yard_opt_16) 
# 506 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_8) 
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 74 "Grammar.yrd"
                      Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 74 "Grammar.yrd"
               : '_rnglr_type_yard_opt_16) 
# 526 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with COLON _rnglr_val -> [_rnglr_val] | a -> failwith "COLON expected, but %A found" a )
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_opt_16) 
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 74 "Grammar.yrd"
                                                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 74 "Grammar.yrd"
               : '_rnglr_type_port) 
# 550 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 72 "Grammar.yrd"
                   None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 72 "Grammar.yrd"
               : '_rnglr_type_yard_opt_14) 
# 568 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_7) (g: GraphData)
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 72 "Grammar.yrd"
                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 72 "Grammar.yrd"
               : '_rnglr_type_yard_opt_14) 
# 588 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 32 "Grammar.yrd"
                                   None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 32 "Grammar.yrd"
               : '_rnglr_type_yard_opt_15) 
# 606 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 32 "Grammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 32 "Grammar.yrd"
               : '_rnglr_type_yard_opt_15) 
# 626 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_14) (g: GraphData)
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with LBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "LBRACE expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_stmt_list) (copyAttrs g)
                 |> List.iter (fun (s) -> 
                  (match ((unbox _rnglr_children.[3]) : Token) with RBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "RBRACE expected, but %A found" a )
                   |> List.iter (fun (_) -> 
                    _rnglr_cycle_res := (
                      
# 72 "Grammar.yrd"
                                                                                           addSubgraph g s 
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 71 "Grammar.yrd"
               : '_rnglr_type_subgraph) 
# 652 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun name ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 55 "Grammar.yrd"
                                                 None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 55 "Grammar.yrd"
               : '_rnglr_type_yard_opt_13) 
# 670 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun name ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_6) (g: GraphData) name
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 55 "Grammar.yrd"
                                                   Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 55 "Grammar.yrd"
               : '_rnglr_type_yard_opt_13) 
# 690 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
             |> List.iter (fun (name) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_13) (g: GraphData) name
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 69 "Grammar.yrd"
                                                                  addNode g name Map.empty 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 69 "Grammar.yrd"
               : '_rnglr_type_node_id) 
# 712 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with EDGE _rnglr_val -> [_rnglr_val] | a -> failwith "EDGE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 66 "Grammar.yrd"
                        if g.IsDirected then wrongEdge "--" else () 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 65 "Grammar.yrd"
               : '_rnglr_type_edgeop) 
# 732 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DIEDGE _rnglr_val -> [_rnglr_val] | a -> failwith "DIEDGE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 67 "Grammar.yrd"
                             if not <| g.IsDirected then wrongEdge "->" else () 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 65 "Grammar.yrd"
               : '_rnglr_type_edgeop) 
# 752 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_node_id) g
             |> List.iter (fun (n) -> 
              _rnglr_cycle_res := (
                
# 63 "Grammar.yrd"
                                                          n 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 63 "Grammar.yrd"
               : '_rnglr_type_nodes) 
# 772 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_subgraph) g
             |> List.iter (fun (n) -> 
              _rnglr_cycle_res := (
                
# 63 "Grammar.yrd"
                                                                                  n 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 63 "Grammar.yrd"
               : '_rnglr_type_nodes) 
# 792 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: GraphData * string list list) -> fun n ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 61 "Grammar.yrd"
                                                          None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 61 "Grammar.yrd"
               : '_rnglr_type_yard_opt_12) 
# 810 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: GraphData * string list list) -> fun n ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_edge_rhs) (fst n, (snd n) :: (snd d))
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 61 "Grammar.yrd"
                                                            Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 61 "Grammar.yrd"
               : '_rnglr_type_yard_opt_12) 
# 830 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: GraphData * string list list) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_edgeop) (fst d)
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_nodes) (fst d)
               |> List.iter (fun (n) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_opt_12) (d: GraphData * string list list) n
                 |> List.iter (fun (r) -> 
                  _rnglr_cycle_res := (
                    
# 61 "Grammar.yrd"
                                                                                                           if isSome r then r.Value else (fst n), ((snd n) :: (snd d)) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 60 "Grammar.yrd"
               : '_rnglr_type_edge_rhs) 
# 854 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun n -> fun g1 ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 48 "Grammar.yrd"
                                                                              None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 48 "Grammar.yrd"
               : '_rnglr_type_yard_opt_11) 
# 872 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun n -> fun g1 ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_attr_list) 
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 48 "Grammar.yrd"
                                                                                Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 48 "Grammar.yrd"
               : '_rnglr_type_yard_opt_11) 
# 892 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_nodes) g
             |> List.iter (fun (n) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_edge_rhs) (fst n, [snd n])
               |> List.iter (fun (g1) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_opt_11) (g: GraphData) n g1
                 |> List.iter (fun (a) -> 
                  _rnglr_cycle_res := (
                    
# 58 "Grammar.yrd"
                                                                                  addEdgesForList (fst g1) (snd g1) (getOrElse Map.empty a) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 57 "Grammar.yrd"
               : '_rnglr_type_edge_stmt) 
# 916 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun n ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 48 "Grammar.yrd"
                                                                              None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 48 "Grammar.yrd"
               : '_rnglr_type_yard_opt_10) 
# 934 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun n ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_attr_list) 
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 48 "Grammar.yrd"
                                                                                Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 48 "Grammar.yrd"
               : '_rnglr_type_yard_opt_10) 
# 954 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun n ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 55 "Grammar.yrd"
                                                 None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 55 "Grammar.yrd"
               : '_rnglr_type_yard_opt_9) 
# 972 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun n ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_5) (g: GraphData) n
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 55 "Grammar.yrd"
                                                   Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 55 "Grammar.yrd"
               : '_rnglr_type_yard_opt_9) 
# 992 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
             |> List.iter (fun (n) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_9) (g: GraphData) n
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_opt_10) (g: GraphData) n
                 |> List.iter (fun (a) -> 
                  _rnglr_cycle_res := (
                    
# 55 "Grammar.yrd"
                                                                                 addNode g n (getOrElse Map.empty a) |> fst 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 55 "Grammar.yrd"
               : '_rnglr_type_node_stmt) 
# 1016 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun k -> fun v ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 50 "Grammar.yrd"
                                   None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 50 "Grammar.yrd"
               : '_rnglr_type_yard_opt_8) 
# 1034 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun k -> fun v ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_a_list) 
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 50 "Grammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 50 "Grammar.yrd"
               : '_rnglr_type_yard_opt_8) 
# 1054 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun k -> fun v ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 37 "Grammar.yrd"
                                   None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 37 "Grammar.yrd"
               : '_rnglr_type_yard_opt_7) 
# 1072 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun k -> fun v ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_4) k v
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 37 "Grammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 37 "Grammar.yrd"
               : '_rnglr_type_yard_opt_7) 
# 1092 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
             |> List.iter (fun (k) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with ASSIGN _rnglr_val -> [_rnglr_val] | a -> failwith "ASSIGN expected, but %A found" a )
               |> List.iter (fun (_) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
                 |> List.iter (fun (v) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_yard_opt_7) k v
                   |> List.iter (fun (_) -> 
                    ((unbox _rnglr_children.[4]) : '_rnglr_type_yard_opt_8) k v
                     |> List.iter (fun (l) -> 
                      _rnglr_cycle_res := (
                        
# 53 "Grammar.yrd"
                                                                                 Map.add k v (getOrElse Map.empty l) 
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 52 "Grammar.yrd"
               : '_rnglr_type_a_list) 
# 1120 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 50 "Grammar.yrd"
                                   None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 50 "Grammar.yrd"
               : '_rnglr_type_yard_opt_6) 
# 1138 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_a_list) 
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 50 "Grammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 50 "Grammar.yrd"
               : '_rnglr_type_yard_opt_6) 
# 1158 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LBRACK _rnglr_val -> [_rnglr_val] | a -> failwith "LBRACK expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_6) 
               |> List.iter (fun (a) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with RBRACK _rnglr_val -> [_rnglr_val] | a -> failwith "RBRACK expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 50 "Grammar.yrd"
                                                          getOrElse Map.empty a 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 50 "Grammar.yrd"
               : '_rnglr_type_attr_list) 
# 1182 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_3) (g: GraphData)
             |> List.iter (fun (k) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_attr_list) 
               |> List.iter (fun (a) -> 
                _rnglr_cycle_res := (
                  
# 48 "Grammar.yrd"
                                                                                              addAttributes g k a 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 47 "Grammar.yrd"
               : '_rnglr_type_attr_stmt) 
# 1204 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_node_stmt) g
             |> List.iter (fun (g1) -> 
              _rnglr_cycle_res := (
                
# 40 "Grammar.yrd"
                                        g1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 39 "Grammar.yrd"
               : '_rnglr_type_stmt) 
# 1224 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_edge_stmt) g
             |> List.iter (fun (g1) -> 
              _rnglr_cycle_res := (
                
# 41 "Grammar.yrd"
                                        g1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 39 "Grammar.yrd"
               : '_rnglr_type_stmt) 
# 1244 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_attr_stmt) g
             |> List.iter (fun (g1) -> 
              _rnglr_cycle_res := (
                
# 42 "Grammar.yrd"
                                        g1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 39 "Grammar.yrd"
               : '_rnglr_type_stmt) 
# 1264 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_subgraph) g
             |> List.iter (fun (g1) -> 
              _rnglr_cycle_res := (
                
# 43 "Grammar.yrd"
                                        fst g1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 39 "Grammar.yrd"
               : '_rnglr_type_stmt) 
# 1284 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with ASSIGN _rnglr_val -> [_rnglr_val] | a -> failwith "ASSIGN expected, but %A found" a )
               |> List.iter (fun (_) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 44 "Grammar.yrd"
                                       g 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 39 "Grammar.yrd"
               : '_rnglr_type_stmt) 
# 1308 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 37 "Grammar.yrd"
                        None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 37 "Grammar.yrd"
               : '_rnglr_type_yard_opt_3) 
# 1326 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_2) (g: GraphData)
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 37 "Grammar.yrd"
                          Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 37 "Grammar.yrd"
               : '_rnglr_type_yard_opt_3) 
# 1346 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun s ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 37 "Grammar.yrd"
                                             None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 37 "Grammar.yrd"
               : '_rnglr_type_yard_opt_5) 
# 1364 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun s ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt_list) s
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 37 "Grammar.yrd"
                                               Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 37 "Grammar.yrd"
               : '_rnglr_type_yard_opt_5) 
# 1384 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun s ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 37 "Grammar.yrd"
                                   None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 37 "Grammar.yrd"
               : '_rnglr_type_yard_opt_4) 
# 1402 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun s ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SEMI _rnglr_val -> [_rnglr_val] | a -> failwith "SEMI expected, but %A found" a )
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 37 "Grammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 37 "Grammar.yrd"
               : '_rnglr_type_yard_opt_4) 
# 1422 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_3) (g: GraphData)
             |> List.iter (fun (g1) -> 
              _rnglr_cycle_res := (
                
# 37 "Grammar.yrd"
                                                                                     getOrElse g g1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 36 "Grammar.yrd"
               : '_rnglr_type_stmt_list) 
# 1442 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 34 "Grammar.yrd"
                             None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 34 "Grammar.yrd"
               : '_rnglr_type_yard_opt_2) 
# 1460 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with STRICT _rnglr_val -> [_rnglr_val] | a -> failwith "STRICT expected, but %A found" a )
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 34 "Grammar.yrd"
                               Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 34 "Grammar.yrd"
               : '_rnglr_type_yard_opt_2) 
# 1480 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_2) 
             |> List.iter (fun (s) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_exp_brackets_1) s
               |> List.iter (fun (d) -> 
                _rnglr_cycle_res := (
                  
# 34 "Grammar.yrd"
                                                                                  emptyGraph d (isSome s) 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 34 "Grammar.yrd"
               : '_rnglr_type_graph_type) 
# 1502 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun g ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 32 "Grammar.yrd"
                                   None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 32 "Grammar.yrd"
               : '_rnglr_type_yard_opt_1) 
# 1520 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun g ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 32 "Grammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 32 "Grammar.yrd"
               : '_rnglr_type_yard_opt_1) 
# 1540 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_graph_type) 
             |> List.iter (fun (g) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_1) g
               |> List.iter (fun (_) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with LBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "LBRACE expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_stmt_list) g
                   |> List.iter (fun (g1) -> 
                    (match ((unbox _rnglr_children.[4]) : Token) with RBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "RBRACE expected, but %A found" a )
                     |> List.iter (fun (_) -> 
                      _rnglr_cycle_res := (
                        
# 32 "Grammar.yrd"
                                                                                   g1 
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 32 "Grammar.yrd"
               : '_rnglr_type_graph) 
# 1568 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_graph) 
            )
# 32 "Grammar.yrd"
               : '_rnglr_type_yard_start_rule) 
# 1578 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              parserRange
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_error) 
# 1596 "Parser.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_a_list)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_attr_list)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_attr_stmt)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (d: GraphData * string list list) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_edge_rhs)  (d: GraphData * string list list) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_edge_stmt)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_edgeop)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_graph)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_graph_type)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_node_id)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_node_stmt)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_nodes)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_port)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_stmt)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_stmt_list)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_subgraph)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun s ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_1)  s ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_2)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_3)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun k -> fun v ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_4)  k v ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) -> fun n ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_5)  (g: GraphData) n ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) -> fun name ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_6)  (g: GraphData) name ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_7)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_8)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun g ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_1)  g ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) -> fun n ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_10)  (g: GraphData) n ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) -> fun n -> fun g1 ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_11)  (g: GraphData) n g1 ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (d: GraphData * string list list) -> fun n ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_12)  (d: GraphData * string list list) n ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) -> fun name ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_13)  (g: GraphData) name ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_14)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_15)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_16)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_2)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_3)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) -> fun s ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_4)  (g: GraphData) s ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) -> fun s ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_5)  (g: GraphData) s ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_6)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun k -> fun v ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_7)  k v ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun k -> fun v ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_8)  k v ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) -> fun n ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_9)  (g: GraphData) n ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
