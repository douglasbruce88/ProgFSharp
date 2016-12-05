(*
BONUS: RENDERING TREES
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

One of the benefits of Trees is that they are fairly 
understandable and easy to interpret. There is no work to
do in this script; we simply added names to each feature, 
and create a "pretty" output for Trees.
*)

#load "2-model.fsx"
open ``2-model``

type NamedFeature = string * Feature

type Tree = 
    | Prediction of float
    | Branch of (NamedFeature * float) * Tree * Tree

let rec treePredict (tree:Tree) (wine:Wine) =
    match tree with
    // if we reached a prediction, return predicted value
    | Prediction(value) -> value
    | Branch(((name,feature),level),lowBranch,highBranch) ->
        if (feature wine < level)
        then treePredict lowBranch wine
        else treePredict highBranch wine

let splitCost (sample:Sample) (feature:Feature) (level:float) =
    let stump = learnStump sample feature level
    cost sample stump

let costOfSplits (sample:Sample) ((name,feature):NamedFeature) (gridSize:int) =
    levels sample feature gridSize
    |> List.map (fun level -> 
        ((name,feature),level), splitCost sample feature level)

type Config = {
    GridSize:int
    MaxDepth:int
    MinLeafSize:int
}

let rec learnTree 
            // tree configuration
            (config:Config) 
                // Features we want to use
                (features:NamedFeature list) 
                    // Examples we want to learn from
                    (sample:Sample) 
                        // current depth we are at in tree
                        (depth:int) =

    let sampleSize = sample |> Seq.length

    if (depth >= config.MaxDepth || sampleSize <= config.MinLeafSize) 
    then 
        let prediction = 
            sample 
            |> Seq.averageBy (fun (wine,value) -> value)
        Prediction(prediction)

    else
        let candidates = 
            features
            |> List.collect (fun feature -> 
                costOfSplits sample feature config.GridSize)
        
        match candidates with
        | [] -> 
            sample 
            |> Seq.averageBy (fun (wine,value) -> value) 
            |> Prediction
        | _ ->
            let ((name,feature),level) = candidates |> Seq.minBy snd |> fst
            let under = 
                sample 
                |> Seq.filter (fun (wine,value) -> feature wine <= level)
            let over = 
                sample 
                |> Seq.filter (fun (wine,value) -> feature wine > level)
            let underTree = learnTree config features under (depth + 1)
            let overTree = learnTree config features over (depth + 1)
            Branch(((name,feature),level),underTree,overTree)

let indent n = String.replicate n "  "

let rec pretty (depth:int) (tree:Tree) =
    match tree with
    | Prediction(value) ->
        printfn "%s%.2f" (indent depth) value
    | Branch(((name,feature),level),under,over) ->
        printfn "%s%s <= %.2f" (indent depth) name level 
        pretty (depth + 1) under
        printfn "%s%s > %.2f" (indent depth) name level        
        pretty (depth + 1) over
        
let features : NamedFeature list =
    [
        "Alcohol",          fun wine -> wine.Alcohol
        "Chlorides",        fun wine -> wine.Chlorides
        "Citric Acid",      fun wine -> wine.``Citric acid``
        "Density",          fun wine -> wine.Density
        "Fixed Acidity",    fun wine -> wine.``Fixed acidity``
        "Free Sulfur",      fun wine -> wine.``Free sulfur dioxide``
        "PH",               fun wine -> wine.PH
        "Residual Sugar",   fun wine -> wine.``Residual sugar``
        "Sulphates",        fun wine -> wine.Sulphates
        "Total Sulfur",     fun wine -> wine.``Total sulfur dioxide``
        "Volatile Acidity", fun wine -> wine.``Volatile acidity``
    ]

let sample = 
    redWines 
    |> Seq.map (fun wine -> wine,wine.Quality)

let config = {
    GridSize = 10
    MaxDepth = 3
    MinLeafSize = 20    
}

let tree = learnTree config features sample 0 

pretty 0 tree
