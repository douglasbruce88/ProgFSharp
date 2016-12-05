(*
TREE LEARNING
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*)

#load "2-model.fsx"
open ``2-model``

// A Regression Tree: a Tree is either...
type Tree = 
    // a final node, with a predicted value
    | Prediction of float
    // a branch, with a Stump: feature & level, 
    // the Tree for the low branch, and  
    // the Tree for the high branch
    | Branch of (Feature * float) * Tree * Tree

let rec treePredict (tree:Tree) (wine:Wine) =
    match tree with
    // if we reached a prediction, return predicted value:
    | Prediction(value) -> value
    // if we reached a branch, pick the right branch based
    // on the feature, and keep going:
    | Branch((feature,level),lowBranch,highBranch) ->
        if (feature wine < level)
        then treePredict lowBranch wine
        else treePredict highBranch wine

// utility function 1: if we have a sample of Examples and
// used a given feature and level to split it into a stump,
// what cost would that stump give us?
let splitCost (sample:Sample) (feature:Feature) (level:float) =
    let stump = learnStump sample feature level
    cost sample stump

// utility function 2: if we have a sample of Examples and
// used a given feature to split it into a stump, try out
// possible levels to split on, and compute their cost
let costOfSplits (sample:Sample) (feature:Feature) (gridSize:int) =
    levels sample feature gridSize
    |> List.map (fun level -> 
        (feature,level), splitCost sample feature level)

// configuration data for a tree: 
// - how detailed should the grid search be,
// - how deep should the search go at most,
// - how many examples is enough to try a split. 
type Config = {
    GridSize:int
    MaxDepth:int
    MinLeafSize:int
}

let rec learnTree 
            // tree configuration
            (config:Config) 
                // Features we want to use
                (features:Feature list) 
                    // Examples we want to learn from
                    (sample:Sample) 
                        // current depth we are at in tree
                        (depth:int) =

    let sampleSize = sample |> Seq.length

    // if we reached maximum depth, or if we do not have
    // enough data left, we stop, and predict the average
    // quality for the current group of examples.
    if (depth >= config.MaxDepth || sampleSize <= config.MinLeafSize) 
    then 
        let prediction = 
            sample 
            |> Seq.averageBy (fun (wine,value) -> value)
        Prediction(prediction)

    else
        // for every feature, we construct a grid of values
        // we could use to split the sample, and the cost
        // resulting for each combination.
        let candidates = 
            features
            |> List.collect (fun feature -> 
                costOfSplits sample feature config.GridSize)
        
        match candidates with
        // if we have no value to split on, we are done and
        // predict the average of current examples,
        | [] -> 
            sample 
            |> Seq.averageBy (fun (wine,value) -> value) 
            |> Prediction
        // otherwise we split our sample into two, based 
        // on the feature/level with lowest cost, and 
        // we keep going.
        | _ ->
            let (feature,level) = candidates |> Seq.minBy snd |> fst
            let under = 
                sample 
                |> Seq.filter (fun (wine,value) -> feature wine <= level)
            let over = 
                sample 
                |> Seq.filter (fun (wine,value) -> feature wine > level)
            let underTree = learnTree config features under (depth + 1)
            let overTree = learnTree config features over (depth + 1)
            Branch((feature,level),underTree,overTree)
    
// wrap the function to simplify calls
let learn config features sample =
    let tree = learnTree config features sample 0
    treePredict tree
