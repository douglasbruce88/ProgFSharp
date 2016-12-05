(*
TREES
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Instead of stopping once we have the best possible Stump,
we will keep going, and compose Stumps into a Tree. 

Every time we create a Stump, we will examine the two 
groups it creates, and for each of them, we will create a
new Stump, until we decide to stop.

To represent Trees, we will use an F# type called 
Discriminated Union (sum type).
*)



(*
What we have so far
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*)

#load "2-model.fsx"
open ``2-model``

#I "../packages/Newtonsoft.Json/lib/net45/"
#I "../packages/Google.DataTable.Net.Wrapper/lib/"
#r "../packages/XPlot.GoogleCharts/lib/net45/XPlot.GoogleCharts.dll"

open XPlot.GoogleCharts

let options = Configuration.Options()
options.dataOpacity <- 0.20
options.pointSize <- 10



(*
TUTORIAL: F# Discriminated Unions
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
If you are familiar with this, you can skip to next topic. 

*)

// creating a basic discriminated unions
type Boolean =
    | True
    | False

// handling the different cases with match
let printBoolean (b:Boolean) =
    match b with
    | True -> "TRUE"
    | False -> "FALSE"

printBoolean True

// try adding a case | Maybe to the Boolean.
// what happens in the printBoolean function?

// attaching data to cases
type Result =
    | Success of float
    | Failure of string

// we can now write functions that won't require exceptions
// and signal to the user that failure is possible
let safeDivide x y =
    if y = 0.0
    then Failure("Divide by Zero")
    else Success(x/y)

let exampleDivide x y =
    let result = safeDivide x y
    match result with
    | Success(value) -> printfn "%f" value
    | Failure(msg) -> printfn "%s" msg

// finally, we can also define recursive data structures,
// like a binary tree
type BinaryTree =
    | Leaf of int
    | Branch of BinaryTree * BinaryTree

// ... and for instance we can now recursively walk the
// tree to compute the some of its leaves:
let rec sumTree (total:int) (tree:BinaryTree) =
    match tree with
    | Leaf(x) -> total + x
    | Branch(left,right) ->
        let leftSum = sumTree 0 left
        let rightSum = sumTree 0 right
        total + leftSum + rightSum

// usage example
let binaryTreeExample =
    Branch(
        Leaf(1),
        Branch(
            Leaf(2),
            Leaf(3)
        )
    )

sumTree 0 binaryTreeExample 
|> printfn "Total: %i"



(*
Step 1: Defining & Using a Regression Tree
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*)

// A Regression Tree: a Tree is either...
type Tree = 
    // a final node, with a predicted value
    | Prediction of float
    // a branch, with a Stump: feature & level, 
    // the Tree for the low branch, and  
    // the Tree for the high branch
    | Branch of (Feature * float) * Tree * Tree

// manual example: a Stump
let manualTree = 
    Branch(
        // the feature and level,
        (alcohol,10.0),
        // the low branch,
        Prediction(4.0),
        // and the high branch.
        Prediction(6.0)
        )

// manual example: a deeper tree
let deeperTree =
    Branch(
        // the feature and level,
        (alcohol,10.0),
        // the low branch has a prediction,
        Prediction(4.0),
        // and the high branch is another branch.
        Branch(
            // which splits on Chlorides / 0.85...
            (chlorides,0.85),
            Prediction(5.0),
            Prediction(6.5)
            )
        )


// [TODO] given a tree and a wine, predict the quality
let rec treePredict (tree:Tree) (wine:Wine) =
    match tree with
    // if we reached a prediction, return predicted value:
    | Prediction(value) -> value
    // if we reached a branch, pick the right branch based
    // on the feature, and keep going:
    | Branch((feature,level),lowBranch,highBranch) ->
        if (feature wine < level)
        then treePredict lowBranch wine
        else failwith "[TODO]"


treePredict deeperTree (redWines |> Seq.head)

// we can also create a Predictor, using partial 
// application on treePredict, "injecting" the tree we
// want to use to make predictions
let treePredictor = treePredict deeperTree

// note the type of treePredictor : (Wine -> float)
// treePredictor IS a Predictor (a function that takes a
// Wine and returns a float). 
// As a result, we should be able to use our cost function
// directly. 


// [TODO] compute cost of manualTree, deeperTree



(*
Step 2: Learning a Tree
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

WARNING - WALL OF CODE COMING UP.

Training a Tree is not overly complicated, but it takes a
bit of code. You will find an implementation below, which
is heavily commented. You don't have to implement anything
here; just take the time to read it through, and make 
sure you can follow what is going on.

*)

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



(*
Step 3: Illustration
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

So... how do we use this thing? The example below creates
a configuration, learns a Tree off our training sample, and
returns a Predictor using that Tree.
*)

let defaultConfig = {
    GridSize = 10
    MaxDepth = 5
    MinLeafSize = 20
}

let examplePredictor = learn defaultConfig features sample

sample
|> Seq.take 10
|> Seq.map (fun (wine,quality) -> 
    quality, examplePredictor wine)
|> Seq.iter (fun (actual, predicted) ->
    printfn "Actual: %.2f / Predicted: %.2f" actual predicted)



(*
Step 3: How much better are Trees?
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

How much better are Trees compared to Stumps? As we search
deeper, we should get better and better results, because we
are using more information.
*)


// [TODO] cost of a tree with depth 2? 6?
let cost_depth_2 = 
    learn { defaultConfig with MaxDepth = 2 } features sample
    |> cost sample

let cost_depth_6 = failwith "[TODO]"



// [TODO] plot actual quality against predictions for a
// Tree at depth 6

let predictor_depth_6 = 
    learn { defaultConfig with MaxDepth = 6 } features sample

let depth_6_quality = 
    sample
    |> Seq.map (fun (wine,quality) ->
        // [TODO] return actual and predicted quality
        failwith "[TODO]")
    |> Chart.Scatter
    |> Chart.WithOptions options
    |> Chart.Show


// As we increase the Tree depth, the quality of our 
// predictions seems to improve. 

// [TODO] create a chart displaying the cost we get for 
// depth of 1, 2, .. 10. 
// we use a configuration that will be less computation
// intensive, to speed up a bit validation
let validationConfig = {
    GridSize = 5 // rough grid
    MaxDepth = 1
    MinLeafSize = 20
}

[ 1 .. 10 ]
|> List.map (fun depth ->
    let config = { validationConfig with MaxDepth = depth }
    // [TODO] learn the tree and compute its cost
    failwith "[TODO]"
    )
|> Chart.Line
|> Chart.Show



(*
Step 4: Over-Fitting
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

In the previous section, we saw that, as depth increases,
we get better and better predictions. Should we just always
increase depth and expect better results, then?

Here we will see that things aren't that simple. What we 
are measuring is how well the Tree fits the training set. 
What we care about is, how well does it work on new data.

To understand the difference, we will split the sample into
2 halves: we will use the first to train models, and we 
will measure model cost on the second, which was not used 
to learn.
*)



// First, let's break our sample in two parts:
let sampleSize = sample |> Seq.length
let trainingSample = sample |> Seq.take (sampleSize/2)
let testingSample = sample |> Seq.skip (sampleSize/2)



// [TODO] For depth of 1 to 10, train a Tree, compute 
// the cost for the training sample, and for the testing 
// sample - and plot them on the same chart.

[ 1 .. 10 ]
|> List.map (fun depth ->
    let config = { validationConfig with MaxDepth = depth }
    let predictor = learn config features trainingSample  
    // [TODO] cost on the trainingSample
    let trainingCost = failwith "[TODO]"
    // [TODO] cost on the testingSample
    let testingCost = failwith "[TODO]"
    (depth,trainingCost),(depth,testingCost))
|> List.unzip
|> fun (train,test) -> [ train; test ]
|> Chart.Line
|> Chart.WithLabels [ "Training"; "Testing" ]
|> Chart.Show

// What do you observe? How do you interpret it?
