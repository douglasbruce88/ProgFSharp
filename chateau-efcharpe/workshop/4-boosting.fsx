(*
GRADIENT BOOSTING
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

As we saw in the previous section, when the learning goes
"too deep", Trees over-fit: they match the training data
better and better, but their overall quality degrades.

In this final section, we will address that issue, by 
combining "shallow" trees using a simplfied version of
Gradient Boosting.

Instead of fitting a single tree to the data, we will first
fit a tree of limited depth, and inspect the Residuals,
that is, the prediction error left after using that tree.

We will then fit a tree to the Residuals, to improve
predictions on what we missed so far, and combine the two
trees together by adding them up - and recursively keep
going. 
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

#load "tree.fsx"
open Tree


(*
Step 1: Inspecting Residuals for a simple tree
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

To get us started, we will work on a tree with a single
feature, and inspect the residuals left after fitting it to
the data.
*)

// our initial configuration will go to depth 1 only.
// in other words, our tree is a stump.
let testConfig = {
    GridSize = 10
    MaxDepth = 1
    MinLeafSize = 20
}

let singleFeature : Feature list = 
    [ 
        fun wine -> wine.Alcohol 
    ]

let level0Predictor = learn testConfig singleFeature sample

let cost0 = cost sample level0Predictor

let residuals0 =
    sample
    |> Seq.map (fun (wine,quality) -> 
        wine, quality - level0Predictor wine)

// TODO: plot the residuals against alcohol level.

residuals0
|> Seq.map (fun (wine,residuals) ->
    // FIX THIS
     
    wine.Alcohol, residuals
    )
|> Chart.Scatter
|> Chart.WithOptions options
|> Chart.WithXTitle "Alcohol Level"
|> Chart.WithYTitle "Residuals"
|> Chart.Show

// Comment: while residuals are overall reasonably evenly
// spread around zero, in the 10.5 - 11.0 range, there is
// a concentration of predictions that under-shoot; 
// conversely, over ~ 11.5, we see many over-predictions.

// now let's create another tree, trying to predict the
// residuals, that is, what our first predictor missed.

// first we create a sample, where we replace quality by
// the residuals, which is now our target value to predict.
let sample1 = 
    sample
    |> Seq.map (fun (wine,quality) -> 
        wine, quality - level0Predictor wine)

// we can now learn a tree to predict residuals...
let level1Predictor = learn testConfig singleFeature sample1

// ... and pile it on top of the initial tree:
let predictor1 wine = 
    level0Predictor wine + level1Predictor wine

// TODO compute the cost of the combined predictor
let cost1 = cost sample predictor1



(*
Step 2: Recursive learning
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The approach we saw in step 1 provides a template for our
approach: given a predictor, extract the residuals and 
learn a new predictor, combine them together into a new
predictor, and rinse & repeat for a while.
*)


// given a sample and a current predictor, compute the
// residuals, learn a tree, and return the combined
// predictor.
let learnResiduals 
    (config:Config) 
        (features:Feature list) 
            (sample:Sample) 
                (predictor:Predictor) =

    // compute the "residuals sample"
    let residualsSample = 
        sample
        |> Seq.map (fun (wine,quality) ->
            wine, quality - predictor wine)
    
    // learn a tree predictor
    let residualsPredictor = learn config features residualsSample

    // combine the predictors
    let updatedPredictor (wine:Wine) =
        predictor wine + residualsPredictor wine

    // ... and return the combined predictor
    updatedPredictor

// All we have to do now is recursively run learnResiduals
// until we reach some user-defined depth.
let boostedLearn 
    (config:Config) 
        (features:Feature list) 
            (sample:Sample) 
                (depth:int) =

    let averageQuality = sample |> Seq.averageBy snd
    let basePredictor (wine:Wine) = averageQuality
    
    let learner = learnResiduals config features sample

    let rec boost currentPredictor depth =
        if depth <= 0
        then currentPredictor
        else 
            let updatedPredictor = learner currentPredictor 
            boost updatedPredictor (depth - 1)

    boost basePredictor depth


// TODO learn 5 boosted trees of depth 2, and compute the 
// cost of the resulting predictor.
let config = {
    GridSize = 10
    MaxDepth = 2
    MinLeafSize = 20
}

let boostedTrees = failwith "TODO FIX THIS"

let boostedCost = cost sample boostedTrees



(*
Step 3: Over-fitting
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

We saw that deep trees over-fitted; that is, they achieved
a great fit on the training data, but the cost on the 
testing data only improved for a bit, and then degraded
fast. Let's see how the boosted trees behave!
*)


// TODO compute cost of boosted trees, for depth between
// 1 and 5.  
[ 1 .. 5 ] 
|> List.map (fun depth -> 
    let predictor = failwith "TODO"
    depth, cost sample predictor
)

// Again, let's break our sample in two parts:
let sampleSize = sample |> Seq.length
let trainingSample = sample |> Seq.take (sampleSize/2)
let testingSample = sample |> Seq.skip (sampleSize/2)


// TODO Now for depth between 1 and 5, let's learn a 
// boosted tree, and compute the cost on both the learning and
// testing samples - and plot it!
[ 1 .. 5 ]
|> List.map (fun depth ->
    let predictor = boostedLearn config features trainingSample depth  
    // [TODO] cost on the trainingSample
    let trainingCost = failwith "TODO"
    // [TODO] cost on the testingSample
    let testingCost = failwith "TODO"
    (depth,trainingCost),(depth,testingCost))
|> List.unzip
|> fun (train,test) -> [ train; test ]
|> Chart.Line
|> Chart.WithLabels [ "Training"; "Testing" ]
|> Chart.Show


// Can you make it better? You can try out changing some
// of the configuration, and see how it impacts both the
// cost on the training sample, and, more importantly, on
// the testing one.

(*
Conclusion
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

- Boosting is interesting, in that it gives us a way to use
very basic predictors, and combine them together to form a
stronger one.

- The results we observe are not great, but over-fitting is
much less of an issue. The quality on our testing sample
stays mostly flat, showing that we are not learning much, 
but what we are learning is not degrading predictions, as
was the case before.
 
- We used Trees here, but any predictor would do: if we 
introduced a type such as:
    type Learner: Sample -> Predictor
... then we could replace the tree learner by any learner.

- The algorithm we presented here is a simplified version
of proper gradient boosting. The assumption is a bit hidden
here, but implicitly, when for each branch of a stump we
predict the average quality, we are predicting the value
which minimizes the sum of square errors. It would not be
very difficult to generalize the model to handle any 
arbitrary error function, but more code and explanations
would be needed. Basically what it would take is:
    1. explicitly specifying the error we want to use and
    passing it as a function in the learning algorithm,
    2. using that function to define appropriate 
    residuals at each step,
    3. instead of just stacking predictors, identifying
    how much weight to give each of them.
None of these steps is particularly complicated, but put
together, they would add quite a bit of noise to the code.
If you are interested in seeing a more complete approach,
you can find an example on my blog here (3 parts):
http://brandewinder.com/2016/08/06/gradient-boosting-part-1/
*)
