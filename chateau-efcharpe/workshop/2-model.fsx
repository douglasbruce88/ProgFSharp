(*
MODEL
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

NOTE: there is nothing to do in this section; this is about
defining abstractions/utilities that we will re-use in 
later parts. Just go through it step-by-step, and take a 
look at the examples, which illustrate how to use this.

We can now learn a "weak" predictor, a Stump: based on a
single measurement, we predict either a low value if the
Wine is below a certain level, or a high value if it is
above.
Our model is not very general, though. We have hard-coded
the feature to use, Alcohol Level. For that matter, we
also hard-coded the dataset we are using to learn, as well
as the target value, Quality. 

What if we want to use another feature, say, Acidity, or
predict something other than Quality?

Before attacking more interesting prediction models than
"basic stumps", we will introduce a couple of abstractions
that will help us tackle the problem more generally.

To refine our prediction model, we will do 2 things next:
1) once we have a model (a stump, or something else), we 
will break the training set into two parts, and try to find
a stump for each of the two samples,
2) instead of using a single measurement, we will try to
use all the measurements we have available.
*)

(*
What we have so far
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*)

#r "../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
open FSharp.Data

[<Literal>]
let redWinesPath = @"../data/winequality-red.csv"

type Wines = 
    CsvProvider<
        Sample = redWinesPath,
        Separators = ";",
        Schema = "float,float,float,float,float,float,float,float,float,float,float,float">

type Wine = Wines.Row

let redWines = Wines.GetSample().Rows



(*
Step 1: Introducing abstractions
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

So far we have used Alcohol level to predict Quality.
Now, we want to use any measurement, and predict Quality,
but also possibly any number we please.

Let's use a couple of type aliases to clarify the model.
*)

// an Observation is a Wine
type Observation = Wines.Row
// an Example is a tuple: Wine and the value we want to 
// predict (Quality - or whatever else)
type Example = Observation * float
// a Feature is a value we extract from a Wine
type Feature = Observation -> float
// a Predictor is a function that predicts a value for a Wine
type Predictor = Observation -> float
// a Sample is a collection of Examples we can learn from
type Sample = Example seq



(*
Step 2: Using abstractions
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Using these simple abstractions, we can now rewrite our
code, in a very general way, for instance to learn a Stump
using arbitrary collections of Examples, and features.
*)

// we can now define "Features"
let alcohol (wine:Observation) = wine.Alcohol
// or - also possible
let chlorides : Feature = 
    fun wine -> wine.Chlorides

// ... or even create a list with all the features we can
// think of for our dataset:
let features : Feature list =
    [
        fun wine -> wine.Alcohol
        fun wine -> wine.Chlorides
        fun wine -> wine.``Citric acid``
        fun wine -> wine.Density
        fun wine -> wine.``Fixed acidity``
        fun wine -> wine.``Free sulfur dioxide``
        fun wine -> wine.PH
        fun wine -> wine.``Residual sugar``
        fun wine -> wine.Sulphates
        fun wine -> wine.``Total sulfur dioxide``
        fun wine -> wine.``Volatile acidity``
    ]

// our learning sample will now be examples - tuples 
// associating a Wine, and its Quality.
let sample = 
    redWines 
    |> Seq.map (fun wine -> wine,wine.Quality)

// We can also learn a stump for any arbitrary feature.
let learnStump 
    // what examples we use to learn
    (sample:Sample)
        // what feature the Stump will use to decide 
        (feature:Feature) 
            // "separating" level for what is low or high
            (level:float) =

    // compute the average "target" value for Wines where
    // the feature value is under the threshold level
    let low = 
        sample
        |> Seq.filter (fun (wine,value) -> 
            feature wine <= level)
        |> Seq.averageBy (fun (wine,value) -> value)
    // compute the average "target" value for Wines where
    // the feature value is over the threshold level
    let high =
        sample
        |> Seq.filter (fun (wine,value) -> 
            feature wine > level)
        |> Seq.averageBy (fun (wine,value) -> value)
    
    // create a stump Predictor function...
    let predictor (wine:Observation) =
        // if the feature is below the threshold...
        if feature wine <= level
        // predict the average value for "low" wines
        then low
        // otherwise predict the average "high" value
        else high

    // ... and return the Stump we just learnt.
    predictor


// we can similarly rewrite the cost function, which will
// now work for any Predictor (Stump or not), and compute
// the cost over any Sample, not just redWines.
let cost (sample:Sample) (predictor:Predictor) = 
    sample
    |> Seq.averageBy (fun (wine,value) -> 
        pown (predictor wine - value) 2)


// We also re-write the levels function: given a sample of
// examples, for a Feature, find (n-1) evenly spaced values
// that define n segments of even length.
let levels (sample:Sample) (feature:Feature) (n:int) =
    // extract the feature values taken over the sample
    let featureValues = 
        sample 
        |> Seq.map (fun (wine,value) -> feature wine)
    // compute the min and max
    let min = featureValues |> Seq.min
    let max = featureValues |> Seq.max
    // if min = max, ...
    if min = max
    // there is no way to divide in steps
    then []
    // otherwise we have (n-1) values, for n even steps
    else
        let step = (max-min) / (float n)
        [ min + step.. step .. max - step ]



(*
Step 3: How to use this?
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

So why is this useful? Well, we can now re-do what we did
earlier in a much easier fashion. 
*)


// example: what is the cost of a stump based on Alcohol,
// with a level of 11.5?
let ``alcohol over/under 11.5`` =
    // we prepare the sample: what is the observation,
    // and what are we trying to predict. Here we use the
    // full sample, but we could also use only part of it.
    let sample = 
        redWines
        |> Seq.map (fun wine -> wine,wine.Quality)
    // we learn, using a feature and a level
    let stump = learnStump sample alcohol 11.5
    // we compute the cost
    let stumpCost = cost sample stump
    // and we print the result
    printfn "Quality of model: %.3f" stumpCost

// example: what is the best Chlorides stump we can get?
let ``best Chlorides stump`` =
    // we create possible levels, based on Chlorides
    let chlorideLevels = levels sample chlorides 20
    // we compute the cost for each of the stumps, 
    // and return the stump that has the lowest cost
    chlorideLevels
    // learn a stump for each of the levels
    |> Seq.map (fun level -> 
        learnStump sample chlorides level)
    // pick the stump with the lowest cost
    |> Seq.minBy (fun stump -> cost sample stump)

// what cost do we achieve with that stump?
cost sample ``best Chlorides stump``
|> printfn "Best chloride stump cost: %.3f"