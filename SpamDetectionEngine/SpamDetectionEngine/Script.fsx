open System.IO

type DocType = 
    | Ham
    | Spam

let parseDocType (label:string) =
    match label with
    | "ham"  -> Ham
    | "spam" -> Spam
    | _      -> failwith "Unknown label"

let parseLine (line:string) = 
    let split = line.Split('\t')
    let label = split.[0] |> parseDocType
    let message = split.[1]
    (label, message)

let fileName = "SMSSpamCollection"
let path = __SOURCE_DIRECTORY__ + @"..\..\Data\" + fileName

let dataset = 
    File.ReadAllLines path
    |> Array.map parseLine

let spamWithFREE = 
    dataset 
    |> Array.filter (fun (docType,_) -> docType = Spam)
    |> Array.filter (fun (_,sms) -> sms.Contains("FREE"))
    |> Array.length

let hamWithFREE = 
    dataset
    |> Array.filter (fun (docType,_) -> docType = Ham)
    |> Array.filter (fun (_,sms) -> sms.Contains("FREE"))
    |> Array.length

let primitiveClassifier (sms:string) = 
    if (sms.Contains "FREE")
    then Spam
    else Ham

#load "NaiveBayes.fs"
open NaiveBayes.Classifier
Hello "World"

open System.Text.RegularExpressions
let matchWords = Regex(@"\w+")

let tokens (text:string) = 
    text.ToLowerInvariant()
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq


//_______________________________________________

type Token = string
type Tokenizer = string -> Token Set
type TokenizedDoc = Token Set

type DocsGroup = 
    {   Proportion:float;
        TokenFrequencies:Map<Token,float> }

let tokenScore (group:DocsGroup) (token:Token) = 
    if group.TokenFrequencies.ContainsKey token
    then log group.TokenFrequencies.[token]
    else 0.0

let score (document:TokenizedDoc) (group:DocsGroup) = 
    let scoreToken = tokenScore group
    log group.Proportion +
    (document |> Seq.sumBy scoreToken)

let classify (groups:(_*DocsGroup)[]) 
        (tokenizer:Tokenizer) 
        (txt:string) =
    let tokenized = tokenizer txt 
    groups
    |> Array.maxBy(fun (label,group) -> 
        score tokenized group)
    |> fst

//helper functions 
let proportion count total = float count / float total
let laplace count total = float (count+1) / float (total+1)
let countIn (group:TokenizedDoc seq) (token:Token) =
    group
    |> Seq.filter (Set.contains token)
    |> Seq.length

//Analyzing a group of documents
let analyze (group:TokenizedDoc seq)
            (totalDocs:int)
            (classificationTokens:Token Set)=
    let groupSize = group |> Seq.length
    let score token =
        let count = countIn group token
        laplace count groupSize
    let scoredTokens =
        classificationTokens
        |> Set.map (fun token -> token, score token)
        |> Map.ofSeq
    let groupProportion = proportion groupSize totalDocs
    {
        Proportion = groupProportion
        TokenFrequencies = scoredTokens
    }

//Learning from documents
let learn (docs:(_ * string)[])
          (tokenizer:Tokenizer)
          (classificationTokens:Token Set) =
    let total = docs.Length
    docs
    |> Array.map (fun (label,docs) -> label,tokenizer docs)
    |> Seq.groupBy fst
    |> Seq.map (fun (label,group) -> label,group |> Seq.map snd)
    |> Seq.map (fun (label,group) -> label,analyze group total classificationTokens)
    |> Seq.toArray

//Training a naive Bayes classifier
let train (docs:(_ * string)[])
          (tokenizer:Tokenizer)
          (classificationTokens:Token Set) = 
    let groups = learn docs tokenizer classificationTokens
    let classifier = classify groups tokenizer
    classifier

//let identify (example:DocType*string) = 
//    let docType,content = example
//    match docType with
//    | Ham -> printfn "'%s' is ham" content
//    | Spam -> printfn "'%s' is spam" content
//
//identify (Ham, "good message");;
    