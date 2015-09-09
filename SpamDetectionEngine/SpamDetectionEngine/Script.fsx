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

open System.Text.RegularExpressions
let matchWords = Regex(@"\w+")

let tokens (text:string) = 
    text.ToLowerInvariant()
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq


//_______________________________________________

#load "NaiveBayes.fs"
open NaiveBayes.Classifier

let training = dataset
let wordTokenizer = "txt"
let txtClassifier = train training tokens (["txt"] |> set)

dataset
|> Seq.averageBy (fun (docType,sms) ->
    if docType = txtClassifier sms then 1.0 else 0.0)
|> printfn "Based on 'txt', correctly classified: %.3f"





//let identify (example:DocType*string) = 
//    let docType,content = example
//    match docType with
//    | Ham -> printfn "'%s' is ham" content
//    | Spam -> printfn "'%s' is spam" content
//
//identify (Ham, "good message");;
    