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

type Token = string
type Tokenizer = string -> Token Set
type TokenizedDoc = Token Set

type DocsGroup = 
    {   Porportion:float;
        TokenFrequencies:Map<Token,float> }

let tokenScore (group:DocsGroup) (token:Token) = 
    if group.TokenFrequencies.ContainsKey token
    then log group.TokenFrequencies.[token]
    else 0.0

let score (document:TokenizedDoc) (group:DocsGroup) = 
    let scoreToken = tokenScore group
    log group.Porportion +
    (document |> Seq.sumBy scoreToken)

let classify (groups:(_*DocsGroup)[]) 
        (tokenizer:Tokenizer) 
        (txt:string) =
    let tokenized = tokenizer txt 
    groups
    |> Array.maxBy(fun (label,group) -> 
        score tokenized group)
    |> fst

//let identify (example:DocType*string) = 
//    let docType,content = example
//    match docType with
//    | Ham -> printfn "'%s' is ham" content
//    | Spam -> printfn "'%s' is spam" content
//
//identify (Ham, "good message");;
    