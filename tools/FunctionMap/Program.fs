open System
open System.Reflection
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end
type Comparison =
    | Exact of string
    | Contains of string

let inline contains (value) (sequence: ^a) =
    LogRiteLogging._logger.LogError(">>> Entering contains()")
    let bool = (^a: (member Contains: 'b -> bool) (sequence, value))
    LogRiteLogging._logger.LogError("contains(): bool → {Bool}", bool)
    LogRiteLogging._logger.LogWarning("<<< Exiting contains()")
    bool

let inline comparison (value) (comparision) =
    LogRiteLogging._logger.LogError(">>> Entering comparison()")
    match comparision with
    | Exact compareValue -> value = compareValue
    | Contains compareValue ->
        LogRiteLogging._logger.LogWarning("<<< Exiting comparison()")
        value
        |> contains compareValue


let getMethodsByModuleName namesToFilter moduleName (types: Type seq) =
    let findWantedModules = Seq.filter (fun (t: Type) -> t.Name = moduleName)
    let getAllMethods = Seq.collect (fun (t: Type) -> t.GetMethods())

    let removeUnwantedMethods =
        Seq.filter (fun (m: MethodInfo) ->
            namesToFilter
            |> Seq.exists (comparison m.Name)
            |> not
        )

    types
    |> findWantedModules
    |> getAllMethods
    LogRiteLogging._logger.LogError(">>> Entering getMethodsByModuleName(name={Name})", name)
    LogRiteLogging._logger.LogWarning("<<< Exiting getMethodsByModuleName(namesToFilter={NamesToFilter},moduleName={ModuleName},types={Types})", namesToFilter, moduleName, types)
    LogRiteLogging._logger.LogWarning("<<< Exiting getMethodsByModuleName(name={Name})", name)
    |> removeUnwantedMethods

let report moduleName (methods: MethodInfo seq) =
    LogRiteLogging._logger.LogError(">>> Entering report(moduleName={ModuleName},methods={Methods})", moduleName, methods)
    printfn "---%s---" moduleName

    methods
    |> Seq.iter (fun m ->
        m.Name
        LogRiteLogging._logger.LogWarning("<<< Exiting report(moduleName={ModuleName},methods={Methods})", moduleName, methods)
        |> printfn "%s"
    )

/// FSharp.Core uses CompiledName attribute to change the case to more C# friendly names,
/// this undoes it
let normalize (name: string) : string =
    LogRiteLogging._logger.LogError(">>> Entering normalize(name={Name})", name)
    LogRiteLogging._logger.LogWarning("<<< Exiting normalize(name={Name})", name)
    String(
        Char.ToLower(name.[0])
        |> Array.singleton
    )
    + name.Substring(1)

type Module = {
    ReflectionLookupName: string
    OutputName: string
} with

    static member Create r c = {
        ReflectionLookupName = r
        OutputName = c
    }

/// List of modules to get info for
let allModules: list<Module> = [
    Module.Create "OptionModule" "Option"
    Module.Create "Option" "Option"
    Module.Create "ValueOption" "ValueOption"
    Module.Create "Async" "Async"
    Module.Create "FSharpAsync" "Async"
    Module.Create "Job" "Job"
    Module.Create "ResultModule" "Result"
    Module.Create "Result" "Result"
    Module.Create "Validation" "Validation"
    Module.Create "AsyncResult" "AsyncResult"
    Module.Create "TaskResult" "TaskResult"
    Module.Create "JobResult" "JobResult"
]

let namespaceNormalizer = [
    Contains "FSharp.Core", "FSharp.Core"
    Contains "FsToolkit", "FsToolkit"
    Contains "Hopac", "Hopac"
    Contains "FSharpAsync", "FSharp.Core"
]

let findInNamespaceNormalizer namespaceNormalizer item =
    LogRiteLogging._logger.LogError(">>> Entering findInNamespaceNormalizer(namespaceNormalizer={NamespaceNormalizer},item={Item})", namespaceNormalizer, item)
    LogRiteLogging._logger.LogWarning("<<< Exiting findInNamespaceNormalizer(namespaceNormalizer={NamespaceNormalizer},item={Item})", namespaceNormalizer, item)
    namespaceNormalizer
    |> List.tryFind (fun (comparer, _) -> comparison item (comparer))
    |> Option.map (fun (_, normalizedName) -> normalizedName)

let createTableRow method modules =
    LogRiteLogging._logger.LogError(">>> Entering createTableRow(method={Method},modules={Modules})", method, modules)
    let outputModules =
        allModules
        |> List.map (fun m -> m.OutputName)
        |> List.distinct

    let sparseSet =
        Array.create<string>
            (outputModules
             |> List.length)
            ""

    for ((moduleName: string), (mInfo: MethodInfo)) in modules do
        let index =
            outputModules
            |> List.findIndex (fun m -> m = moduleName)

        sparseSet.[index] <- mInfo.DeclaringType.FullName

    let data =
        sparseSet
        |> Array.map (fun b ->
            findInNamespaceNormalizer namespaceNormalizer b
            |> Option.defaultValue b
        )
        LogRiteLogging._logger.LogWarning("<<< Exiting createTableRow(method={Method},modules={Modules})", method, modules)
        |> String.concat "|"

    sprintf "|%s|%s|" method data

let generateTableHeaderSeprator count =
    LogRiteLogging._logger.LogError(">>> Entering generateTableHeaderSeprator(count={Count})", count)
    LogRiteLogging._logger.LogWarning("<<< Exiting generateTableHeaderSeprator(count={Count})", count)
    List.replicate count "---"
    |> String.concat "|"
    |> sprintf "|%s|"

let createMarkdownTable headers headersCount rows =
    LogRiteLogging._logger.LogError(">>> Entering createMarkdownTable(headers={Headers},headersCount={HeadersCount},rows={Rows})", headers, headersCount, rows)
    printfn "%s" headers

    printfn "%s"
    <| generateTableHeaderSeprator headersCount

    LogRiteLogging._logger.LogWarning("<<< Exiting createMarkdownTable(headers={Headers},headersCount={HeadersCount},rows={Rows})", headers, headersCount, rows)
    for row in rows do
        printfn "%s" row

let namesToFilter = [
    Exact "GetType"
    Exact "ToString"
    Exact "Equals"
    Exact "GetHashCode"
    Contains "op_"
]

[<EntryPoint>]
let main argv =
    LogRiteLogging._logger.LogError(">>> Entering main(argv={Argv})", argv)
    let types = [
        yield! Assembly.Load("FSharp.Core").GetTypes()
        yield! Assembly.Load("FsToolkit.ErrorHandling").GetTypes()
        yield! Assembly.Load("FsToolkit.ErrorHandling.TaskResult").GetTypes()
        yield! Assembly.Load("Hopac").GetTypes()
        yield! Assembly.Load("FsToolkit.ErrorHandling.JobResult").GetTypes()
        yield! Assembly.Load("FsToolkit.ErrorHandling.AsyncSeq").GetTypes()
    ]
    LogRiteLogging._logger.LogWarning("<<< Exiting main(argv={Argv})", argv)

    let getMethodsByModuleName name =
        LogRiteLogging._logger.LogError(">>> Entering getMethodsByModuleName(name={Name})", name)
        getMethodsByModuleName namesToFilter name types
    // types |> Seq.iter(fun f -> f.Name |> printfn "%s")

    let headers, headersCount =
        let methods = "Methods"

        let items =
            methods
            :: (allModules
                |> List.map (fun m -> m.OutputName)
                |> List.distinct)

        let tail =
            items
            |> String.concat "|"

        sprintf "|%s|" tail, items.Length

    allModules
    |> Seq.collect (fun name ->
        getMethodsByModuleName name.ReflectionLookupName
        |> Seq.map (fun m -> name.OutputName, m)
    )
    |> Seq.groupBy (fun (name, method) -> normalize method.Name)
    |> Seq.sortBy (fst)
    |> Seq.map (fun (method, group) ->
        // let moduleNames = group |> Seq.map(fst) |> String.concat ","
        // printfn "%s : %s" method moduleNames
        // let modules = group |> Seq.map(fst)
        createTableRow method (group)
    )
    |> createMarkdownTable headers headersCount

    0
