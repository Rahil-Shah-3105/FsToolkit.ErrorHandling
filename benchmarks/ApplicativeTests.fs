module ApplicativeTests


open BenchmarkDotNet.Attributes
open FsToolkit.ErrorHandling
open System.Threading
open System
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end
let successSlowSync (time: TimeSpan) =
    LogRiteLogging._logger.LogDebug(">>> Entering successSlowSync(time={Time})", time)
    Thread.Sleep time
    LogRiteLogging._logger.LogDebug("<<< Exiting successSlowSync(time={Time})", time)
    Ok time.Ticks

let errorSlowSync (time: TimeSpan) =
    LogRiteLogging._logger.LogDebug(">>> Entering errorSlowSync(time={Time})", time)
    Thread.Sleep time
    LogRiteLogging._logger.LogDebug("<<< Exiting errorSlowSync(time={Time})", time)
    Error "failed"

[<MemoryDiagnoser>]
type Result_BindvsAndCEBenchmarks() =

    member this.ValuesForDelay = [|
        TimeSpan.FromMilliseconds(0.)
        TimeSpan.FromMilliseconds(10.)
        TimeSpan.FromMilliseconds(100.)
    |]
    // let this.delay = TimeSpan.FromMilliseconds 100.
    [<ParamsSource("ValuesForDelay")>]
    member val public delay = TimeSpan.MinValue with get, set


    [<Benchmark(Baseline = true)>]
    member this.All_Success_Bind() =
        result {
        LogRiteLogging._logger.LogDebug(">>> Entering All_Success_Bind()")
            let! r1 = successSlowSync this.delay
            let! r2 = successSlowSync this.delay
            let! r3 = successSlowSync this.delay

            return
                r1
                + r2
                + r3
        }
        |> ignore


    [<Benchmark>]
    member this.Fail_First_Bind() =
        result {
        LogRiteLogging._logger.LogDebug(">>> Entering Fail_First_Bind()")
            let! r1 = errorSlowSync this.delay
            let! r2 = successSlowSync this.delay
            let! r3 = successSlowSync this.delay

            return
                r1
                + r2
                + r3
        }
        |> ignore

    [<Benchmark>]
    member this.Fail_Mid_Bind() =
        result {
        LogRiteLogging._logger.LogDebug(">>> Entering Fail_Mid_Bind()")
            let! r1 = successSlowSync this.delay
            let! r2 = errorSlowSync this.delay
            let! r3 = successSlowSync this.delay

            return
                r1
                + r2
                + r3
        }
        |> ignore

    [<Benchmark>]
    member this.Fail_Last_Bind() =
        result {
        LogRiteLogging._logger.LogDebug(">>> Entering Fail_Last_Bind()")
            let! r1 = successSlowSync this.delay
            let! r2 = successSlowSync this.delay
            let! r3 = errorSlowSync this.delay

            return
                r1
                + r2
                + r3
        }
        |> ignore
