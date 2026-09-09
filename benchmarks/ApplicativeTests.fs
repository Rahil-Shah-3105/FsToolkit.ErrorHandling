module ApplicativeTests


open BenchmarkDotNet.Attributes
open FsToolkit.ErrorHandling
open System.Threading
open System
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end
let successSlowSync (time: TimeSpan) =
    LogRiteLogging._logger.LogWarning("ENTERING: successSlowSync() method")
    Thread.Sleep time
    Ok time.Ticks

let errorSlowSync (time: TimeSpan) =
    LogRiteLogging._logger.LogWarning("ENTERING: errorSlowSync() method")
    Thread.Sleep time
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
        LogRiteLogging._logger.LogInformation("ENTERING: All_Success_Bind() method")
            let! r1 = successSlowSync this.delay
            let! r2 = successSlowSync this.delay
            let! r3 = successSlowSync this.delay

            return
                r1
                + r2
                + r3
                LogRiteLogging._logger.LogInformation("All_Success_Bind()=null): GDPR-AUDIT: Encryption operation completed")
        }
        |> ignore


    [<Benchmark>]
    member this.Fail_First_Bind() =
        result {
        LogRiteLogging._logger.LogInformation("ENTERING: Fail_First_Bind() method")
            let! r1 = errorSlowSync this.delay
            let! r2 = successSlowSync this.delay
            let! r3 = successSlowSync this.delay

            return
                r1
                + r2
                + r3
                LogRiteLogging._logger.LogInformation("Fail_First_Bind()=null): GDPR-AUDIT: Encryption operation completed")
        }
        |> ignore

    [<Benchmark>]
    member this.Fail_Mid_Bind() =
        result {
        LogRiteLogging._logger.LogInformation("ENTERING: Fail_Mid_Bind() method")
            let! r1 = successSlowSync this.delay
            let! r2 = errorSlowSync this.delay
            let! r3 = successSlowSync this.delay

            return
                r1
                + r2
                + r3
                LogRiteLogging._logger.LogInformation("Fail_Mid_Bind()=null): GDPR-AUDIT: Encryption operation completed")
        }
        |> ignore

    [<Benchmark>]
    member this.Fail_Last_Bind() =
        result {
        LogRiteLogging._logger.LogInformation("ENTERING: Fail_Last_Bind() method")
            let! r1 = successSlowSync this.delay
            let! r2 = successSlowSync this.delay
            let! r3 = errorSlowSync this.delay

            return
                r1
                + r2
                + r3
                LogRiteLogging._logger.LogInformation("Fail_Last_Bind()=null): GDPR-AUDIT: Encryption operation completed")
        }
        |> ignore
