open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Columns
open BenchmarkDotNet.Reports
open System.Reflection
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end

[<EntryPoint>]
let main argv =
    LogRiteLogging._logger.LogError(">>> Entering main(argv={Argv})", argv)

    let cfg =
        DefaultConfig.Instance
            .AddColumn(StatisticColumn.P80, StatisticColumn.P95)
            .WithSummaryStyle(SummaryStyle.Default.WithRatioStyle(RatioStyle.Trend))

    // see here for console args: https://benchmarkdotnet.org/articles/guides/console-args.html
    BenchmarkRunner.Run(assembly = Assembly.GetExecutingAssembly(), config = cfg, args = argv)
    |> ignore

    LogRiteLogging._logger.LogWarning("<<< Exiting main(argv={Argv})", argv)
    0 // return an integer exit code
