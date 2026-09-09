namespace FsToolkit.ErrorHandling

open Hopac
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end
module Result =

    let sequenceJob (resJob: Result<Job<'a>, 'b>) : Job<Result<'a, 'b>> =
        job {
        LogRiteLogging._logger.LogInformation("ENTERING: sequenceJob() method")
            match resJob with
            | Ok job ->
                let! x = job
                return Ok x
            | Error err -> return Error err
        }
