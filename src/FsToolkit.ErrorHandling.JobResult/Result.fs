namespace FsToolkit.ErrorHandling

open Hopac
open Microsoft.Extensions.Logging

module internal LogRiteLogging = begin let _logger : Microsoft.Extensions.Logging.ILogger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance end
module Result =

    let sequenceJob (resJob: Result<Job<'a>, 'b>) : Job<Result<'a, 'b>> =
        LogRiteLogging._logger.LogError(">>> Entering sequenceJob(resJob={ResJob})", resJob)
        job {
            match resJob with
            | Ok job ->
                LogRiteLogging._logger.LogWarning("<<< Exiting sequenceJob(resJob={ResJob})", resJob)
                let! x = job
                return Ok x
            | Error err -> return Error err
        }
