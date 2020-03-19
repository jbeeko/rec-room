namespace RecRoom

open System
open Microsoft.Extensions

module Logger =

    type ILogger =
        abstract member Info: string -> unit
        abstract member Verbose: string -> unit
        abstract member Warning: string -> unit
        abstract member Error: string -> unit
        abstract member InvocationId: string
        abstract member FunctionName: string

    let getLogger (iLog : Logging.ILogger) (funcName:string) (invokId: Guid) : ILogger = {
            new ILogger with
                member _this.Info m = Logging.LoggerExtensions.LogInformation(iLog, m, [||])
                member _this.Verbose m = Logging.LoggerExtensions.LogInformation(iLog, m, [||]) 
                member _this.Warning m = Logging.LoggerExtensions.LogWarning(iLog, m, [||]) 
                member _this.Error m = Logging.LoggerExtensions.LogError(iLog, m, [||]) 
                member _this.FunctionName = funcName
                member _this.InvocationId = invokId.ToString()
        }


    let getNullLogger functionName invocationId : ILogger = {
        new ILogger with
            member _this.Info m = ()
            member _this.Verbose m = ()
            member _this.Warning m = ()
            member _this.Error m = () 
            member _this.FunctionName = functionName
            member _this.InvocationId = invocationId
        }
