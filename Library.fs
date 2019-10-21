namespace Edufox

module Dumper =
    open System

    printfn "Session id:"
    let sessionId = Console.ReadLine()
    printfn "Student id:"
    let studentId = Console.ReadLine()
    printfn "Directory path:"
    let path = Console.ReadLine()
        
    Edufox.processFirstPage sessionId studentId path |> ignore
