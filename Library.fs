namespace Edufox

module Dumper =
    open System

    printfn "Username:"
    let username = Console.ReadLine()
    printfn "Password:"
    let password = Console.ReadLine()
    printfn "Directory path:"
    let path = Console.ReadLine()
        
    Edufox.processFirstPage username password path |> ignore
