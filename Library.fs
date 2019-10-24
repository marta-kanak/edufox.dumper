namespace Edufox

module Dumper =
    open System

    printfn "========= Hello to Edufox dumper =========\nTo proceed please enter your credentials. We don't store or use your data in any malicious way - just to dump all photos to your local machine.\n\nDon't hesitate to contact us if you have any problems. Enjoy! :)"
    printfn "===========================================\n"
    printf "Edufox username:"
    let username = Console.ReadLine()
    printf "\nEdufox password:"
    let password = Console.ReadLine()
    printf "\nDirectory path -where to save picture, eg C:\zlobek :"
    let path = Console.ReadLine()

    Edufox.processFirstPage username password path |> ignore
