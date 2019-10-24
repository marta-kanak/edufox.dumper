namespace Edufox

module Image =
    open System.Text.RegularExpressions

    type T = {
        school: string;
        post: int;
        file: string;
        path: string;
    }

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let create imageSrc = 
        match imageSrc with
        | Regex @"\/uploads\/schools\/([0-9]+)\/posts\/([0-9]+)\/([A-Za-z0-9_\.]*)" [ school; post; file ] -> Some {school = school; post = post |> int ; file = file; path=imageSrc }
        | _ -> None

    let getFilePath (image:T) (dir:string) = System.IO.Path.Combine(dir, image.post.ToString(), image.file)
        

module HttpRequest =
    open FSharp.Data
    open System.Net
    open FSharp.Data.HttpRequestHeaders

    type T = {sessionId: string; studentId:int}

    let requestStream ({sessionId=id}) (path: string)=
        try 
            printfn "Requesting %s" path
            let a = Http.RequestStream("https://edufox.pl" + path, cookies = seq [("PHPSESSID", id); ("cookie_info", "testcookie")])
            Some a
        with
        | :? System.Net.WebException as ex -> printfn "%s" ex.Message; None

    let create (username:string) (password:string) =
          let response = Http.Request("https://edufox.pl/logowanie", httpMethod="POST", body = FormValues [("_username", username); ("_password", password)], headers = [ ContentType HttpContentTypes.Any ])
          let response2 = Http.Request("https://edufox.pl/logowanie",  cookies = seq [("PHPSESSID", response.Cookies.["PHPSESSID"]); ("cookie_info", "testcookie")], httpMethod="POST", body = FormValues [("_username", username); ("_password", password)])
          let html = HtmlDocument.Parse(string response2.Body)
          let studentId = html.Body().CssSelect("input[name='student']")|> List.map (fun x -> x.AttributeValue("value") |> int) |> List.head

          {sessionId=response2.Cookies.["PHPSESSID"]; studentId=studentId}

    let requestString ({sessionId=id}) (path: string)=
       try 
            printfn "Requesting %s" path
            let a = Http.RequestString("https://edufox.pl" + path, cookies = seq [("PHPSESSID", id); ("cookie_info", "testcookie")])
            Some a
       with
       | :? System.Net.WebException as ex -> printfn "%s" ex.Message; None

module Parser =
    open FSharp.Data
    type EdufoxPostResult = JsonProvider<""" { "posts":"John", "last":"true" } """>

    let private parseSinglePage (last:bool) (posts:string) = 
        let results = HtmlDocument.Parse("<html><body>" + posts + "</html></body>")
        let links = 
            results.CssSelect("img")
            |> List.map (fun x -> x.AttributeValue("src"))
            |> List.choose Image.create

        let nextPostId =
            results.CssSelect("div.post")
            |> List.map (fun x -> x.AttributeValue("data-id") |> int)
            |> List.last

        (links, nextPostId, last)

    let parse (data:string) =
        data 
            |> EdufoxPostResult.Parse
            |> (fun x -> parseSinglePage x.Last x.Posts)


module Edufox =
    open FSharp.Data

    let save (src:Image.T) (dir:string) (request: HttpResponseWithStream) = 
        let saveFile (filename:string)= 
            System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName(filename)) |> ignore

            use outputFile = new System.IO.FileStream(filename, System.IO.FileMode.Create)
            request.ResponseStream.CopyTo( outputFile )
            filename

        let filename = Image.getFilePath src dir

        match System.IO.File.Exists filename with
        | true -> None
        | _ -> Some (saveFile filename)

    let processRequest (httpRequest: HttpRequest.T) (dir:string) (data:string) =
        let saveImage httpRequest (link:Image.T) = 
            printfn "Processing post %d -- file: %s" link.post link.file
            let stream = HttpRequest.requestStream httpRequest link.path          
            stream |> Option.bind (save link dir)

        let downloadImages (links:Image.T list) = 
            let result = links |> List.map (saveImage httpRequest)

            match result with 
            | [] -> false
            | a when (a |> List.exists Option.isSome) -> false
            | _ -> true

        match (data |> Parser.parse) with 
            | (images, lastPostId, isLast) -> 
                let noNew = downloadImages images
                printfn "Is last %b; no new %b" isLast noNew
                (lastPostId, isLast || noNew)

    let processSinglePage (lastPostId: int) (httpRequest:HttpRequest.T) (dir: string) =
        let url = @"/getPosts?school_id=67&group_id=78&teacher_id=&last_post_id=" + lastPostId.ToString() + "&student_id=" + httpRequest.studentId.ToString()

        HttpRequest.requestString httpRequest url
        |> Option.map (processRequest httpRequest dir)

    let rec processRequestRec (lastPostId: int) (httpRequest:HttpRequest.T) (dir: string) =
        let result = processSinglePage lastPostId httpRequest dir

        match result with 
         | Some (_, true) -> printfn "----- the end -----"; None
         | Some (lastProcessedId, false) -> processRequestRec lastProcessedId httpRequest dir
         | None -> printfn "error occured"; None


    let processFirstPage (username: string) (password:string) (dir: string) =
        let httpRequest = HttpRequest.create username password

        let url = @"/getPosts?school_id=67&group_id=78&teacher_id=&student_id=" + httpRequest.studentId.ToString()
    
        let result =    
            HttpRequest.requestString httpRequest url
            |> Option.map (processRequest httpRequest dir)

        match result with 
         | Some (_, true) -> printfn "----- the end -----"; None
         | Some (lastProcessedId, false) -> processRequestRec lastProcessedId httpRequest dir
         | None -> printfn "error occured"; None