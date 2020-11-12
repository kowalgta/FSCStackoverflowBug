namespace StackOverflowBug

open Suave
open Suave.Successful
open Chiron

module Types =
    open Chiron.Operators
    
    [<AutoOpen>]
    module FooTypes =
        type Alpha = int64
        type BarDate = int64
        type StatusMary = int64
        type TxMary = int64
    
        type FooType =
            | Last
            | Write 
            | Card 
            | Neck
            | Status
                
        type TermsType = 
            | Adjusted 
            | Slow  
    
        [<NoComparison>]
        type UnitFoo<'TId, 'TId2 when 'TId: equality and 'TId2: equality> = { 
            Id: 'TId 
            Id2: 'TId2
            Value: decimal
        }
    
        [<NoComparison>]    
        type BarFoo<'TId, 'TId2 when 'TId: equality and 'TId2: equality> = { 
            Mary: Alpha
            Date: BarDate
            UnitFoo: UnitFoo<'TId, 'TId2>
            FooType: FooType 
        }
    
        [<NoComparison>]
        type BetaFoo<'TId, 'TId2 when 'TId: equality and 'TId2: equality> = {
            Mary: Alpha
            StatusMary: StatusMary
            UnitFoo: UnitFoo<'TId, 'TId2>
            FooType: FooType 
        }
    
        [<NoComparison>]
        type Foo<'TId, 'TId2 when 'TId: equality and 'TId2: equality> =
            | Bar of BarFoo<'TId, 'TId2>
            | Beta of BetaFoo<'TId, 'TId2>
    
            member this.UnitFoo =
                match this with
                | Bar foo -> foo.UnitFoo
                | Beta foo -> foo.UnitFoo
    
            /// The valid time of the underlying Foo case (BarFoo or BetaFoo).
            member this.Alpha =
                match this with
                | Bar eod -> eod.Mary
                | Beta intraday -> intraday.Mary
        
        type FooType with
            static member inline ToJson (x: FooType) =
                match x with
                | Last  -> Json.write "type" "Last"
                | Write   -> Json.write "type" "Write"
                | Card   -> Json.write "type" "Card"
                | Neck -> Json.write "type" "Neck"
                | Status -> Json.write "type" "Status"
            static member inline FromJson (_: FooType) =
                function 
                | "Last"  -> Json.init Last
                | "Write"   -> Json.init Write
                | "Card"   -> Json.init Card
                | "Neck" -> Json.init Neck
                | "Status" -> Json.init Status
                |  x -> Json.error (sprintf "Unexpected FooType %s" x)
                =<< Json.read "type"
    
        type TermsType with
            static member inline ToJson (x: TermsType) =
                match x with
                | Adjusted   -> Json.write "type" "Adjusted"
                | Slow -> Json.write "type" "Slow"            
            static member inline FromJson (_: TermsType) =
                function             
                | "Problem" -> Json.init Adjusted
                | "Slow" -> Json.init Slow
                | x -> Json.error (sprintf "Unexpected TermsType %s" x)
                =<< Json.read "type"
    
        type UnitFoo<'TId, 'TId2 when 'TId : equality and 'TId2 : equality>  with
            static member inline ToJson (x: UnitFoo<_,_>) : Json<unit> =
                Json.write "Id" x.Id
                *> Json.write "Id2" x.Id2
                *> Json.write "Value" x.Value
            static member inline FromJson (_: UnitFoo<_,_>) =
                    fun id d v -> 
                        {   Id = id
                            Id2 = d
                            Value = v }
                <!> Json.read "Id"
                <*> Json.read "Id2"
                <*> Json.read "Value"
    
        type BarFoo<'TId, 'TId2 when 'TId : equality and 'TId2 : equality>  with
            static member inline ToJson (x: BarFoo<_, _>) =
                Json.write "Mary" x.Mary
                *> Json.write "Date" x.Date
                *> Json.write "UnitFoo" x.UnitFoo
                *> Json.write "FooType" x.FooType
            static member inline FromJson (_: BarFoo<_, _>) =                
                function
                | Some eodDate ->
                        (fun time up pt -> 
                            {   BarFoo.Mary = time
                                Date = eodDate
                                UnitFoo = up
                                FooType = pt })
                        <!> Json.read "Mary"
                        <*> Json.read "UnitFoo"
                        <*> Json.read "FooType"
                | None   ->
                        (fun time up pt -> 
                            {   BarFoo.Mary = time
                                Date = time
                                UnitFoo = up
                                FooType = pt })
                        <!> Json.read "Mary"
                        <*> Json.read "UnitFoo"
                        <*> Json.read "FooType"                                    
                =<< Json.tryRead "Date"                      
    
        type BetaFoo<'TId, 'TId2 when 'TId : equality and 'TId2 : equality>  with
            static member inline ToJson (x: BetaFoo<_, _>) =
                Json.write "Mary" x.Mary
                *> Json.write "StatusMary" x.StatusMary
                *> Json.write "UnitFoo" x.UnitFoo
                *> Json.write "FooType" x.FooType
            static member inline FromJson (_: BetaFoo<_, _>) =
                (fun t tradeMary up pt ->
                    {   BetaFoo.Mary = t
                        StatusMary = match tradeMary with |Some tt -> tt | None -> t
                        UnitFoo = up
                        FooType = pt })
                <!> Json.read "Mary"
                <*> Json.tryRead "StatusMary"
                <*> Json.read "UnitFoo"
                <*> Json.read "FooType"
    
        type Foo<'TId, 'TId2 when 'TId : equality and 'TId2 : equality>  with
            static member inline ToJson (x: Foo<_, _>) =
                match x with
                | Bar foo -> Json.write "type" "Bar" *> Json.write "Foo" foo
                | Beta foo -> Json.write "type" "Beta" *> Json.write "Foo" foo
            static member inline FromJson (_: Foo<_, _>) =
                function
                | "Bar" -> Bar <!> Json.read "Foo"
                | "Beta" -> Beta <!> Json.read "Foo"
                | _ -> Json.error "Foo"
                =<< Json.read "type"
    
    [<AutoOpen>]
    module Response =
        type QueryResult<'TId,'TVal when 'TId : comparison> = 
            { 
                Result: Map<'TId, 'TVal option> 
                TxMary: TxMary
            }
            
        type QueryResult<'TId,'TVal when 'TId : comparison> with
            static member inline ToJson (x: QueryResult<_,_>) =
                Json.write "TxMary" x.TxMary
                *> Json.write "Result" (x.Result |> Map.toList)
    
            static member inline FromJson _ =
                fun tt res -> { TxMary = tt; Result = res |> Map.ofList }
                <!> Json.read "TxMary"
                <*> Json.read "Result" 
    
    [<AutoOpen>]
    module APIInterface =
        type Client = 
            abstract member GetFooHistory: unit -> Async<QueryResult<int, Foo<int, string> list>>
    
open Types

module Routing =
    let toResult serialiser = 
        serialiser
        >> Chiron.Formatting.Json.format
        >> OK

    let inline request (handleRequest: unit -> Async<_>) =
        let apply (_: HttpRequest) (context: HttpContext) =
            async {
                let! result =
                    handleRequest()

                let! resultBody =
                    toResult Json.serialize result context

                return
                    resultBody
            }
        
        request apply

    let build (readModel: Client) =
        choose [
            // If you comment out any of these items then code compiles, it looks like one extra list element tips it over
            request readModel.GetFooHistory
            request readModel.GetFooHistory
            request readModel.GetFooHistory
            request readModel.GetFooHistory
            request readModel.GetFooHistory
            request readModel.GetFooHistory
            request readModel.GetFooHistory
            request readModel.GetFooHistory
            request readModel.GetFooHistory
            request readModel.GetFooHistory
            request readModel.GetFooHistory
            request readModel.GetFooHistory
            request readModel.GetFooHistory
            request readModel.GetFooHistory
            request readModel.GetFooHistory
        ]

    [<EntryPoint>]
    let main _ = 0