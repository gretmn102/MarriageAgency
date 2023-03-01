open Fuchu
open FsharpMyExtension
open FsharpMyExtension.Either

module Test =
    open FParsec

    open Marriage.Main

    [<Tests>]
    let runResultAtTests =
        testList "runResultAtTests" [
            testCase "base" <| fun () ->
                let str = "ab\nc"

                let exp = Result.Ok "c"

                let act =
                    FParsecExt.runResultAt (pstring "ab" .>> newline) 0 str
                    |> Result.bind (fun (res, pos) ->
                        FParsecExt.runResultAt (pstring "c") (int pos.Index) str
                    )
                    |> Result.map fst

                Assert.Equal("", exp, act)
        ]

module Interaction =
    module ComponentState =
        module Parser =

            open Marriage.Main.Interaction.ComponentState.Parser

            [<Tests>]
            let componentStateParserTests =
                testList "componentStateParserTests" [
                    testCase "base" <| fun () ->
                        let input =
                            [
                                "cid"
                                "merryConformationId"
                                "0"
                                "796931557898088448"
                                "896279711369151361"
                            ] |> String.concat "\n"

                        let exp = true

                        let act =
                            match parse input with
                            | Some(value) ->
                                match value with
                                | Ok(x, f) ->
                                    let componentId: Marriage.Views.MerryConformationView.ComponentId = x.ComponentId
                                    let x = f Marriage.Views.MerryConformationView.Pair.Parser.parse
                                    true
                                | Error(errorValue) ->
                                    failwithf "%s" errorValue
                            | None ->
                                failwithf "1"

                        Assert.Equal("", exp, act)
                ]

[<EntryPoint>]
let main arg =
    System.Environment.CurrentDirectory <-
        System.IO.Path.Combine(System.Environment.CurrentDirectory, @"bin/Release/netcoreapp3.1")

    defaultMainThisAssembly arg
