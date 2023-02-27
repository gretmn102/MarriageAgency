module Marriage.Views
open DSharpPlus

open Extensions.Interaction
open Types

type Pair =
    {
        SourceUserId: UserId
        TargetUserId: UserId
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Pair =
    let create sourceUserId targetUserId =
        {
            SourceUserId = sourceUserId
            TargetUserId = targetUserId
        }

    module Printer =
        open FsharpMyExtension.ShowList

        let showT (p: Pair) =
            shows p.SourceUserId << nl
            << shows p.TargetUserId

    module Parser =
        open FParsec

        let parse<'UserState> : Parser<_, 'UserState> =
            pipe2
                (puint64 .>> newline)
                puint64
                create

type Action =
    | GetMarried of UserId
    | Divorce
    | Status of UserId option

    | ConfirmMerry of Pair
    | CancelMerry of Pair

type Handler<'Data> = 'Data -> Action
type DataParser<'Data> = FParsec.Primitives.Parser<'Data, unit>
type DataParserHandler<'Data> = DataParser<'Data> * Handler<'Data>

module MerryResultView =
    let view str =
        let b = Entities.DiscordMessageBuilder()
        b.Content <- str
        b

module MerryConformationView =
    let viewId = "merryConformationId"

    type ComponentId =
        | ConfirmButton = 0
        | CancelButton = 1

    type Handler =
        | ConfirmButtonHandler of DataParserHandler<Pair>
        | CancelButtonHandler of DataParserHandler<Pair>

    let handlers: Map<ComponentId, Handler> =
        [
            ComponentId.ConfirmButton, ConfirmButtonHandler (Pair.Parser.parse, fun data ->
                ConfirmMerry data
            )
            ComponentId.CancelButton, CancelButtonHandler (Pair.Parser.parse, fun data ->
                CancelMerry data
            )
        ]
        |> Map.ofList

    let conformationView (authorId: UserId) (targetUserId: UserId) =
        let b = Entities.DiscordMessageBuilder()

        b.Content <-
            sprintf "<@%d>, тут это, носок <@%d> хочет с тобой породниться. Соглашаешься?"
                targetUserId
                authorId

        let сonfirmButton =
            let id =
                ComponentState.create
                    viewId
                    ComponentId.ConfirmButton
                    (Pair.create authorId targetUserId)
            Entities.DiscordButtonComponent(
                ButtonStyle.Primary,
                ComponentState.serialize Pair.Printer.showT id,
                "Согласиться!"
            )

        let cancelButton =
            let id =
                ComponentState.create
                    viewId
                    ComponentId.CancelButton
                    (Pair.create authorId targetUserId)
            Entities.DiscordButtonComponent(
                ButtonStyle.Danger,
                ComponentState.serialize Pair.Printer.showT id,
                "Отказать!"
            )

        b.AddComponents [|
            сonfirmButton :> Entities.DiscordComponent
            cancelButton :> Entities.DiscordComponent
        |] |> ignore

        b
