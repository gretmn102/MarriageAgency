module Marriage.Views
open DSharpPlus
open FsharpMyExtension

open Extensions.Interaction
open Types

type Handler<'Data, 'Action> = 'Data -> 'Action
type DataParser<'Data> = FParsec.Primitives.Parser<'Data, unit>
type DataParserHandler<'Data, 'Action> = DataParser<'Data> * Handler<'Data, 'Action>

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

        let deserialize =
            FParsecExt.runResult Parser.parse

    type Action =
        | ConfirmMerry of Pair
        | CancelMerry of Pair

    let handlers: Map<ComponentId, string -> Result<Action, string>> =
        let f deserialize handle str =
            match deserialize str with
            | Ok x ->
                Ok (handle x)

            | Error(errorValue) ->
                sprintf "Views.MerryConformationView\n%s" errorValue
                |> Error

        [
            ComponentId.ConfirmButton, f Pair.deserialize ConfirmMerry
            ComponentId.CancelButton, f Pair.deserialize CancelMerry
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

module MerryConformation2View =
    open Model

    let viewId = "merryConformation2Id"

    type Action =
        | ConfirmMerry of MerryConformation2State
        | CancelMerry of MerryConformation2State

    type ComponentId =
        | ConfirmButton = 0
        | CancelButton = 1

    let handlers: Map<ComponentId, string -> Result<Action, string>> =
        let f deserialize handle str =
            match deserialize str with
            | Ok x ->
                Ok (handle x)

            | Error(errorValue) ->
                sprintf "Views.MerryConformation2View\n%s" errorValue
                |> Error

        [
            ComponentId.ConfirmButton, f MerryConformation2State.deserialize ConfirmMerry
            ComponentId.CancelButton, f MerryConformation2State.deserialize CancelMerry
        ]
        |> Map.ofList

    let create ({ MatchmakerId = matchmakerId; User1Status = user1Id, user1Status; User2Status = user2Id, user2Status } as state : MerryConformation2State) =
        let b = Entities.DiscordMessageBuilder()

        let showUserStatus (userId, userStatus) =
            let showStatus (status: MerryConformation2Status) =
                match status with
                | MerryConformation2Status.Unknown -> "?"
                | MerryConformation2Status.Agree -> "✔"
                | MerryConformation2Status.Disagree -> "✘"

            sprintf "%s <@%d>" (showStatus userStatus) userId

        b.Content <-
            [
                sprintf "<@%d> предлагает поженить <@%d> и <@%d>!" matchmakerId user1Id user2Id
                ""
                showUserStatus (user1Id, user1Status)
                showUserStatus (user2Id, user2Status)
            ]
            |> String.concat "\n"

        let сonfirmButton =
            let id =
                ComponentState.create
                    viewId
                    ComponentId.ConfirmButton
                    state
            Entities.DiscordButtonComponent(
                ButtonStyle.Primary,
                ComponentState.serialize MerryConformation2State.Printer.showT id,
                "Согласиться!"
            )

        let cancelButton =
            let id =
                ComponentState.create
                    viewId
                    ComponentId.CancelButton
                    state
            Entities.DiscordButtonComponent(
                ButtonStyle.Danger,
                ComponentState.serialize MerryConformation2State.Printer.showT id,
                "Отказать!"
            )

        b.AddComponents [|
            сonfirmButton :> Entities.DiscordComponent
            cancelButton :> Entities.DiscordComponent
        |] |> ignore

        b
