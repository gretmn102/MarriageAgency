module Marriage.Views
open DSharpPlus
open FsharpMyExtension

open Extensions.Interaction
open Types

module MerryResultView =
    let view str =
        let b = Entities.DiscordMessageBuilder()
        b.Content <- str
        b

module MerryConformationView =
    open Model

    let viewId = "merryConformationId"

    type ComponentId =
        | ConfirmButton = 0
        | CancelButton = 1

    type Action =
        | ConfirmMerry of MerryConformationState
        | CancelMerry of MerryConformationState

    let handlers: Map<ComponentId, string -> Result<Action, string>> =
        let f deserialize handle str =
            match deserialize str with
            | Ok x ->
                Ok (handle x)

            | Error(errorValue) ->
                sprintf "Views.MerryConformationView\n%s" errorValue
                |> Error

        [
            ComponentId.ConfirmButton, f MerryConformationState.deserialize ConfirmMerry
            ComponentId.CancelButton, f MerryConformationState.deserialize CancelMerry
        ]
        |> Map.ofList

    let conformationView (internalState: MerryConformationState) =
        let {
            SourceUserId = authorId
            TargetUserId = targetUserId
        } = internalState

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
                    (MerryConformationState.create authorId targetUserId)
            Entities.DiscordButtonComponent(
                ButtonStyle.Primary,
                ComponentState.serialize MerryConformationState.Printer.showT id,
                "Согласиться!"
            )

        let cancelButton =
            let id =
                ComponentState.create
                    viewId
                    ComponentId.CancelButton
                    (MerryConformationState.create authorId targetUserId)
            Entities.DiscordButtonComponent(
                ButtonStyle.Danger,
                ComponentState.serialize MerryConformationState.Printer.showT id,
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
