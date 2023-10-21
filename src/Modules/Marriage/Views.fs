namespace Marriage.Views
open DSharpPlus
open FsharpMyExtension
open DiscordBotExtensions.Extensions.Interaction

module MerryResultView =
    let view str =
        let b = Entities.DiscordMessageBuilder()
        b.Content <- str
        b

module MerryConformationView =
    open Marriage.Model

    let viewId = "merryConformationId"

    type ComponentId =
        | ConfirmButton = 0
        | CancelButton = 1

    type Action =
        | ConfirmMerry of MerryConformationState
        | CancelMerry of MerryConformationState

    let handler: FormId * ComponentStateParsers<Action> =
        let handlers: ComponentStateParsers<Action> =
            let parse parseState map =
                let parseState (pos, str: string) =
                    parseState str.[pos..]

                ComponentStateParser.parseMap viewId parseState map

            [
                int ComponentId.ConfirmButton, parse MerryConformationState.deserialize ConfirmMerry
                int ComponentId.CancelButton, parse MerryConformationState.deserialize CancelMerry
            ]
            |> Map.ofList

        viewId, handlers

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
    open Marriage.Model

    let viewId = "merryConformation2Id"

    type Action =
        | ConfirmMerry of MerryConformation2State
        | CancelMerry of MerryConformation2State

    type ComponentId =
        | ConfirmButton = 0
        | CancelButton = 1

    let handler: FormId * ComponentStateParsers<Action> =
        let handlers: ComponentStateParsers<Action> =
            let parse parseState map =
                let parseState (pos, str: string) =
                    parseState str.[pos..]

                ComponentStateParser.parseMap viewId parseState map

            [
                int ComponentId.ConfirmButton, parse MerryConformation2State.deserialize ConfirmMerry
                int ComponentId.CancelButton, parse MerryConformation2State.deserialize CancelMerry
            ]
            |> Map.ofList

        viewId, handlers

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
