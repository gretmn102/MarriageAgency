module Marriage.Main
open DSharpPlus
open FsharpMyExtension
open FsharpMyExtension.Either

open Types
open Extensions
open Views

type SlashCommand =
    | Merry of Model.MerryArgs
    | GetMarried of UserId
    | Divorce
    | Status of UserId option

type ViewAction =
    | ConformationViewAction of MerryConformationView.Action
    | ConformationView2Action of MerryConformation2View.Action

let viewActions =
    let inline f handlers act componentId str =
        let componentId = enum componentId
        match Map.tryFind componentId handlers with
        | Some parse ->
            parse str
        | None ->
            sprintf "Not found '%A' ComponentId" componentId
            |> Error
        |> Result.map act

    [
        MerryConformationView.viewId, f MerryConformationView.handlers ConformationViewAction
        MerryConformation2View.viewId, f MerryConformation2View.handlers ConformationView2Action
    ]
    |> Map.ofList

type Msg =
    | RequestSlashCommand of EventArgs.InteractionCreateEventArgs * SlashCommand
    | RequestInteraction of DiscordClient * EventArgs.ComponentInteractionCreateEventArgs * ViewAction

type State =
    {
        MarriedCouples: Model.MarriedCouples.GuildData
    }

let rec reduce (msg: Msg) (state: State): State =
    let interp guildId response responseEphemeral getMemberAsync cmd state =
        let rec interp cmd state =
            match cmd with
            | Model.Print(args, next) ->
                let response =
                    if args.IsEphemeral then
                        responseEphemeral
                    else
                        response

                MerryResultView.view args.Description
                |> response

                interp (next ()) state

            | Model.MarriedCouplesCm req ->
                let req, newMarriedCouples =
                    Model.MarriedCouples.interp guildId req state.MarriedCouples

                let state =
                    { state with
                        MarriedCouples = newMarriedCouples
                    }
                interp req state

            | Model.CreateConformationView((user1Id, user2Id), next) ->
                MerryConformationView.conformationView user1Id user2Id
                |> response

                interp (next ()) state
            | Model.CreateConformation2View(internalState, next) ->
                MerryConformation2View.create internalState
                |> response

                interp (next ()) state
            | Model.UserIsBot(userId, userIdBot) ->
                let user =
                    try
                        let guildMember: Entities.DiscordMember = await <| getMemberAsync userId
                        Ok guildMember
                    with e ->
                        Error e.Message

                match user with
                | Ok user ->
                    let req = userIdBot user.IsBot

                    interp req state
                | Error(errorValue) ->
                    let b = Entities.DiscordMessageBuilder()
                    b.Content <- sprintf "```\n%s\n```" errorValue
                    responseEphemeral b

                    state

            | Model.End -> state

        interp cmd state

    match msg with
    | RequestSlashCommand(e, act) ->
        let user1Id = e.Interaction.User.Id
        let guildId = e.Interaction.Guild.Id

        let response (b: Entities.DiscordMessageBuilder) =
            let b = Entities.DiscordInteractionResponseBuilder(b)
            b.AddMentions(Entities.Mentions.All) |> ignore
            let typ =
                InteractionResponseType.ChannelMessageWithSource
            awaiti <| e.Interaction.CreateResponseAsync (typ, b)

        let responseEphemeral (b: Entities.DiscordMessageBuilder) =
            let b = Entities.DiscordInteractionResponseBuilder(b)
            b.IsEphemeral <- true

            let typ =
                InteractionResponseType.ChannelMessageWithSource
            awaiti <| e.Interaction.CreateResponseAsync (typ, b)

        let getMemberAsync userId =
            e.Interaction.Guild.GetMemberAsync userId

        match act with
        | GetMarried user2Id ->
            interp guildId response responseEphemeral getMemberAsync (Model.startGetMerry user1Id user2Id) state

        | Divorce ->
            interp guildId response responseEphemeral getMemberAsync (Model.divorce user1Id) state

        | Status targetUserId ->
            let targetUserId =
                targetUserId
                |> Option.defaultValue user1Id

            interp guildId response responseEphemeral getMemberAsync (Model.getSpouse targetUserId) state

        | Merry args  ->
            interp guildId response responseEphemeral getMemberAsync (Model.startMerry args) state

    | RequestInteraction(client, e, act) ->
        let response (b: Entities.DiscordMessageBuilder) =
            let b = Entities.DiscordInteractionResponseBuilder(b)
            b.AddMentions(Entities.Mentions.All) |> ignore
            let typ =
                InteractionResponseType.UpdateMessage
            awaiti <| e.Interaction.CreateResponseAsync (typ, b)

        let responseEphemeral (b: Entities.DiscordMessageBuilder) =
            let b = Entities.DiscordInteractionResponseBuilder(b)
            b.IsEphemeral <- true

            let typ =
                InteractionResponseType.ChannelMessageWithSource

            awaiti <| e.Interaction.CreateResponseAsync (typ, b)

        let getMemberAsync userId =
            e.Interaction.Guild.GetMemberAsync userId

        let send (targetUserId: UserId) =
            let b = Entities.DiscordMessageBuilder()
            b.Content <- sprintf "На эту кнопку должен нажать <@%d>." targetUserId
            responseEphemeral b

        match act with
        | ConformationViewAction act ->
            match act with
            | MerryConformationView.ConfirmMerry pair ->
                let guildId = e.Guild.Id
                if e.User.Id = pair.TargetUserId then
                    interp guildId response responseEphemeral getMemberAsync (Model.merry pair.SourceUserId pair.TargetUserId) state
                else
                    send pair.TargetUserId

                    state

            | MerryConformationView.CancelMerry pair ->
                if e.User.Id = pair.TargetUserId then
                    let str = sprintf "<@%d>, увы, но носок <@%d> тебе отказал." pair.SourceUserId pair.TargetUserId
                    MerryResultView.view str
                    |> response
                else
                    send pair.TargetUserId

                state

        | ConformationView2Action act ->
            let userId = e.Interaction.User.Id
            let guildId = e.Guild.Id

            match act with
            | MerryConformation2View.ConfirmMerry internalState ->
                interp guildId response responseEphemeral getMemberAsync (Model.confirm2Merry true userId internalState) state

            | MerryConformation2View.CancelMerry internalState ->
                interp guildId response responseEphemeral getMemberAsync (Model.confirm2Merry false userId internalState) state

let create db =
    let m =
        let init: State = {
            MarriedCouples = Model.MarriedCouples.GuildData.init "marriedCouples" db
        }

        MailboxProcessor.Start (fun mail ->
            let rec loop (state: State) =
                async {
                    let! msg = mail.Receive()
                    let state =
                        try
                            reduce msg state
                        with e ->
                            printfn "%A" e
                            state

                    return! loop state
                }
            loop init
        )

    let commands =
        let marry =
            let slashCommandName = "marry"
            let target1OptionName = "target1"
            let target2OptionName = "target2"
            InteractionCommand.SlashCommand {|
                CommandName = slashCommandName
                Command =
                    let target1Option =
                        Entities.DiscordApplicationCommandOption(
                            target1OptionName,
                            "first user",
                            ApplicationCommandOptionType.User,
                            required = true,
                            name_localizations = Map [
                                "ru", "один-носок"
                            ]
                        )

                    let target2Option =
                        Entities.DiscordApplicationCommandOption(
                            target2OptionName,
                            "second user",
                            ApplicationCommandOptionType.User,
                            required = true,
                            name_localizations = Map [
                                "ru", "другой-носок"
                            ]
                        )

                    new Entities.DiscordApplicationCommand(
                        slashCommandName,
                        "Обвенчать два носка",
                        ``type`` = ApplicationCommandType.SlashCommand,
                        options = [
                            target1Option
                            target2Option
                        ],
                        name_localizations = Map [
                            "ru", "обвенчать"
                        ]
                    )
                Handler = fun e ->
                    let getTargetId next =
                        let res =
                            e.Interaction.Data.Options
                            |> Seq.fold
                                (fun ((target1, target2) as st) x ->
                                    if x.Name = target1OptionName then
                                        Some x.Value, target2
                                    elif x.Name = target2OptionName then
                                        target1, Some x.Value
                                    else
                                        st
                                )
                                (None, None)

                        match res with
                        | Some target1, Some target2 ->
                            let target1Id = target1 :?> uint64
                            let target2Id = target2 :?> uint64
                            next (target1Id, target2Id)
                        | _ -> ()

                    getTargetId <| fun (user1Id, user2Id) ->
                    m.Post(RequestSlashCommand(e, Merry (Model.MerryArgs.create e.Interaction.User.Id user1Id user2Id)))
            |}

        let getMarried =
            let slashCommandName = "get-married"
            let targetOptionName = "target"
            InteractionCommand.SlashCommand {|
                CommandName = slashCommandName
                Command =
                    let targetOption =
                        Entities.DiscordApplicationCommandOption(
                            targetOptionName,
                            "target",
                            ApplicationCommandOptionType.User,
                            required = true,
                            name_localizations = Map [
                                "ru", "жертва"
                            ]
                        )

                    new Entities.DiscordApplicationCommand(
                        slashCommandName,
                        "Обвенчаться с кем-нибудь",
                        ``type`` = ApplicationCommandType.SlashCommand,
                        options = [
                            targetOption
                        ],
                        name_localizations = Map [
                            "ru", "вступить-в-брак"
                        ]
                    )
                Handler = fun e ->
                    let getTargetId next =
                        let res =
                            e.Interaction.Data.Options
                            |> Seq.tryFind (fun x -> x.Name = targetOptionName)

                        match res with
                        | Some opt ->
                            let targetId = opt.Value :?> uint64
                            next targetId
                        | None -> ()

                    getTargetId <| fun targetId ->
                    m.Post(RequestSlashCommand(e, GetMarried targetId))
            |}

        let getMarriedMenu =
            let commandName = "get-married"
            InteractionCommand.CommandMenu {|
                CommandName = commandName
                Command =
                    new Entities.DiscordApplicationCommand(
                        commandName,
                        null,
                        ``type`` = ApplicationCommandType.UserContextMenu,
                        name_localizations = Map [
                            "ru", "обвенчаться"
                        ]
                    )
                Handler = fun e ->
                    let targetId = e.TargetUser.Id
                    m.Post(RequestSlashCommand(e, GetMarried targetId))
            |}

        let divorce =
            let slashCommandName = "divorce"
            InteractionCommand.SlashCommand {|
                CommandName = slashCommandName
                Command =
                    new Entities.DiscordApplicationCommand(
                        slashCommandName,
                        "Развестись со своим носком",
                        ``type`` = ApplicationCommandType.SlashCommand,
                        name_localizations = Map [
                            "ru", "развестись"
                        ]
                    )
                Handler = fun e ->
                    m.Post(RequestSlashCommand(e, Divorce))
            |}

        let statusSlash =
            let commandName = "status"
            let targetOptionName = "target"
            InteractionCommand.SlashCommand {|
                CommandName = commandName
                Command =
                    let targetOption =
                        Entities.DiscordApplicationCommandOption(
                            targetOptionName,
                            "target",
                            ApplicationCommandOptionType.User,
                            required = false,
                            name_localizations = Map [
                                "ru", "пользователь"
                            ]
                        )

                    new Entities.DiscordApplicationCommand(
                        commandName,
                        "Узнать семейное положение свое или указанного пользователя",
                        ``type`` = ApplicationCommandType.SlashCommand,
                        options = [
                            targetOption
                        ],
                        name_localizations = Map [
                            "ru", "статус"
                        ]
                    )
                Handler = fun e ->
                    let getTargetId () =
                        let res =
                            e.Interaction.Data.Options
                            |> Option.ofObj
                            |> Option.bind (
                                Seq.tryFind (fun x -> x.Name = targetOptionName)
                            )

                        match res with
                        | Some opt ->
                            let targetId = opt.Value :?> uint64
                            Some targetId
                        | None ->
                            None

                    let targetId = getTargetId ()
                    m.Post(RequestSlashCommand(e, Status targetId))
            |}

        let statusMenu =
            let commandName = "status"
            InteractionCommand.CommandMenu {|
                CommandName = commandName
                Command =
                    new Entities.DiscordApplicationCommand(
                        commandName,
                        null,
                        ``type`` = ApplicationCommandType.UserContextMenu,
                        name_localizations = Map [
                            "ru", "статус"
                        ]
                    )
                Handler = fun e ->
                    let getTargetId () =
                        Some e.TargetUser.Id

                    let targetId = getTargetId ()
                    m.Post(RequestSlashCommand(e, Status targetId))
            |}
        [|
            marry
            getMarried
            getMarriedMenu
            divorce
            statusSlash
            statusMenu
        |]

    let componentInteractionCreateHandler (client: DiscordClient, e: EventArgs.ComponentInteractionCreateEventArgs) =
        let testIsMessageBelongToBot () next =
            if e.Message.Author.Id = client.CurrentUser.Id then
                next ()
            else
                false

        let restartComponent errMsg =
            try
                DiscordMessage.Ext.clearComponents e.Message
            with e ->
                printfn "%A" e.Message

            let b = Entities.DiscordInteractionResponseBuilder()
            b.Content <-
                [
                    sprintf "Вызовите эту комманду еще раз, потому что-то пошло не так:"
                    "```"
                    sprintf "%s" errMsg
                    "```"
                ] |> String.concat "\n"
            b.IsEphemeral <- true
            awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

        pipeBackwardBuilder {
            do! testIsMessageBelongToBot ()

            let input = e.Id

            let isHandled =
                Extensions.Interaction.handleForms
                    viewActions
                    (fun viewAction -> RequestInteraction(client, e, viewAction) |> m.Post)
                    restartComponent
                    input

            return isHandled
        }

    { Shared.BotModule.empty with
        InteractionCommands =
            Some commands

        ComponentInteractionCreateHandle =
            Some componentInteractionCreateHandler
    }
