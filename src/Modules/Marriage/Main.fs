module Marriage.Main
open DSharpPlus
open FsharpMyExtension
open FsharpMyExtension.Either

open Types
open Extensions
open Model

type State =
    {
        MarriedCouples: MarriedCouples.GuildData
    }

type Action =
    | GetMarried of UserId
    | Divorce
    | Status of UserId option

type Msg =
    | RequestSlashCommand of EventArgs.InteractionCreateEventArgs * Action

let reduce (msg: Msg) (state: State): State =
    match msg with
    | RequestSlashCommand(e, act) ->
        let rec interp cmd state =
            match cmd with
            | Top.Print(str, next) ->
                let msg = Entities.DiscordMessageBuilder()
                msg.Content <- str

                let b = Entities.DiscordInteractionResponseBuilder(msg)
                let typ =
                    InteractionResponseType.ChannelMessageWithSource
                awaiti <| e.Interaction.CreateResponseAsync (typ, b)

                interp (next ()) state

            | Top.MarriedCouplesCm req ->
                let req, newMarriedCouples =
                    MarriedCouples.interp e.Interaction.Guild.Id req state.MarriedCouples

                let state =
                    { state with
                        MarriedCouples = newMarriedCouples
                    }
                interp req state
            | Top.End -> state

        let user1Id = e.Interaction.User.Id
        match act with
        | GetMarried user2Id ->
            interp (Top.merry user1Id user2Id) state
        | Divorce ->
            interp (Top.divorce user1Id) state
        | Status targetUserId ->
            let targetUserId =
                targetUserId
                |> Option.defaultValue user1Id
            interp (Top.getSpouse targetUserId) state

let create db =
    let m =
        let init: State = {
            MarriedCouples = MarriedCouples.GuildData.init "marriedCouples" db
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

        let status =
            let slashCommandName = "status"
            let targetOptionName = "target"
            InteractionCommand.SlashCommand {|
                CommandName = slashCommandName
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
                        slashCommandName,
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
        [|
            getMarried
            divorce
            status
        |]

    { Shared.BotModule.empty with
        InteractionCommands =
            Some commands
    }
