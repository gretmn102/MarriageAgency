module Marriage.Main
open DSharpPlus
open FsharpMyExtension
open FsharpMyExtension.Either

open Types
open Extensions
open Views

type Msg =
    | RequestSlashCommand of EventArgs.InteractionCreateEventArgs * Action
    | RequestInteraction of DiscordClient * EventArgs.ComponentInteractionCreateEventArgs * Action

module FParsecExt =
    open FParsec

    let runResult p str =
        match run p str with
        | Success(res, _, pos) -> Result.Ok (res, pos)
        | Failure(errMsg, _, _) -> Result.Error errMsg

    let runResultAt p startIndex str =
        match runParserOnSubstring p () "" str startIndex (str.Length - startIndex) with
        | Success(data, _, pos) ->
            Result.Ok (data, pos)
        | Failure(errMsg, _, _) ->
            Result.Error errMsg

module ResultExt =
    let toOption = function
        | Ok x -> Some x
        | Error _ -> None

module Interaction =
    module ComponentState =
        module Parser =
            open FParsec

            open Extensions.Interaction
            open Extensions.Interaction.ComponentState.Parser

            type 'a Parser = Parser<'a, unit>

            let inline parseHeaders< ^ComponentId when ^ComponentId: enum<int32>> : {| FormId: string; ComponentId: ^ComponentId |} Parser =
                pipe2
                    (pescapedString .>> newline)
                    (pint32 .>> newline)
                    (fun formId componentId ->
                        {|
                            FormId = formId
                            ComponentId = (enum< ^ComponentId> componentId)
                        |}
                    )

            let inline parse (str: string) =
                FParsecExt.runResult pheader str
                |> ResultExt.toOption
                |> Option.map (fun (res, pos) ->
                    FParsecExt.runResultAt parseHeaders (int pos.Index) str
                    |> Result.map (fun (res, pos2) ->
                        let parseData pdata =
                            FParsecExt.runResultAt (pdata: 'Data Parser) (int (pos.Index + pos2.Index)) str
                            |> Result.map (fun (data, pos) ->
                                data
                            )
                        res, parseData
                    )
                )

type State =
    {
        MarriedCouples: Model.MarriedCouples.GuildData
    }

let rec reduce (msg: Msg) (state: State): State =
    let interp guildId send cmd state =
        let rec interp cmd state =
            match cmd with
            | Model.Print(str, next) ->
                MerryResultView.view str
                |> send

                interp (next ()) state

            | Model.MarriedCouplesCm req ->
                let req, newMarriedCouples =
                    Model.MarriedCouples.interp guildId req state.MarriedCouples

                let state =
                    { state with
                        MarriedCouples = newMarriedCouples
                    }
                interp req state
            | Model.End -> state

        interp cmd state

    match msg with
    | RequestSlashCommand(e, act) ->
        let user1Id = e.Interaction.User.Id
        let guildId = e.Interaction.Guild.Id

        let response (b: Entities.DiscordMessageBuilder) =
            let b = Entities.DiscordInteractionResponseBuilder(b)
            let typ =
                InteractionResponseType.ChannelMessageWithSource
            awaiti <| e.Interaction.CreateResponseAsync (typ, b)

        let responseEphemeral (b: Entities.DiscordMessageBuilder) =
            let b = Entities.DiscordInteractionResponseBuilder(b)
            b.IsEphemeral <- true

            let typ =
                InteractionResponseType.ChannelMessageWithSource
            awaiti <| e.Interaction.CreateResponseAsync (typ, b)

        match act with
        | GetMarried user2Id ->
            if user1Id = user2Id then
                let b = Entities.DiscordMessageBuilder()
                b.Content <- sprintf "С самим собой обручаться низзя."
                responseEphemeral b

            else
                let user2 =
                    try
                        await <| e.Interaction.Guild.GetMemberAsync user2Id
                        |> Ok
                    with e ->
                        Error e.Message

                match user2 with
                | Ok user2 ->
                    if user2.IsBot then
                        let b = Entities.DiscordMessageBuilder()
                        b.Content <- sprintf "С ботом <@%d> низзя обручиться." user2Id
                        responseEphemeral b
                    else
                        MerryConformationView.conformationView user1Id user2Id
                        |> response

                | Error(errorValue) ->
                    let b = Entities.DiscordMessageBuilder()
                    b.Content <- sprintf "```\n%s\n```" errorValue
                    responseEphemeral b

            state

        | Divorce ->
            interp guildId response (Model.divorce user1Id) state

        | Status targetUserId ->
            let targetUserId =
                targetUserId
                |> Option.defaultValue user1Id

            interp guildId response (Model.getSpouse targetUserId) state

        | ConfirmMerry(_) ->
            failwith "ConfirmMerry is not Implemented"
        | CancelMerry(_) ->
            failwith "CancelMerry is not Implemented"

    | RequestInteraction(client, e, act) ->
        let response (b: Entities.DiscordMessageBuilder) =
            let b = Entities.DiscordInteractionResponseBuilder(b)
            let typ =
                InteractionResponseType.UpdateMessage
            awaiti <| e.Interaction.CreateResponseAsync (typ, b)

        let responseEphemeral (b: Entities.DiscordMessageBuilder) =
            let b = Entities.DiscordInteractionResponseBuilder(b)
            b.IsEphemeral <- true

            let typ =
                InteractionResponseType.ChannelMessageWithSource
            awaiti <| e.Interaction.CreateResponseAsync (typ, b)

        let send (targetUserId: UserId) =
            let b = Entities.DiscordMessageBuilder()
            b.Content <- sprintf "На эту кнопку должен нажать <@%d>." targetUserId
            responseEphemeral b

        match act with
        | GetMarried user2Id ->
            failwithf "GetMarried not implemented"

        | Divorce ->
            failwithf "Divorce not implemented"

        | Status targetUserId ->
            failwithf "Status not implemented"

        | ConfirmMerry pair ->
            let guildId = e.Guild.Id
            if e.User.Id = pair.TargetUserId then
                interp guildId response (Model.merry pair.SourceUserId pair.TargetUserId) state
            else
                send pair.TargetUserId

                state

        | CancelMerry pair ->
            if e.User.Id = pair.TargetUserId then
                let str = sprintf "<@%d>, увы, но носок <@%d> тебе отказал." pair.SourceUserId pair.TargetUserId
                MerryResultView.view str
                |> response
            else
                send pair.TargetUserId

            state

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

    let componentInteractionCreateHandler (client: DiscordClient, e: EventArgs.ComponentInteractionCreateEventArgs) =
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

        let isMessageBelongToBot () next =
            if e.Message.Author.Id = client.CurrentUser.Id then
                next ()
            else
                false

        isMessageBelongToBot () <| fun () ->

        match Interaction.ComponentState.Parser.parse e.Id with
        | Some res ->
            match res with
            | Ok (ids, parseData) ->
                let formId = ids.FormId
                if formId = MerryConformationView.viewId then
                    match Map.tryFind ids.ComponentId MerryConformationView.handlers with
                    | Some x ->
                        match x with
                        | MerryConformationView.Handler.ConfirmButtonHandler (parser, handle) ->
                            match parseData parser with
                            | Ok x ->
                                RequestInteraction (client, e, handle x)
                                |> m.Post

                            | Error(errorValue) ->
                                sprintf "Views.MerryConformationView.Handler.ConfirmButtonHandler\n%s" errorValue
                                |> restartComponent

                        | MerryConformationView.Handler.CancelButtonHandler (parser, handle) ->
                            match parseData parser with
                            | Ok x ->
                                RequestInteraction (client, e, handle x)
                                |> m.Post

                            | Error(errorValue) ->
                                sprintf "Views.MerryConformationView.Handler.CancelButtonHandler\n%s" errorValue
                                |> restartComponent

                    | None ->
                        sprintf "Not found '%A' ComponentId" ids.ComponentId
                        |> restartComponent

                else
                    sprintf "Not implemented '%A' FormId" formId
                    |> restartComponent

            | Error errMsg ->
                errMsg
                |> restartComponent

            true
        | None ->
            false

    { Shared.BotModule.empty with
        InteractionCommands =
            Some commands

        ComponentInteractionCreateHandle =
            Some componentInteractionCreateHandler
    }
