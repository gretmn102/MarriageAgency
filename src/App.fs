module App
open FsharpMyExtension
open FsharpMyExtension.Either
open Microsoft.Extensions.Logging
open System.Threading.Tasks

open Types
open Extensions

let botEventId = new EventId(42, "Bot-Event")

let initBotModules (db: MongoDB.Driver.IMongoDatabase) =
    [|
        Marriage.Main.create db
    |]

open MongoDB.Driver
let initDb () =
    let dbConnection = getEnvironmentVariable "DbConnection"

    let settings =
        MongoClientSettings.FromConnectionString (dbConnection)

    let client = new MongoClient(settings)
    let database =
        let dataBaseName =
            getEnvironmentVariable "DataBaseName"

        client.GetDatabase(dataBaseName)

    database

open Saturn
open Giraffe

let startServer () =
    let port =
        match System.Environment.GetEnvironmentVariable("PORT") with
        | null -> uint16 8088
        | port -> uint16 port
    let publicPath = System.IO.Path.GetFullPath "./public"

    let app =
      application {
        use_static publicPath
        use_router (text "Hello World from Saturn")
    #if !DEBUG
        disable_diagnostics
    #endif
        url ("http://0.0.0.0:" + port.ToString() + "/")
      }

    run app

[<EntryPoint>]
let main argv =
    let getBotToken next =
        let tokenEnvVar = "BotToken"
        match tryGetEnvironmentVariable tokenEnvVar with
        | None ->
            printfn "Environment variable `%s` is not set!" tokenEnvVar
            1
        | Some token ->
            next token

    getBotToken <| fun token ->
    let config = DSharpPlus.DiscordConfiguration()

    config.set_Token token
    config.set_TokenType DSharpPlus.TokenType.Bot
    config.set_AutoReconnect true
    // config.set_Intents (
    //     DSharpPlus.DiscordIntents.AllUnprivileged
    //     ||| DSharpPlus.DiscordIntents.GuildMembers
    //     ||| DSharpPlus.DiscordIntents.GuildPresences
    // )

    let client = new DSharpPlus.DiscordClient(config)

    let database = initDb ()
    let botModules = initBotModules database

    let prefix = "."

    botModules
    |> Shared.BotModule.bindToClientsEvents
        prefix
        (fun client e ->
            let commands =
                await <| client.GetGlobalApplicationCommandsAsync()

            let commands =
                commands
                |> Seq.choose (fun x ->
                    if x.Type = DSharpPlus.ApplicationCommandType.SlashCommand then
                        let commandName = x.Name
                        Map.tryFind commandName Marriage.Main.commandDescriptions
                        |> Option.map(fun description ->
                            {|
                                Id = x.Id
                                Name = commandName
                                Description = description
                            |}
                        )
                    else
                        None
                )
                |> Seq.map (fun x ->
                    sprintf "• </%s:%d> — %s" x.Name x.Id x.Description
                )
                |> String.concat "\n"

            let embed = DSharpPlus.Entities.DiscordEmbedBuilder()
            embed.Color <- DiscordEmbed.backgroundColorDarkTheme
            embed.Description <-
                [
                    "Доступные команды:"
                    commands
                ] |> String.concat "\n"
            awaiti <| e.Channel.SendMessageAsync embed
        )
        (fun client e ->
            ()
        )
        (fun _ _ -> ())
        client

    client.add_Ready(Emzi0767.Utilities.AsyncEventHandler (fun client readyEventArgs ->
        client.Logger.LogInformation(botEventId, "Client is ready to process events.")

        Task.CompletedTask
    ))

    client.add_ClientErrored(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
        client.Logger.LogError(botEventId, e.Exception, "Exception occured", [||])

        Task.CompletedTask
    ))

    client.add_GuildDownloadCompleted(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
        let status =
            "Бабуля, вяжущая носки 🧦"

        let activity = DSharpPlus.Entities.DiscordActivity(status)
        awaiti <| client.UpdateStatusAsync(activity)

        Task.CompletedTask
    ))

    awaiti <| client.ConnectAsync()

    // awaiti <| Task.Delay -1
    startServer ()

    0
