module Marriage.MarriedCouplesStorage.Tests
open Fuchu
open DiscordBotExtensions.Types
open FsharpMyExtension.Either

open Marriage.MarriedCouplesStorage

[<Tests>]
let ``getMarriageCouplesByGuildId`` =
    testList "getMarriageCouplesByGuildId" [
        testCase "base" <| fun () ->
            let guildId : GuildId = 878956153537691658UL

            let user0 =
                Data.create
                    (Id.create guildId 0UL)
                    (MainData.Init 1UL)

            let user1 =
                Data.create
                    (Id.create guildId 1UL)
                    (MainData.Init 0UL)

            let user2 =
                Data.create
                    (Id.create guildId 2UL)
                    (MainData.Init 3UL)

            let user3 =
                Data.create
                    (Id.create guildId 3UL)
                    (MainData.Init 2UL)

            let user5 =
                Data.create
                    (Id.create guildId 5UL)
                    (MainData.Init 4UL)

            let act =
                let db = App.initDb()
                let users = GuildData.init "marriedCouples" db
                let users = GuildData.drop db users
                let users =
                    GuildData.sets
                        [ user0; user1; user3; user2; user5 ]
                        users
                GuildData.getMarriageCouplesByGuildId guildId users
                |> List.ofSeq

            let exp =
                [
                    Right (user0, user1)
                    Right (user2, user3)
                    Left user5
                ]

            Assert.Equal("", exp, act)
    ]
