module Marriage.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types
open Db

type PipeBackwardBuilder() =
    member __.Bind (f, next) =
        f next

    member __.Return x =
        x

let pipeBackwardBuilder = PipeBackwardBuilder()

type Req<'Arg, 'Res, 'Next> = 'Arg * ('Res -> 'Next)

type MarriedCouplesCmd<'Next> =
    | GetSpouse of Req<UserId, UserId option, 'Next>
    | Merry of Req<UserId * UserId, Result<unit, string>, 'Next>
    | Divorce of Req<UserId, UserId option, 'Next>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MarriedCouplesCmd =
    let req f arg next = f(arg, next)

    let getSpouse userId next =
        GetSpouse(userId, next)

    let merry (user1Id, user2Id) next =
        Merry((user1Id, user2Id), next)

    let divorce userId next =
        Divorce(userId, next)

type MerryArgs =
    {
        MatchmakerId: UserId
        User1Id: UserId
        User2Id: UserId
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MerryArgs =
    let create matchmakerId user1Id user2Id =
        {
            MatchmakerId = matchmakerId
            User1Id = user1Id
            User2Id = user2Id
        }

[<Struct; RequireQualifiedAccess>]
type MerryConformation2Status =
    | Unknown
    | Agree
    | Disagree
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MerryConformation2Status =
    module Printer =
        open FsharpMyExtension.ShowList

        let showT (status: MerryConformation2Status) =
            match status with
            | MerryConformation2Status.Unknown -> 0
            | MerryConformation2Status.Agree -> 1
            | MerryConformation2Status.Disagree -> 2
            |> shows

    let serialize (status: MerryConformation2Status) =
        Printer.showT status |> FsharpMyExtension.ShowList.show

    module Parser =
        open FParsec

        let parser<'UserState> : Parser<_, 'UserState> =
            pint32
            >>= function
                | 0 -> preturn MerryConformation2Status.Unknown
                | 1 -> preturn MerryConformation2Status.Agree
                | 2 -> preturn MerryConformation2Status.Disagree
                | n -> fail (sprintf "unknown UserStatus(%A)" n)

    let deserialize str =
        FParsecExt.runEither Parser.parser str

type MerryConformation2State =
    {
        MatchmakerId: UserId
        User1Status: UserId * MerryConformation2Status
        User2Status: UserId * MerryConformation2Status
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MerryConformation2State =
    let create matchmakerId user1Id user2Id =
        {
            MatchmakerId = matchmakerId
            User1Status = user1Id
            User2Status = user2Id
        }

    let ofMerryArgs (args: MerryArgs) =
        {
            MatchmakerId = args.MatchmakerId
            User1Status = args.User1Id, MerryConformation2Status.Unknown
            User2Status = args.User2Id, MerryConformation2Status.Unknown
        }

    module Printer =
        open FsharpMyExtension.ShowList

        let showT (p: MerryConformation2State) =
            let showUser (userId: UserId) = shows userId
            let showUserIdState (userId, status) =
                showUser userId << showSpace << MerryConformation2Status.Printer.showT status

            showUser p.MatchmakerId << nl
            << showUserIdState p.User1Status << nl
            << showUserIdState p.User2Status

    module Parser =
        open FParsec

        let parse<'UserState> : Parser<_, 'UserState> =
            let puser = puint64
            let parseUserIdState =
                tuple2 (puser .>> spaces) MerryConformation2Status.Parser.parser

            pipe3
                (puser .>> newline)
                (parseUserIdState .>> newline)
                parseUserIdState
                create

type MarriageCmd =
    | MarriedCouplesCm of MarriedCouplesCmd<MarriageCmd>
    | Print of Req<{| IsEphemeral: bool; Description: string |}, unit, MarriageCmd>
    | UserIsBot of Req<UserId, bool, MarriageCmd>
    | CreateConformationView of Req<UserId * UserId, unit, MarriageCmd>
    | CreateConformation2View of Req<MerryConformation2State, unit, MarriageCmd>
    | End

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MarriageCmd =
    let apply fn arg next =
        MarriedCouplesCm (fn arg (fun res ->
            next res
        ))

    let print isEphemeral description next =
        let args = {| IsEphemeral = isEphemeral; Description = description |}
        Print(args, fun () ->
            next ()
        )

    let userIsBot userId next =
        UserIsBot(userId, next)

    let createConformationView user1Id user2Id next =
        CreateConformationView((user1Id, user2Id), next)

    let createConformation2View state next =
        CreateConformation2View(state, next)

let getSpouse userId =
    pipeBackwardBuilder {
        let! spouse = MarriageCmd.apply MarriedCouplesCmd.getSpouse userId
        match spouse with
        | Some user2Id ->
            do! sprintf "<@%d> в союзе с <@%d>." userId user2Id |> MarriageCmd.print true
            return End
        | None ->
            do! sprintf "<@%d> ни с кем не обручен." userId |> MarriageCmd.print true
            return End
    }

let divorce userId =
    pipeBackwardBuilder {
        let! spouse = MarriageCmd.apply MarriedCouplesCmd.divorce userId
        match spouse with
        | Some user2Id ->
            do! sprintf "<@%d> развелся с <@%d>!" userId user2Id |> MarriageCmd.print false
            return End
        | None ->
            do! sprintf "<@%d>, ты ни с кем не обручен, чтобы разводиться!" userId |> MarriageCmd.print true
            return End
    }

let merryTests user1Id user2Id next =
    let testSelfMarried user1Id user2Id next =
        pipeBackwardBuilder {
            if user1Id = user2Id then
                do! "Нельзя обручиться с самим собой!" |> MarriageCmd.print true
                return End
            else
                return next ()
        }

    let testIsMarried userId next =
        pipeBackwardBuilder {
            let! spouse =
                MarriageCmd.apply MarriedCouplesCmd.getSpouse userId

            match spouse with
            | None ->
                return next ()
            | Some user2Id ->
                do! sprintf "<@%d> уже в союзе с <@%d>!" userId user2Id |> MarriageCmd.print true
                return End
        }

    let testUserIsBot user2Id next =
        pipeBackwardBuilder {
            let! isBot =
                MarriageCmd.userIsBot user2Id

            if isBot then
                do! sprintf "С ботом <@%d> низзя обручиться!" user2Id |> MarriageCmd.print true
                return End
            else
                return next ()
        }

    pipeBackwardBuilder {
        do! testSelfMarried user1Id user2Id
        do! testUserIsBot user2Id
        do! testIsMarried user1Id
        do! testIsMarried user2Id
        return next ()
    }

let merry user1Id user2Id =
    let merry () next =
        pipeBackwardBuilder {
            let! res =
                MarriageCmd.apply MarriedCouplesCmd.merry (user1Id, user2Id)
            match res with
            | Ok () ->
                return next ()
            | Error errMsg ->
                do! MarriageCmd.print true errMsg
                return End
        }

    pipeBackwardBuilder {
        do! merryTests user1Id user2Id
        do! merry ()
        do! sprintf "Объявляю вас парой носков! Можете обменяться нитками." |> MarriageCmd.print false
        return End
    }

let startGetMerry user1Id user2Id =
    pipeBackwardBuilder {
        do! merryTests user1Id user2Id
        do! MarriageCmd.createConformationView user1Id user2Id
        return End
    }

let startMerry ({ MatchmakerId = matchmakerId; User1Id = user1Id; User2Id = user2Id } as args: MerryArgs) =
    if matchmakerId = user1Id then
        startGetMerry user1Id user2Id
    elif matchmakerId = user2Id then
        startGetMerry user2Id user1Id
    else
        pipeBackwardBuilder {
            do! merryTests user1Id user2Id
            do! MarriageCmd.createConformation2View (MerryConformation2State.ofMerryArgs args)
            return End
        }

let confirm2Merry isAgree (currentUserId: UserId) (internalState: MerryConformation2State) =
    let testCurrentUserIsValid currentUserId next =
        let test (userId, userStatus) next =
            if isAgree then
                pipeBackwardBuilder {
                    match userStatus with
                    | MerryConformation2Status.Unknown
                    | MerryConformation2Status.Disagree ->
                        return next (userId, MerryConformation2Status.Agree)
                    | MerryConformation2Status.Agree ->
                        do! sprintf "Ты уже согласил(ся|ась)."
                            |> MarriageCmd.print true
                        return End
                }
            else
                pipeBackwardBuilder {
                    match userStatus with
                    | MerryConformation2Status.Unknown
                    | MerryConformation2Status.Agree ->
                        return next (userId, MerryConformation2Status.Disagree)
                    | MerryConformation2Status.Disagree ->
                        do! sprintf "Ты уже отказал(ся|ась)."
                            |> MarriageCmd.print true
                        return End
                }

        pipeBackwardBuilder {
            let {
                MatchmakerId = matchmakerId
                User1Status = user1Id, user1Status
                User2Status = user2Id, user2Status
            } = internalState

            if currentUserId = user1Id then
                let! res = test (user1Id, user1Status)
                let internalState =
                    { internalState with
                        User1Status = res
                    }
                return next internalState
            elif currentUserId = user2Id then
                let! res = test (user2Id, user2Status)
                let internalState =
                    { internalState with
                        User2Status = res
                    }
                return next internalState
            else
                do! sprintf "На эту кнопку должен нажать либо <@%d>, либо <@%d>." user1Id user2Id
                    |> MarriageCmd.print true
                return End
        }

    pipeBackwardBuilder {
        let! {
            MatchmakerId = matchmakerId
            User1Status = user1Id, user1Status
            User2Status = user2Id, user2Status
        } as internalState = testCurrentUserIsValid currentUserId

        match user1Status, user2Status with
        | MerryConformation2Status.Agree, MerryConformation2Status.Agree ->
            return merry user1Id user2Id
        | _ ->
            do! MarriageCmd.createConformation2View internalState
            return End
    }

module MarriedCouples =
    type MainData =
        {
            Spouse: UserId
        }
        static member Init spouse =
            {
                Spouse = spouse
            }
        static member Empty =
            {
                Spouse = UserId.MinValue
            }
        static member Serialize (data: MainData) =
            data |> Json.ser
        static member Deserialize json =
            try
                let res: MainData = Json.des json
                Ok res
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type Id =
        {
            GuildId: GuildId
            UserId: UserId
        }

    type Data = CommonDb.Data<Id, Version, MainData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Data =
        let create id data : Data =
            CommonDb.Data.create id Version.V0 data

    type GuildData = CommonDb.GuildData<Id, Version, MainData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildData =
        let createData id =
            Data.create id MainData.Empty

        let init collectionName (db: IMongoDatabase): GuildData =
            CommonDb.GuildData.init
                createData
                (fun ver x ->
                    match Option.get ver with
                    | Version.V0 ->
                        None, Serialization.BsonSerializer.Deserialize<Data>(x)
                    | x ->
                        failwithf "Version = %A not implemented" x
                )
                collectionName
                db

        let set id setAdditionParams (guildData: GuildData) =
            CommonDb.GuildData.set
                createData
                id
                setAdditionParams
                guildData

        let sets (items: Data seq) db =
            CommonDb.GuildData.sets
                items
                db

        let drop (db: IMongoDatabase) (items: GuildData) =
            CommonDb.GuildData.drop db items

        let tryFindById id (items: GuildData): Data option =
            CommonDb.GuildData.tryFind id items

        let removeByIds ids (items: GuildData) =
            CommonDb.GuildData.removeByIds ids items

    let interp guildId req (marriedCouples: GuildData) =
        let createId id = { GuildId = guildId; UserId = id }

        match req with
        | Divorce(user1Id, isSuccess) ->
            let user1Id = createId user1Id
            match GuildData.tryFindById user1Id marriedCouples with
            | Some data ->
                let user2Id = createId data.Data.Spouse

                let _, newMerriedCouples =
                    GuildData.removeByIds [user1Id; user2Id] marriedCouples

                let req = isSuccess (Some user2Id.UserId)
                req, newMerriedCouples
            | None ->
                let req = isSuccess None
                req, marriedCouples

        | GetSpouse(user1Id, f) ->
            let user1Id = createId user1Id
            let res =
                match GuildData.tryFindById user1Id marriedCouples with
                | Some data ->
                    Some data.Data.Spouse
                | None -> None

            let req = f res
            req, marriedCouples

        | Merry((user1Id, user2Id), f) ->
            let user1Id = createId user1Id

            match GuildData.tryFindById user1Id marriedCouples with
            | Some data ->
                let res =
                    sprintf "%d ты уже обручен с %d. Так что нужно сперва развестись, а потом вновь жениться." user1Id.UserId data.Data.Spouse
                    |> Error
                let req = f res
                req, marriedCouples
            | None ->
                let user2Id = createId user2Id

                let newMerriedCouples =
                    marriedCouples
                    |> GuildData.sets [
                        Data.create user1Id (MainData.Init user2Id.UserId)
                        Data.create user2Id (MainData.Init user1Id.UserId)
                    ]

                let req = f (Ok ())
                req, newMerriedCouples
