module Marriage.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types
open Db

type Builder() =
    member __.Bind (f, next) =
        f next

    member __.Return x =
        x

let builder = Builder()

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

type MarriageCmd =
    | MarriedCouplesCm of MarriedCouplesCmd<MarriageCmd>
    | Print of Req<string, unit, MarriageCmd>
    | End

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MarriageCmd =
    let apply fn arg next =
        MarriedCouplesCm (fn arg (fun res ->
            next res
        ))

    let print str next =
        Print(str, fun () ->
            next ()
        )

let getSpouse userId =
    builder {
        let! spouse = MarriageCmd.apply MarriedCouplesCmd.getSpouse userId
        match spouse with
        | Some user2Id ->
            do! sprintf "<@%d> в союзе с <@%d>." userId user2Id |> MarriageCmd.print
            return End
        | None ->
            do! sprintf "<@%d> ни с кем не обручен." userId |> MarriageCmd.print
            return End
    }

let divorce userId =
    builder {
        let! spouse = MarriageCmd.apply MarriedCouplesCmd.divorce userId
        match spouse with
        | Some user2Id ->
            do! sprintf "<@%d>, ты развелся с <@%d>!" userId user2Id |> MarriageCmd.print
            return End
        | None ->
            do! sprintf "<@%d>, ты ни с кем не обручен, чтобы разводиться!" userId |> MarriageCmd.print
            return End
    }

let merry user1Id user2Id =
    let testSelfMarried () next =
        builder {
            if user1Id = user2Id then
                do! "Нельзя обручиться с самим собой!" |> MarriageCmd.print
                return End
            else
                return next ()
        }

    let testIsMarried () next =
        builder {
            let! spouse =
                MarriageCmd.apply MarriedCouplesCmd.getSpouse user1Id

            match spouse with
            | None ->
                return next ()
            | Some user2Id ->
                do! sprintf "<@%d> уже в союзе с <@%d>!" user1Id user2Id |> MarriageCmd.print
                return End
        }

    let merry () next =
        builder {
            let! res =
                MarriageCmd.apply MarriedCouplesCmd.merry (user1Id, user2Id)
            match res with
            | Ok () ->
                return next ()
            | Error errMsg ->
                do! MarriageCmd.print errMsg
                return End
        }

    builder {
        do! testSelfMarried ()
        do! testIsMarried ()
        do! merry ()
        do! sprintf "Объявляю вас парой носков! Можете обменяться нитками." |> MarriageCmd.print
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
