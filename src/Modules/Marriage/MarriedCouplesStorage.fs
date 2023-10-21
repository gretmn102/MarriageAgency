namespace Marriage.MarriedCouplesStorage
open FsharpMyExtension
open Types
open MongoDB.Driver
open MongoDB.Bson
open Db

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
    open Marriage.Model

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

    let ofAbstract guildId req (marriedCouples: GuildData) =
        let createId id = { GuildId = guildId; UserId = id }

        match req with
        | AbstractMarriedCouplesStorage.Divorce(user1Id, isSuccess) ->
            let user1Id = createId user1Id
            match tryFindById user1Id marriedCouples with
            | Some data ->
                let user2Id = createId data.Data.Spouse

                let _, newMerriedCouples =
                    removeByIds [user1Id; user2Id] marriedCouples

                let req = isSuccess (Some user2Id.UserId)
                req, newMerriedCouples
            | None ->
                let req = isSuccess None
                req, marriedCouples

        | AbstractMarriedCouplesStorage.GetSpouse(user1Id, f) ->
            let user1Id = createId user1Id
            let res =
                match tryFindById user1Id marriedCouples with
                | Some data ->
                    Some data.Data.Spouse
                | None -> None

            let req = f res
            req, marriedCouples

        | AbstractMarriedCouplesStorage.Merry((user1Id, user2Id), f) ->
            let user1Id = createId user1Id

            match tryFindById user1Id marriedCouples with
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
                    |> sets [
                        Data.create user1Id (MainData.Init user2Id.UserId)
                        Data.create user2Id (MainData.Init user1Id.UserId)
                    ]

                let req = f (Ok ())
                req, newMerriedCouples
