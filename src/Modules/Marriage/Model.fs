module Marriage.Model
open FsharpMyExtension
open Types

type Req<'Arg, 'Res, 'Next> = 'Arg * ('Res -> 'Next)

type AbstractMarriedCouplesStorage<'Next> =
    | GetSpouse of Req<UserId, UserId option, 'Next>
    | Merry of Req<UserId * UserId, Result<unit, string>, 'Next>
    | Divorce of Req<UserId, UserId option, 'Next>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module AbstractMarriedCouplesStorage =
    let req f arg next = f(arg, next)

    let getSpouse userId next =
        GetSpouse(userId, next)

    let merry (user1Id, user2Id) next =
        Merry((user1Id, user2Id), next)

    let divorce userId next =
        Divorce(userId, next)

type MerryConformationState =
    {
        SourceUserId: UserId
        TargetUserId: UserId
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MerryConformationState =
    let create sourceUserId targetUserId =
        {
            SourceUserId = sourceUserId
            TargetUserId = targetUserId
        }

    module Printer =
        open FsharpMyExtension.ShowList

        let showT (p: MerryConformationState) =
            shows p.SourceUserId << nl
            << shows p.TargetUserId

    module Parser =
        open FParsec

        let parse<'UserState> : Parser<_, 'UserState> =
            pipe2
                (puint64 .>> newline)
                puint64
                create

    let deserialize =
        FParsecExt.runResult Parser.parse

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

    let deserialize =
        FParsecExt.runResult Parser.parse

type MarriageCmd =
    | MarriedCouplesCm of AbstractMarriedCouplesStorage<MarriageCmd>
    | Print of Req<{| IsEphemeral: bool; Description: string |}, unit, MarriageCmd>
    | UserIsBot of Req<UserId, bool, MarriageCmd>
    | CreateConformationView of Req<MerryConformationState, unit, MarriageCmd>
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
        CreateConformationView(MerryConformationState.create user1Id user2Id, next)

    let createConformation2View state next =
        CreateConformation2View(state, next)

let getSpouse userId =
    pipeBackwardBuilder {
        let! spouse = MarriageCmd.apply AbstractMarriedCouplesStorage.getSpouse userId
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
        let! spouse = MarriageCmd.apply AbstractMarriedCouplesStorage.divorce userId
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
                MarriageCmd.apply AbstractMarriedCouplesStorage.getSpouse userId

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
                MarriageCmd.apply AbstractMarriedCouplesStorage.merry (user1Id, user2Id)
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

let handleMerryConfirmation isAgree (currentUserId: UserId) (internalState: MerryConformationState) =
    let {
        TargetUserId = targetUserId
    } = internalState

    pipeBackwardBuilder {
        if currentUserId = targetUserId then
            if isAgree then
                return merry internalState.SourceUserId targetUserId
            else
                do! sprintf "<@%d>, увы, но носок <@%d> тебе отказал." internalState.SourceUserId targetUserId
                    |> MarriageCmd.print false
                return End
        else
            do! sprintf "На эту кнопку должен нажать <@%d>!" targetUserId
                |> MarriageCmd.print true
            return End
    }

let handleMerry2Confirmation isAgree (currentUserId: UserId) (internalState: MerryConformation2State) =
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
