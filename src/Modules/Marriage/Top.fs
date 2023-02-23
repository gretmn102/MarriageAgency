module Marriage.Top
open Types

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
