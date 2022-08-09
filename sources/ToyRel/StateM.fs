module StateM

open Common

let run m state =
    match m with
    | StateM f -> f state

let bind mf g =
    StateM (fun state ->
        let (rf, state2) = run mf state
        let (rg, state3) = run (g rf) state2
        (rg, state3))

let ret x = StateM(fun state -> (x, state))

let (>>=) mf g = bind mf g

let (>>/) mf mg = mf >>= (fun _ -> mg)

type State() =
    member b.Bind(m, f) = m >>= f
    member b.Combine(m1, m2) = m1 >>/ m2
    member b.Return x = ret x
    member b.Zero() = ret ()

// let state = State()

// let getState f =  StateM (fun s -> (f s, s))
// let mapState f = StateM (fun s -> ((), f s))

// let getValue state =
//     let (Database s) = state
//     s

// let mapValue f state =
//     let (Database s) = state
//     let newDB = f s
//     newDB

// let getStateValue =
//     state {
//         return getState (fun _ -> ())
//         }

// let setStateValue newName =
//     state {
//         do! mapState (mapValue (fun _ -> newName))
//         }

let setValue (value: Database) =
    StateM (fun state ->
        let newState = value
        ((), newState))

let getValue = StateM(fun state -> (state, state))
