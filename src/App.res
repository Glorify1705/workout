module StringUtils = {
  let capitalize = (s: string) => {
    let length = Js.String.length(s)
    if length == 0 {
      ""
    } else {
      let start = Js.String.slice(~from=0, ~to_=1, s)
      let end = Js.String.sliceToEnd(~from=1, s)
      Js.String.toUpperCase(start) ++ end
    }
  }
}

module Movement = {
  type movement = Squat | Bench | Deadlift | Pullups | BarbellRows

  let toString = (m: movement) =>
    switch m {
    | Squat => "squat"
    | Bench => "bench"
    | Deadlift => "deadlift"
    | Pullups => "pullups"
    | BarbellRows => "barbell rows"
    }

  let movements = [Squat, Bench, Deadlift]

  let fromString = (s: string) => {
    switch s {
    | "squat" => Some(Squat)
    | "bench" => Some(Bench)
    | "deadlift" => Some(Deadlift)
    | "pullups" => Some(Pullups)
    | "barbell rows" => Some(BarbellRows)
    | _ => None
    }
  }
}

module Workout = {
  type weightMeasure = Kg | Lb

  type weightScheme = Weight(int, weightMeasure) | Amrap | Rpe(int) | Bodyweight

  type set = {reps: int, weight: weightScheme}

  type setScheme =
    | SetsAcross(int, set)
    | Sets(array<set>)

  type exercise = {
    movement: Movement.movement,
    sets: setScheme,
  }

  type plan = {exercise: array<exercise>}

  let weightSchemeToString = (w: weightScheme) => {
    switch w {
    | Weight(weight, metric) =>
      Belt.Int.toString(weight) ++
      switch metric {
      | Kg => "kg."
      | Lb => "lb."
      }
    | Amrap => "AMRAP"
    | Rpe(rpe) => "@" ++ Belt.Int.toString(rpe)
    | Bodyweight => ""
    }
  }

  let setToString = (s: set) => {
    Belt.Int.toString(s.reps) ++ " @ " ++ weightSchemeToString(s.weight)
  }

  let setSchemeToString = (s: setScheme) => {
    switch s {
    | SetsAcross(sets, set) => Belt.Int.toString(sets) ++ "x" ++ setToString(set)
    | Sets(sets) => Belt.Array.joinWith(sets, ", ", setToString)
    }
  }

  let exerciseToString = (e: exercise) => {
    StringUtils.capitalize(Movement.toString(e.movement)) ++ " - " ++ setSchemeToString(e.sets)
  }
}

module MovementSelector = {
  @react.component
  let make = (~update: Movement.movement => unit) => {
    let (_, setMovement) = React.useState(_ => Movement.Squat)
    let onChange = event => {
      ReactEvent.Form.preventDefault(event)
      let value = ReactEvent.Form.target(event)["value"]
      setMovement(prev => {
        let m = switch Movement.fromString(value) {
        | Some(next) => next
        | None => prev
        }
        update(m)
        m
      })
    }
    let selectors = Movement.movements->Belt.Array.mapWithIndex((i, m) => {
      let ms = Movement.toString(m)
      <option key={Belt.Int.toString(i)} value={ms}>
        {React.string(StringUtils.capitalize(ms))}
      </option>
    })
    <select onChange> {React.array(selectors)} </select>
  }
}

module InputNumberComponent = {
  @react.component
  let make = (~update: int => unit) => {
    let (_, setValue) = React.useState(_ => 0)
    let onChange = event => {
      ReactEvent.Form.preventDefault(event)
      let value = ReactEvent.Form.target(event)["value"]
      setValue(prev => {
        let m = switch Belt.Int.fromString(value) {
        | Some(next) => next
        | None => prev
        }
        update(m)
        m
      })
    }
    <input pattern="[0-9]+" type_="number" onChange />
  }
}

module WorkoutComponent = {
  @react.component
  let make = (~plan: Workout.plan) => {
    <div>
      <ul>
        {React.array(
          Belt.Array.mapWithIndex(plan.exercise, (i, e) =>
            <li key={Belt.Int.toString(i)}> {React.string(Workout.exerciseToString(e))} </li>
          ),
        )}
      </ul>
    </div>
  }
}

type action = AddExercise(Movement.movement, Workout.setScheme)
let reducer = (state: Workout.plan, a: action) => {
  open Workout
  switch a {
  | AddExercise(m, s) => {exercise: [{movement: m, sets: s}]->Js.Array.concat(state.exercise)}
  }
}

@react.component
let make = () => {
  let (plan, dispatch) = React.useReducer(reducer, {exercise: []})
  <div>
    <h1> {React.string("Workout")} </h1>
    <WorkoutComponent plan />
    <MovementSelector update={m => Js.log(m)} />
    <span>
      {React.string("Sets: ")}
      <InputNumberComponent update={v => Js.log(v)} />
    </span>
    <span>
      {React.string("Reps: ")}
      <InputNumberComponent update={v => Js.log(v)} />
    </span>
    <span>
      {React.string("Weight: ")}
      <InputNumberComponent update={v => Js.log(v)} />
    </span>
    <button
      onClick={_ =>
        dispatch(
          AddExercise(
            Movement.Squat,
            Workout.SetsAcross(4, {reps: 4, weight: Workout.Weight(100, Workout.Lb)}),
          ),
        )}>
      {React.string("Add")}
    </button>
  </div>
}
