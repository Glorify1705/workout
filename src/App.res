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

  let movements = [Squat, Bench, Deadlift, Pullups, BarbellRows]

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

module WeightScheme = {
  type measure = Kg | Lb

  type scheme = Weight(int, measure) | Amrap | Rpe(int) | Bodyweight

  let toString = (w: scheme) => {
    switch w {
    | Weight(weight, metric) =>
      Belt.Int.toString(weight) ++
      switch metric {
      | Kg => "kg."
      | Lb => "lb."
      }
    | Amrap => "AMRAP"
    | Rpe(rpe) => "RPE " ++ Belt.Int.toString(rpe)
    | Bodyweight => ""
    }
  }
}

module Workout = {
  type exercise = {
    movement: Movement.movement,
    reps: int,
    sets: int,
    weight: WeightScheme.scheme,
  }

  type plan = {exercise: array<exercise>}

  let setsToString = (~sets: int, ~reps: int) => {
    let intToStr = Belt.Int.toString
    intToStr(sets) ++ "x" ++ intToStr(reps)
  }

  let loadToString = (~sets: int, ~reps: int, ~weight: WeightScheme.scheme) => {
    setsToString(~sets, ~reps) ++ " @ " ++ WeightScheme.toString(weight)
  }

  let exerciseToString = (e: exercise) => {
    let {movement, reps, sets, weight} = e
    StringUtils.capitalize(Movement.toString(movement)) ++
    " - " ++
    loadToString(~sets, ~reps, ~weight)
  }

  let planToString = (p: plan) => Belt.Array.joinWith(p.exercise, "\n", exerciseToString)
}

module MovementSelector = {
  @react.component
  let make = (~value: Movement.movement, ~update: Movement.movement => unit) => {
    let onChange = event => {
      ReactEvent.Form.preventDefault(event)
      let value = ReactEvent.Form.target(event)["value"]
      update(
        switch Movement.fromString(value) {
        | Some(next) => next
        | None => Movement.Squat
        },
      )
    }
    let selectors = Movement.movements->Belt.Array.map(m => {
      let ms = Movement.toString(m)
      <option key={ms} value={ms}> {React.string(StringUtils.capitalize(ms))} </option>
    })
    <select className="input" value={Movement.toString(value)} onChange>
      {React.array(selectors)}
    </select>
  }
}

module InputNumberComponent = {
  @react.component
  let make = (~value: int, ~update: int => unit) => {
    let onChange = event => {
      ReactEvent.Form.preventDefault(event)
      let inputValue = ReactEvent.Form.target(event)["value"]
      update(
        switch Belt.Int.fromString(inputValue) {
        | Some(next) => next
        | None => value
        },
      )
    }
    <input pattern="[0-9]+" value={Belt.Int.toString(value)} type_="number" onChange />
  }
}

module WorkoutComponent = {
  @react.component
  let make = (~plan: Workout.plan, ~delete: int => unit) => {
    open Workout
    <div className="exercise-display">
      {React.array(
        Belt.Array.mapWithIndex(plan.exercise, (i, e) => {
          let {movement, sets, reps, weight} = e
          <div key={Belt.Int.toString(i)}>
            <span className="movement-display">
              {React.string(StringUtils.capitalize(Movement.toString(movement)))}
            </span>
            <span className="scheme-display">
              {React.string(Workout.loadToString(~sets, ~reps, ~weight))}
            </span>
            <button
              onClick={e => {
                ReactEvent.Mouse.preventDefault(e)
                delete(i)
              }}>
              {React.string("X")}
            </button>
          </div>
        }),
      )}
    </div>
  }
}

module WeightSelector = {
  type weightUnit = [#Kg | #Lb | #Rpe]

  let weightUnitToString = w =>
    switch w {
    | #Kg => "Kg"
    | #Lb => "Lb"
    | #Rpe => "RPE"
    }

  @react.component
  let make = (~update: WeightScheme.scheme => unit) => {
    let (state, setState) = React.useState(() => (#Kg, 0))
    let (c, v) = state
    React.useEffect2(() => {
      update(
        switch c {
        | #Kg => WeightScheme.Weight(v, WeightScheme.Kg)
        | #Lb => WeightScheme.Weight(v, WeightScheme.Lb)
        | #Rpe => WeightScheme.Rpe(v)
        },
      )
      None
    }, state)
    let onChange = event => {
      ReactEvent.Form.preventDefault(event)
      let c = ReactEvent.Form.target(event)["value"]
      setState(((_c, v)) => (c, v))
    }
    <span className="input">
      <InputNumberComponent value={v} update={v => setState(((c, _v)) => (c, v))} />
      <select value={weightUnitToString(c)} onChange>
        {React.array(
          [#Kg, #Lb, #Rpe]->Belt.Array.map(v => {
            let val = weightUnitToString(v)
            <option key={val} value={val}> {React.string(val)} </option>
          }),
        )}
      </select>
    </span>
  }
}

module ExerciseAdder = {
  @react.component
  let make = (~update: Workout.exercise => unit) => {
    let (state, setState) = React.useState((): Workout.exercise => {
      movement: Movement.Squat,
      sets: 0,
      reps: 0,
      weight: WeightScheme.Bodyweight,
    })
    let {movement, sets, reps, weight} = state
    <div id="exercise-adder">
      <MovementSelector
        value={movement} update={movement => setState(state => {...state, movement})}
      />
      <span className="input">
        {React.string("Sets: ")}
        <InputNumberComponent value=sets update={sets => setState(state => {...state, sets})} />
      </span>
      <span className="input">
        {React.string("Reps: ")}
        <InputNumberComponent value=reps update={reps => setState(state => {...state, reps})} />
      </span>
      <WeightSelector update={weight => setState(state => {...state, weight})} />
      <button
        className="input"
        onClick={_ => {
          update({
            movement,
            sets,
            reps,
            weight,
          })
        }}>
        {React.string("Add")}
      </button>
    </div>
  }
}

type action = AddExercise(Workout.exercise) | DeleteExercise(int)
let reducer = (state: Workout.plan, a: action) => {
  open Workout
  switch a {
  | AddExercise(e) => {
      exercise: [e]->Js.Array.concat(state.exercise),
    }
  | DeleteExercise(i) => {
      let length = Js.Array2.length(state.exercise)
      let prefix = state.exercise->Js.Array2.slice(~start=0, ~end_=i)
      let suffix = state.exercise->Js.Array2.slice(~start=i + 1, ~end_=length)
      {exercise: Js.Array2.concat(prefix, suffix)}
    }
  }
}

module Clipboard = {
  @val external write: string => Js.Promise.t<'a> = "navigator.clipboard.writeText"
}

@react.component
let make = () => {
  let (plan, dispatch) = React.useReducer(reducer, {exercise: []})
  let copyToClipboard = plan => {
    Clipboard.write(Workout.planToString(plan))
    ->Promise.catch(err => {
      Js.log(err)
      Js.Promise.resolve()
    })
    ->ignore
  }
  <div>
    <h1> {React.string("Workout tracker")} </h1>
    <WorkoutComponent plan delete={i => dispatch(DeleteExercise(i))} />
    <ExerciseAdder update={exercise => dispatch(AddExercise(exercise))} />
    <div className="copiers">
      <button onClick={_ => copyToClipboard(plan)}> {React.string("Copy to clipboard")} </button>
    </div>
  </div>
}
