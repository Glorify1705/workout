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

module ArrayUtils = {
  module Array = Js.Array2
  let removeIndex = (arr: array<'t>, i: int) => {
    let length = Array.length(arr)
    let prefix = arr->Array.slice(~start=0, ~end_=i)
    let suffix = arr->Array.slice(~start=i + 1, ~end_=length)
    Array.concat(prefix, suffix)
  }

  let setIndex = (arr: array<'t>, i: int, v: 't) => {
    let length = Array.length(arr)
    let prefix = arr->Array.slice(~start=0, ~end_=i)
    let suffix = arr->Array.slice(~start=i + 1, ~end_=length)
    prefix->Array.concat([v])->Array.concat(suffix)
  }
}

module DateUtils = {
  let toIso8861 = (d: Js.Date.t): string => {
    let day = Js.Date.getDate(d)
    let month = Js.Date.getMonth(d)
    let year = Js.Date.getFullYear(d)
    [year, month +. 1.0, day]->Js.Array2.map(Belt.Float.toInt)->Js.Array2.joinWith("-")
  }

  let fromIso8861 = (s: string): Js.Date.t => {
    Js.Date.fromString(s)
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

  type workout = {
    date: Js.Date.t,
    exercises: array<exercise>,
  }

  type plan = {workouts: array<workout>}

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

  let workoutToString = (w: workout) => {
    "🗓 " ++
    Js.Date.toUTCString(w.date) ++
    "\n----------------\n" ++
    Belt.Array.joinWith(w.exercises, "\n", exerciseToString)
  }

  let planToString = (p: plan) => {
    Belt.Array.joinWith(p.workouts, "\n", workoutToString)
  }
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

module WeightSelector = {
  type weightUnit = [#Kg | #Lb | #Rpe | #Amrap | #Bodyweight]

  let weightUnitToString = w =>
    switch w {
    | #Kg => "Kg"
    | #Lb => "Lb"
    | #Rpe => "RPE"
    | #Amrap => "AMRAP"
    | #Bodyweight => "Bodyweight"
    }

  let weightSchemeUnitToUnit = w =>
    switch w {
    | WeightScheme.Weight(_, WeightScheme.Kg) => #Kg
    | WeightScheme.Weight(_, WeightScheme.Lb) => #Lb
    | WeightScheme.Amrap => #Amrap
    | WeightScheme.Rpe(_) => #Rpe
    | WeightScheme.Bodyweight => #Bodyweight
    }

  let weightValue = w =>
    switch w {
    | WeightScheme.Weight(v, _) => v
    | _ => 0
    }

  @react.component
  let make = (~weight: WeightScheme.scheme, ~update: WeightScheme.scheme => unit) => {
    let (state, setState) = React.useState(() => (
      weightSchemeUnitToUnit(weight),
      weightValue(weight),
    ))
    let (c, v) = state
    React.useEffect2(() => {
      update(
        switch c {
        | #Kg => WeightScheme.Weight(v, WeightScheme.Kg)
        | #Lb => WeightScheme.Weight(v, WeightScheme.Lb)
        | #Rpe => WeightScheme.Rpe(v)
        | #Amrap => WeightScheme.Amrap
        | #Bodyweight => WeightScheme.Bodyweight
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

module ExerciseEditor = {
  @react.component
  let make = (~exercise: Workout.exercise, ~update: Workout.exercise => unit) => {
    let (state, setState) = React.useState(() => exercise)
    let {movement, sets, reps, weight} = state
    <span className="exercise-adder">
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
      <WeightSelector weight update={weight => setState(state => {...state, weight})} />
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
        {React.string("✓")}
      </button>
    </span>
  }
}

module WorkoutComponent = {
  module IndexSet = Belt.Set.Int

  type state = {
    workout: Workout.workout,
    editing: IndexSet.t,
  }

  @react.component
  let make = (~workout: Workout.workout, ~update: Workout.workout => unit) => {
    let (state, setState) = React.useState(() => {workout, editing: IndexSet.empty})

    let blankExercise: Workout.exercise = {
      movement: Movement.Squat,
      sets: 0,
      reps: 0,
      weight: WeightScheme.Weight(0, WeightScheme.Kg),
    }

    React.useEffect1(() => {
      update(state.workout)
      None
    }, [state.workout])

    <div className="workout-display">
      <input
        type_="date"
        value={DateUtils.toIso8861(workout.date)}
        onChange={event => {
          ReactEvent.Form.preventDefault(event)
          let date = DateUtils.fromIso8861(ReactEvent.Form.target(event)["value"])
          setState(s => {...s, workout: {...s.workout, date}})
        }}
      />
      <div className="exercise-display">
        {React.array(
          Belt.Array.mapWithIndex(workout.exercises, (i, e) => {
            let {movement, sets, reps, weight} = e
            <div key={Belt.Int.toString(i)}>
              {if IndexSet.has(state.editing, i) {
                <ExerciseEditor
                  exercise={e}
                  update={exercise => {
                    setState(state => {
                      let exercises = workout.exercises
                      {
                        editing: IndexSet.remove(state.editing, i),
                        workout: {
                          ...state.workout,
                          exercises: ArrayUtils.setIndex(exercises, i, exercise),
                        },
                      }
                    })
                  }}
                />
              } else {
                <span>
                  <span className="movement-display">
                    {React.string(StringUtils.capitalize(Movement.toString(movement)))}
                  </span>
                  <span className="scheme-display">
                    {React.string(Workout.loadToString(~sets, ~reps, ~weight))}
                  </span>
                  <button
                    className="edit-button"
                    onClick={e => {
                      ReactEvent.Mouse.preventDefault(e)
                      setState(state => {...state, editing: IndexSet.add(state.editing, i)})
                    }}>
                    {React.string("✎")}
                  </button>
                </span>
              }}
              <button
                className="delete-button"
                onClick={e => {
                  ReactEvent.Mouse.preventDefault(e)
                  setState(state => {
                    let exercises = workout.exercises
                    {
                      editing: IndexSet.remove(state.editing, i),
                      workout: {...workout, exercises: ArrayUtils.removeIndex(exercises, i)},
                    }
                  })
                }}>
                {React.string("✖")}
              </button>
            </div>
          }),
        )}
      </div>
      <div id="exercise-adder">
        <ExerciseEditor
          exercise={blankExercise}
          update={exercise =>
            setState(state => {
              {
                ...state,
                workout: {
                  ...state.workout,
                  exercises: state.workout.exercises->Js.Array2.concat([exercise]),
                },
              }
            })}
        />
      </div>
    </div>
  }
}

module Clipboard = {
  @val external write: string => Js.Promise.t<'a> = "navigator.clipboard.writeText"
}

module App = {
  @react.component
  let make = (~workout) => {
    let blankWorkout: Workout.workout = {date: Js.Date.make(), exercises: []}
    let blankState: Workout.plan = {workouts: [blankWorkout]}
    let (state, setState) = React.useState(() => blankState)
    let copyToClipboard = plan => {
      Clipboard.write(Workout.workoutToString(plan))
      ->Promise.catch(err => {
        Js.log(err)
        Js.Promise.resolve()
      })
      ->ignore
    }
    <div>
      <h1> {React.string("Workout Tracker")} </h1>
      <div className="workout-controls">
        <button
          className="input"
          onClick={_ => {
            setState(state => {workouts: state.workouts->Js.Array2.concat([blankWorkout])})
          }}>
          {React.string("Add Workout")}
        </button>
      </div>
      {React.array(
        Belt.Array.mapWithIndex(state.workouts, (i, w) => {
          <div key={Belt.Int.toString(i)}>
            <WorkoutComponent
              workout={w}
              update={workout =>
                setState(state => {workouts: ArrayUtils.setIndex(state.workouts, i, workout)})}
            />
            <div id="copiers">
              <button onClick={_ => copyToClipboard(workout)}>
                {React.string("Copy to clipboard")}
              </button>
            </div>
          </div>
        }),
      )}
    </div>
  }
}
