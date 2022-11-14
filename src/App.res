open Utils
open Model

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
  @react.component
  let make = (~weight: WeightScheme.scheme, ~update: WeightScheme.scheme => unit) => {
    let (state, setState) = React.useState(() => weight)
    React.useEffect1(() => {
      update(state)
      None
    }, [state])
    <span className="weight-input">
      {switch state {
      | WeightScheme.Weight(v, WeightScheme.Kg) =>
        <InputNumberComponent
          value={v} update={nv => setState(s => WeightScheme.applyMeasure(nv, s))}
        />
      | WeightScheme.Weight(v, WeightScheme.Lb) =>
        <InputNumberComponent
          value={v} update={nv => setState(s => WeightScheme.applyMeasure(nv, s))}
        />
      | WeightScheme.Rpe(v) =>
        <InputNumberComponent
          value={v} update={nv => setState(s => WeightScheme.applyMeasure(nv, s))}
        />
      | _ => <span />
      }}
      <select
        value={(WeightScheme.schemeToType(state) :> string)}
        onChange={e => {
          ReactEvent.Form.preventDefault(e)
          let t = ReactEvent.Form.target(e)["value"]
          setState(s => WeightScheme.applyType(t, s))
        }}>
        {React.array(
          WeightScheme.schemeTypes->Belt.Array.map(t => {
            <option key={(t :> string)} value={(t :> string)}>
              {React.string((t :> string))}
            </option>
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
        value={movement}
        update={movement => {
          setState(state => {
            ...state,
            movement,
            weight: if Movement.isWeighted(movement) {
              weight
            } else {
              WeightScheme.Bodyweight
            },
          })
        }}
      />
      <span className="input">
        {React.string("Sets: ")}
        <InputNumberComponent value=sets update={sets => setState(state => {...state, sets})} />
      </span>
      <span className="input">
        {React.string("Reps: ")}
        <InputNumberComponent value=reps update={reps => setState(state => {...state, reps})} />
      </span>
      {if Movement.isWeighted(movement) {
        <WeightSelector weight update={weight => setState(state => {...state, weight})} />
      } else {
        <span />
      }}
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

module ExerciseDisplay = {
  @react.component
  let make = (~exercise: Workout.exercise, ~previous: option<Workout.exercise>) => {
    let {movement, sets, reps, weight} = exercise
    let sameMovement = Belt.Option.mapWithDefault(previous, false, p =>
      p.movement == exercise.movement
    )
    <span>
      <span className="movement-display">
        {if sameMovement {
          React.string("")
        } else {
          {React.string(StringUtils.capitalize(Movement.toString(movement)))}
        }}
      </span>
      <span className="scheme-display">
        {React.string(Workout.loadToString(~sets, ~reps, ~weight))}
      </span>
    </span>
  }
}

module NotesComponent = {
  type state = {editing: bool, notes: string}
  @react.component
  let make = (~initialNotes: string, ~update: string => unit) => {
    let (state, setState) = React.useState(_ => {editing: false, notes: initialNotes})
    let {editing, notes} = state
    React.useEffect1(() => {
      update(state.notes)
      None
    }, [state])

    {
      if !editing {
        <div>
          <pre> {React.string(notes)} </pre>
          <button
            className="edit-button"
            onClick={e => {
              ReactEvent.Mouse.preventDefault(e)
              setState(s => {...s, editing: true})
            }}>
            {React.string("✎")}
          </button>
        </div>
      } else {
        <div>
          <textarea
            value={notes}
            onChange={e => {
              let notes = ReactEvent.Form.target(e)["value"]
              setState(s => {...s, notes})
            }}
          />
          <button
            className="edit-button"
            onClick={e => {
              ReactEvent.Mouse.preventDefault(e)
              setState(s => {...s, editing: false})
            }}>
            {React.string("✓")}
          </button>
        </div>
      }
    }
  }
}

module DateComponent = {
  @react.component
  let make = (~date: Js.Date.t, ~update: Js.Date.t => unit) => {
    let (state, setState) = React.useState(_ => date)
    React.useEffect1(() => {
      update(state)
      None
    }, [state])
    <input
      type_="date"
      value={DateUtils.toIso8861(state)}
      onChange={event => {
        ReactEvent.Form.preventDefault(event)
        let d = DateUtils.fromIso8861(ReactEvent.Form.target(event)["value"])
        setState(_ => d)
      }}
    />
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
      <div id="workout-display-top">
        <DateComponent
          date={state.workout.date}
          update={date => setState(s => {...s, workout: {...s.workout, date}})}
        />
        <button onClick={_ => ClipboardUtils.copy(Workout.workoutToString(state.workout))}>
          {React.string("Copy to clipboard")}
        </button>
      </div>
      <div className="exercise-display">
        {React.array(
          Belt.Array.mapWithIndex(workout.exercises, (i, e) => {
            <div key={Belt.Int.toString(i)}>
              {if IndexSet.has(state.editing, i) {
                <ExerciseEditor
                  exercise={e}
                  update={exercise => {
                    setState(state => {
                      {
                        editing: IndexSet.remove(state.editing, i),
                        workout: {
                          ...state.workout,
                          exercises: ArrayUtils.setIndex(state.workout.exercises, i, exercise),
                        },
                      }
                    })
                  }}
                />
              } else {
                <span>
                  <ExerciseDisplay
                    exercise={e}
                    previous={if i > 0 {
                      Some(state.workout.exercises[i - 1])
                    } else {
                      None
                    }}
                  />
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
      <NotesComponent
        initialNotes={workout.notes}
        update={notes => setState(state => {...state, workout: {...state.workout, notes}})}
      />
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

let testState: Workout.workout = {
  date: DateUtils.now(),
  exercises: [
    {movement: Movement.Squat, sets: 5, reps: 5, weight: WeightScheme.Weight(100, WeightScheme.Kg)},
    {
      movement: Movement.Deadlift,
      sets: 5,
      reps: 5,
      weight: WeightScheme.Weight(140, WeightScheme.Kg),
    },
    {movement: Movement.Pullups, sets: 5, reps: 8, weight: WeightScheme.Bodyweight},
  ],
  notes: "Very challenging but fun workout!",
}

module App = {
  @react.component
  let make = () => {
    let blankState: Workout.plan = {workouts: [testState]}
    let (state, setState) = React.useState(() => blankState)
    <div>
      <h1> {React.string("Workout Tracker")} </h1>
      <div className="workout-controls">
        <button
          className="input"
          onClick={_ =>
            setState(s => {
              if Belt.Option.isNone(Workout.getWorkout(s, DateUtils.now())) {
                Workout.addWorkout(s, Workout.emptyWorkout())
              } else {
                s
              }
            })}>
          {React.string("Add Workout")}
        </button>
        <button
          className="input"
          onClick={_ => {
            let workout = Workout.getWorkout(state, DateUtils.now())
            if Belt.Option.isSome(workout) {
              ClipboardUtils.copy(Belt.Option.getUnsafe(workout)->Workout.workoutToString)
            }
          }}>
          {React.string("Copy current workout")}
        </button>
      </div>
      {React.array(
        Belt.Array.map(state.workouts, w => {
          <div key={DateUtils.toIso8861(w.date)}>
            <WorkoutComponent
              workout={w}
              update={workout => {
                setState(s => {
                  Workout.editWorkout(s, w.date, workout)
                })
              }}
            />
          </div>
        }),
      )}
    </div>
  }
}
