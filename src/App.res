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
      <option key={ms} value={ms}> {React.string(Utils.String.capitalize(ms))} </option>
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
  type mode = Editor | Adder

  @react.component
  let make = (~exercise: Workout.exercise, ~update: Workout.exercise => unit, ~mode: mode) => {
    let (state, setState) = React.useState(() => exercise)
    let {movement, sets, reps, weight} = state
    switch mode {
    | Editor =>
      <>
        <td>
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
        </td>
        <td>
          <InputNumberComponent value=sets update={sets => setState(state => {...state, sets})} />
        </td>
        <td>
          <InputNumberComponent value=reps update={reps => setState(state => {...state, reps})} />
        </td>
        {if Movement.isWeighted(movement) {
          <td>
            <WeightSelector weight update={weight => setState(state => {...state, weight})} />
          </td>
        } else {
          <td />
        }}
        <td className="exercise-controls">
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
        </td>
      </>

    | Adder =>
      <span className="exercise-adder">
        <span>
          <b> {React.string("Add exercise: ")} </b>
        </span>
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
}

module ExerciseDisplay = {
  @react.component
  let make = (~exercise: Workout.exercise, ~previous: option<Workout.exercise>) => {
    let {movement, sets, reps, weight} = exercise
    let sameMovement = Belt.Option.mapWithDefault(previous, false, p =>
      p.movement == exercise.movement
    )
    <>
      <td className="movement-display">
        {if sameMovement {
          React.string("")
        } else {
          {React.string(Utils.String.capitalize(Movement.toString(movement)))}
        }}
      </td>
      <td> {React.string(Belt.Int.toString(sets))} </td>
      <td> {React.string(Belt.Int.toString(reps))} </td>
      <td> {React.string(WeightScheme.toString(weight))} </td>
    </>
  }
}

module NotesComponent = {
  type state = {editing: bool, notes: string}
  @react.component
  let make = (~initialNotes: string, ~update: string => unit) => {
    let (state, setState) = React.useState(_ => {editing: false, notes: initialNotes})
    let {editing, notes} = state
    let converter = Utils.Markdown.makeConverter()
    React.useEffect1(() => {
      update(state.notes)
      None
    }, [state])

    <div className="workout-notes">
      {if !editing {
        <>
          <div
            className="notes-display"
            dangerouslySetInnerHTML={"__html": Utils.Markdown.convertHtml(converter, notes)}
          />
          <button
            className="edit-notes-button"
            onClick={e => {
              ReactEvent.Mouse.preventDefault(e)
              setState(s => {...s, editing: true})
            }}>
            {React.string("Edit Notes ✎")}
          </button>
        </>
      } else {
        <>
          <div className="notes-editor">
            <textarea
              value={notes}
              onChange={e => {
                let notes = ReactEvent.Form.target(e)["value"]
                setState(s => {...s, notes})
              }}
            />
          </div>
          <button
            className="edit-button"
            onClick={e => {
              ReactEvent.Mouse.preventDefault(e)
              setState(s => {...s, editing: false})
            }}>
            {React.string("✓")}
          </button>
        </>
      }}
    </div>
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
      value={Utils.Date.toIso8601(state)}
      onChange={event => {
        ReactEvent.Form.preventDefault(event)
        let d = Utils.Date.fromIso8601(ReactEvent.Form.target(event)["value"])
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
  let make = (
    ~workout: Workout.workout,
    ~update: Workout.workout => unit,
    ~delete: Workout.workout => unit,
    ~duplicate: Workout.workout => unit,
  ) => {
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
      <div className="workout-display-top">
        <DateComponent
          date={state.workout.date}
          update={date => setState(s => {...s, workout: {...s.workout, date}})}
        />
        <button onClick={_ => Utils.Clipboard.copy(Workout.workoutToString(state.workout))}>
          {React.string("Copy to clipboard")}
        </button>
        <button onClick={_ => duplicate(state.workout)}> {React.string("Duplicate")} </button>
        <button onClick={_ => delete(state.workout)}> {React.string("✖")} </button>
      </div>
      <table className="exercise-display">
        <thead>
          <tr>
            <th> {React.string("Exercise")} </th>
            <th> {React.string("Sets")} </th>
            <th> {React.string("Reps")} </th>
            <th> {React.string("Loading")} </th>
            <th className="exercise-control-header"> {React.string("Edit")} </th>
          </tr>
        </thead>
        <tbody>
          {React.array(
            Belt.Array.mapWithIndex(workout.exercises, (i, e) => {
              <tr
                key={Belt.Int.toString(i)}
                onDoubleClick={_ => setState(s => {...s, editing: IndexSet.add(s.editing, i)})}>
                {if IndexSet.has(state.editing, i) {
                  <ExerciseEditor
                    mode=ExerciseEditor.Editor
                    exercise={e}
                    update={exercise => {
                      setState(state => {
                        {
                          editing: IndexSet.remove(state.editing, i),
                          workout: {
                            ...state.workout,
                            exercises: Utils.Array.setIndex(state.workout.exercises, i, exercise),
                          },
                        }
                      })
                    }}
                  />
                } else {
                  <>
                    <ExerciseDisplay
                      exercise={e}
                      previous={if i > 0 {
                        Some(state.workout.exercises[i - 1])
                      } else {
                        None
                      }}
                    />
                    <td className="exercise-controls">
                      <button
                        className="edit-button"
                        onClick={e => {
                          ReactEvent.Mouse.preventDefault(e)
                          setState(state => {...state, editing: IndexSet.add(state.editing, i)})
                        }}>
                        {React.string("✎")}
                      </button>
                      <button
                        className="delete-button"
                        onClick={event => {
                          ReactEvent.Mouse.preventDefault(event)
                          setState(state => {
                            let exercises = workout.exercises
                            {
                              editing: IndexSet.remove(state.editing, i),
                              workout: {
                                ...workout,
                                exercises: Utils.Array.removeIndex(exercises, i),
                              },
                            }
                          })
                        }}>
                        {React.string("✖")}
                      </button>
                      <button
                        className="duplicate-button"
                        onClick={event => {
                          ReactEvent.Mouse.preventDefault(event)
                          setState(state => {
                            let exercises = workout.exercises
                            {
                              editing: IndexSet.remove(state.editing, i),
                              workout: {
                                ...workout,
                                exercises: Utils.Array.insertAt(exercises, i, e),
                              },
                            }
                          })
                        }}>
                        {React.string("Duplicate")}
                      </button>
                    </td>
                  </>
                }}
              </tr>
            }),
          )}
        </tbody>
      </table>
      <NotesComponent
        initialNotes={workout.notes}
        update={notes => setState(state => {...state, workout: {...state.workout, notes}})}
      />
      <div className="exercise-editor">
        <ExerciseEditor
          mode=ExerciseEditor.Adder
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

let testState: array<Workout.workout> = {
  [
    "2022/11/10",
    "2022/11/11",
    "2022/11/12",
    "2022/11/13",
    "2022/11/14",
    "2022/11/15",
    "2022/11/16",
    "2022/11/17",
    "2022/11/18",
    "2022/11/19",
    "2022/11/20",
    "2022/11/21",
    "2022/11/22",
    "2022/11/23",
    "2022/11/24",
  ]->Js.Array2.map(d => {
    let workout: Workout.workout = {
      date: Utils.Date.fromIso8601(d),
      exercises: [
        {
          movement: Movement.Squat,
          sets: 3,
          reps: 8,
          weight: WeightScheme.Weight(100, WeightScheme.Kg),
        },
        {
          movement: Movement.Bench,
          sets: 5,
          reps: 5,
          weight: WeightScheme.Weight(80, WeightScheme.Kg),
        },
        {
          movement: Movement.BarbellRows,
          sets: 4,
          reps: 8,
          weight: WeightScheme.Weight(80, WeightScheme.Kg),
        },
      ],
      notes: "**Notes**: Workout on the " ++ d,
    }
    workout
  })
}

module WorkoutsComponent = {
  type t = {
    plan: Workout.plan,
    week: Js.Date.t,
  }
  @react.component
  let make = () => {
    let (state, setState) = React.useState(() => {
      plan: {workouts: testState},
      week: Utils.Date.startOfWeek(Utils.Date.now()),
    })
    let duplicateWorkout = workout => {
      setState(s => {
        {
          ...s,
          plan: Workout.addWorkout(
            s.plan,
            {
              ...workout,
              date: Utils.Date.now(),
            },
          ),
        }
      })
    }
    let addWorkout = () => {
      setState(s => {
        let plan = if Belt.Option.isNone(Workout.getWorkout(s.plan, Utils.Date.now())) {
          Workout.addWorkout(s.plan, Workout.emptyWorkout())
        } else {
          s.plan
        }
        {...s, plan}
      })
    }
    <div>
      <h1> {React.string("Workout Tracker")} </h1>
      <div className="week-title">
        <button
          className="move-button"
          onClick={_ => setState(s => {...s, week: Utils.Date.daysBefore(s.week, 7.0)})}>
          {React.string("←")}
        </button>
        <span className="week-display">
          <b>
            {React.string(
              "Week " ++
              Utils.Date.toIso8601(state.week) ++
              " to " ++
              Utils.Date.toIso8601(Utils.Date.daysAfter(state.week, 7.0)),
            )}
          </b>
        </span>
        <button
          className="move-button"
          onClick={_ => setState(s => {...s, week: Utils.Date.daysAfter(s.week, 7.0)})}>
          {React.string("→")}
        </button>
      </div>
      <div className="workout-controls">
        <button className="input" onClick={_ => addWorkout()}>
          {React.string("Add Workout")}
        </button>
        <button
          className="input"
          onClick={_ => {
            let maybeWorkout = Workout.getWorkout(state.plan, Utils.Date.now())
            if Belt.Option.isSome(maybeWorkout) {
              let workout = Belt.Option.getUnsafe(maybeWorkout)
              Utils.Clipboard.copy(workout->Workout.workoutToString)
            }
          }}>
          {React.string("Copy today's workout")}
        </button>
        <button
          className="input"
          onClick={_ =>
            Utils.Spreadsheets.download(Workout.planToSpreadsheet(state.plan), "theplan.xlsx")}>
          {React.string("Download as Spreadsheet")}
        </button>
      </div>
      {React.array(
        state.plan.workouts
        ->Belt.Array.keep(w => {
          state.week <= w.date && w.date < Utils.Date.daysAfter(state.week, 7.0)
        })
        ->Belt.Array.map(w => {
          <WorkoutComponent
            key={Utils.Date.toIso8601(w.date)}
            workout={w}
            update={workout => {
              setState(s => {
                {...s, plan: Workout.editWorkout(s.plan, w.date, workout)}
              })
            }}
            delete={workout => {
              setState(s => {...s, plan: Workout.removeWorkout(s.plan, workout.date)})
            }}
            duplicate={duplicateWorkout}
          />
        }),
      )}
    </div>
  }
}

module App = {
  @react.component
  let make = () => {
    let url = RescriptReactRouter.useUrl()
    switch url.path {
    | _ => <WorkoutsComponent />
    }
  }
}
