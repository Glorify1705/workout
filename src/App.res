open Model

module MovementSelector = {
  @react.component
  let make = (~value: Movement.movement, ~update: Movement.movement => unit) => {
    let onChange = (event, _) => {
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
      <Mui.MenuItem key={ms} value={ms->Mui.MenuItem.Value.string}>
        {React.string(Utils.String.capitalize(ms))}
      </Mui.MenuItem>
    })
    <Mui.Select value={value->Movement.toString->Mui.Select.Value.string} onChange={onChange}>
      {React.array(selectors)}
    </Mui.Select>
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
    <Mui.TextField
      variant=#outlined value={value->Belt.Int.toString->Mui.TextField.Value.string} onChange
    />
  }
}

module WeightSelector = {
  @react.component
  let make = (~weight: WeightScheme.scheme, ~update: WeightScheme.scheme => unit) => {
    let (state, setState) = React.useState(() => weight)
    let schemeType = WeightScheme.schemeToType(state)
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
      <Mui.Select
        value={schemeType->WeightScheme.schemeTypeToString->Mui.Select.Value.string}
        onChange={(e, _) => {
          ReactEvent.Form.preventDefault(e)
          let t = ReactEvent.Form.target(e)["value"]
          setState(s => WeightScheme.applyType(t, s))
        }}>
        {React.array(
          WeightScheme.schemeTypes->Belt.Array.map(t => {
            let v = WeightScheme.schemeTypeToString(t)
            <Mui.MenuItem key={v} value={v->Mui.MenuItem.Value.string}>
              {React.string(v)}
            </Mui.MenuItem>
          }),
        )}
      </Mui.Select>
    </span>
  }
}

module ExerciseAdder = {
  @react.component
  let make = (~exercise: Workout.exercise, ~update: Workout.exercise => unit) => {
    let (state, setState) = React.useState(() => exercise)
    let {movement, sets, reps, weight} = state
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

module ExerciseEditor = {
  @react.component
  let make = (~exercise: Workout.exercise, ~update: Workout.exercise => unit) => {
    let (state, setState) = React.useState(() => exercise)
    let {movement, sets, reps, weight} = state
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
      <Mui.TableCell className="movement-display">
        {if sameMovement {
          React.string("")
        } else {
          {React.string(Utils.String.capitalize(Movement.toString(movement)))}
        }}
      </Mui.TableCell>
      <Mui.TableCell> {React.string(Belt.Int.toString(sets))} </Mui.TableCell>
      <Mui.TableCell> {React.string(Belt.Int.toString(reps))} </Mui.TableCell>
      <Mui.TableCell> {React.string(WeightScheme.toString(weight))} </Mui.TableCell>
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
          <Mui.Button
            variant=#outlined
            onClick={e => {
              ReactEvent.Mouse.preventDefault(e)
              setState(s => {...s, editing: true})
            }}>
            {React.string("Edit Notes ✎")}
          </Mui.Button>
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
          <Mui.Button
            variant=#outlined
            onClick={e => {
              ReactEvent.Mouse.preventDefault(e)
              setState(s => {...s, editing: false})
            }}>
            {React.string("✓")}
          </Mui.Button>
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

    let tableHeader =
      <Mui.TableHead>
        <Mui.TableRow>
          <Mui.TableCell> {React.string("Exercise")} </Mui.TableCell>
          <Mui.TableCell> {React.string("Sets")} </Mui.TableCell>
          <Mui.TableCell> {React.string("Reps")} </Mui.TableCell>
          <Mui.TableCell> {React.string("Loading")} </Mui.TableCell>
          <Mui.TableCell> {React.string("Edit")} </Mui.TableCell>
        </Mui.TableRow>
      </Mui.TableHead>

    let exerciseRow = (i, e) => {
      let editableExercise =
        <ExerciseEditor
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
      let editButton =
        <Mui.Button
          variant=#contained
          onClick={e => {
            ReactEvent.Mouse.preventDefault(e)
            setState(state => {...state, editing: IndexSet.add(state.editing, i)})
          }}>
          {React.string("✎")}
        </Mui.Button>
      let duplicateButton =
        <Mui.Button
          variant=#contained
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
        </Mui.Button>
      let deleteButton =
        <Mui.Button
          variant=#contained
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
        </Mui.Button>
      let controls =
        <Mui.TableCell>
          {editButton}
          {deleteButton}
          {duplicateButton}
        </Mui.TableCell>
      let exerciseDisplay =
        <>
          <ExerciseDisplay
            exercise={e}
            previous={if i > 0 {
              Some(state.workout.exercises[i - 1])
            } else {
              None
            }}
          />
          {controls}
        </>
      <tr
        key={Belt.Int.toString(i)}
        onDoubleClick={_ => setState(s => {...s, editing: IndexSet.add(s.editing, i)})}>
        {if IndexSet.has(state.editing, i) {
          editableExercise
        } else {
          exerciseDisplay
        }}
      </tr>
    }

    let tableBody =
      <Mui.TableBody>
        {React.array(workout.exercises->Belt.Array.mapWithIndex(exerciseRow))}
      </Mui.TableBody>

    let displayTop =
      <div>
        <DateComponent
          date={state.workout.date}
          update={date => setState(s => {...s, workout: {...s.workout, date}})}
        />
        <Mui.Button
          variant=#contained
          onClick={_ => Utils.Clipboard.copy(Workout.workoutToString(state.workout))}>
          {React.string("Copy to clipboard")}
        </Mui.Button>
        <Mui.Button variant=#contained onClick={_ => duplicate(state.workout)}>
          {React.string("Duplicate")}
        </Mui.Button>
        <Mui.Button onClick={_ => delete(state.workout)}> {React.string("✖")} </Mui.Button>
      </div>

    let exerciseAdder =
      <ExerciseAdder
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

    <div className="workout-display">
      {displayTop}
      <Mui.TableContainer>
        <Mui.Table>
          {tableHeader}
          {tableBody}
        </Mui.Table>
      </Mui.TableContainer>
      <NotesComponent
        initialNotes={workout.notes}
        update={notes => setState(state => {...state, workout: {...state.workout, notes}})}
      />
      <div className="exercise-editor"> {exerciseAdder} </div>
    </div>
  }
}

let testState: array<Workout.workout> = {
  [
    "2022/11/24",
    "2022/11/25",
    "2022/11/26",
    "2022/11/27",
    "2022/11/28",
    "2022/11/29",
    "2022/11/30",
    "2022/11/01",
    "2022/12/02",
    "2022/12/03",
    "2022/12/04",
    "2022/12/05",
    "2022/12/06",
    "2022/12/07",
    "2022/12/08",
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
    <>
      <Mui.CssBaseline />
      <Mui.Container component={Mui.Container.Component.string("main")}>
        <Mui.AppBar position=#sticky elevation={Mui.Number.int(0)}>
          <Mui.Toolbar>
            <Mui.Typography variant=#h6 color=#inherit>
              {React.string("Workout Tracker")}
            </Mui.Typography>
            <Mui.Divider />
            <Mui.List>
              <Mui.Button color=#inherit onClick={_ => addWorkout()}>
                {React.string("Add Workout")}
              </Mui.Button>
              <Mui.Button
                color=#inherit
                onClick={_ => {
                  let maybeWorkout = Workout.getWorkout(state.plan, Utils.Date.now())
                  if Belt.Option.isSome(maybeWorkout) {
                    let workout = Belt.Option.getUnsafe(maybeWorkout)
                    Utils.Clipboard.copy(workout->Workout.workoutToString)
                  }
                }}>
                {React.string("Copy today's workout")}
              </Mui.Button>
              <Mui.Button
                color=#inherit
                onClick={_ =>
                  Utils.Spreadsheets.download(
                    Workout.planToSpreadsheet(state.plan),
                    "theplan.xlsx",
                  )}>
                {React.string("Download as Spreadsheet")}
              </Mui.Button>
            </Mui.List>
          </Mui.Toolbar>
        </Mui.AppBar>
        <Mui.Paper variant=#outlined elevation={Mui.Number.int(3)}>
          <Mui.Typography variant=#h4 align=#center>
            <Mui.IconButton
              onClick={_ => setState(s => {...s, week: Utils.Date.daysBefore(s.week, 7.0)})}>
              <Icons.ArrowCircleLeft />
            </Mui.IconButton>
            {React.string(
              "Week " ++
              Utils.Date.toIso8601(state.week) ++
              " to " ++
              Utils.Date.toIso8601(Utils.Date.daysAfter(state.week, 7.0)),
            )}
            <Mui.IconButton
              onClick={_ => setState(s => {...s, week: Utils.Date.daysAfter(s.week, 7.0)})}>
              <Icons.ArrowCircleRight />
            </Mui.IconButton>
          </Mui.Typography>
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
        </Mui.Paper>
      </Mui.Container>
    </>
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
