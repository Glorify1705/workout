open Utils

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
    Belt.Int.toString(sets) ++ "x" ++ Belt.Int.toString(reps)
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
    DateUtils.toIso8861(w.date) ++
    "\n----------------\n" ++
    Belt.Array.joinWith(w.exercises, "\n", exerciseToString)
  }

  let planToString = (p: plan) => {
    Belt.Array.joinWith(p.workouts, "\n", workoutToString)
  }
}
