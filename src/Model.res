open Utils

module Movement = {
  type movement = Squat | Bench | Deadlift | Pullups | BarbellRows | OverheadPress

  let toString = (m: movement) =>
    switch m {
    | Squat => "squat"
    | Bench => "bench"
    | Deadlift => "deadlift"
    | Pullups => "pullups"
    | BarbellRows => "barbell rows"
    | OverheadPress => "overhead press"
    }

  let movements = [Squat, Bench, Deadlift, Pullups, BarbellRows, OverheadPress]

  let fromString = (s: string) => {
    switch s {
    | "squat" => Some(Squat)
    | "bench" => Some(Bench)
    | "deadlift" => Some(Deadlift)
    | "pullups" => Some(Pullups)
    | "barbell rows" => Some(BarbellRows)
    | "overhead press" => Some(OverheadPress)
    | _ => None
    }
  }

  let isWeighted = (m: movement) => {
    switch m {
    | Pullups => false
    | _ => true
    }
  }
}

module WeightScheme = {
  type measure = Kg | Lb

  type scheme = Weight(int, measure) | Amrap | Rpe(int) | Bodyweight

  type schemeType = [#Kg | #Lb | #Rpe | #Amrap | #Bodyweight]

  let schemeTypes: array<schemeType> = [#Kg, #Lb, #Rpe, #Amrap, #Bodyweight]

  let schemeTypeToString = t =>
    switch t {
    | #Kg => "Kg"
    | #Lb => "Lb"
    | #Rpe => "RPE"
    | #Amrap => "AMRAP"
    | #Bodyweight => "Bodyweight"
    }

  let schemeToType = (s: scheme): schemeType => {
    switch s {
    | Weight(_, Kg) => #Kg
    | Weight(_, Lb) => #Lb
    | Amrap => #Amrap
    | Rpe(_) => #Rpe
    | Bodyweight => #Bodyweight
    }
  }

  let applyType = (t: schemeType, s: scheme) => {
    switch (s, t) {
    | (Weight(v, Kg), #Kg) => Weight(v, Kg)
    | (Weight(v, Kg), #Lb) => Weight(v, Lb)
    | (Weight(v, Lb), #Lb) => Weight(v, Lb)
    | (Weight(v, Lb), #Kg) => Weight(v, Kg)
    | (Amrap, #Kg) => Weight(0, Kg)
    | (Rpe(_), #Kg) => Weight(0, Kg)
    | (Bodyweight, #Kg) => Weight(0, Kg)
    | (Amrap, #Lb) => Weight(0, Lb)
    | (Rpe(_), #Lb) => Weight(0, Lb)
    | (Bodyweight, #Lb) => Weight(0, Lb)
    | (Rpe(v), #Rpe) => Rpe(v)
    | (Weight(_, _), #Rpe) => Rpe(0)
    | (Amrap, #Rpe) => Rpe(0)
    | (Bodyweight, #Rpe) => Rpe(0)
    | (_, #Bodyweight) => Bodyweight
    | (_, #Amrap) => Amrap
    }
  }

  let applyMeasure = (w: int, s: scheme) => {
    switch s {
    | Weight(_, Kg) => Weight(w, Kg)
    | Weight(_, Lb) => Weight(w, Lb)
    | Amrap => Amrap
    | Rpe(_) => Rpe(w)
    | Bodyweight => Bodyweight
    }
  }

  let toString = (w: scheme) => {
    switch w {
    | Weight(weight, metric) =>
      Belt.Int.toString(weight) ++
      " " ++
      switch metric {
      | Kg => "kg."
      | Lb => "lb."
      }
    | Amrap => "AMRAP"
    | Rpe(rpe) => "RPE " ++ Belt.Int.toString(rpe)
    | Bodyweight => "Bodyweight"
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
    notes: string,
  }

  let emptyWorkout = () => {date: DateUtils.now(), exercises: [], notes: ""}

  type plan = {workouts: array<workout>}

  let setsToString = (~sets: int, ~reps: int) => {
    Belt.Int.toString(sets) ++ "x" ++ Belt.Int.toString(reps)
  }

  let loadToString = (~sets: int, ~reps: int, ~weight: WeightScheme.scheme) => {
    setsToString(~sets, ~reps) ++ WeightScheme.toString(weight)
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

  let addWorkout = (p: plan, w: workout) => {
    let result = {workouts: ArrayUtils.pushBack(p.workouts, w)}
    Js.Array2.sortInPlaceWith(result.workouts, (l, r) =>
      DateUtils.compareDates(l.date, r.date)
    )->ignore
    result
  }

  let removeWorkout = (p: plan, d: Js.Date.t) => {
    {workouts: Js.Array2.filter(p.workouts, w => w.date != d)}
  }

  let editWorkout = (p: plan, d: Js.Date.t, w: workout): plan => {
    let i = Js.Array2.findIndex(p.workouts, w => DateUtils.sameDate(w.date, d))
    if i == -1 {
      p
    } else {
      let result = {workouts: ArrayUtils.setIndex(p.workouts, i, w)}
      Js.Array2.sortInPlaceWith(result.workouts, (l, r) =>
        DateUtils.compareDates(l.date, r.date)
      )->ignore
      result
    }
  }

  let getWorkout = (p: plan, d: Js.Date.t): option<workout> => {
    Js.Array2.find(p.workouts, w => DateUtils.sameDate(w.date, d))
  }
}
