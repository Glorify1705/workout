module String = {
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

module Array = {
  let removeIndex = (arr: array<'t>, i: int) => {
    let length = Js.Array2.length(arr)
    let prefix = arr->Js.Array2.slice(~start=0, ~end_=i)
    let suffix = arr->Js.Array2.slice(~start=i + 1, ~end_=length)
    Js.Array2.concat(prefix, suffix)
  }

  let setIndex = (arr: array<'t>, i: int, v: 't) => {
    let length = Js.Array2.length(arr)
    let prefix = arr->Js.Array2.slice(~start=0, ~end_=i)
    let suffix = arr->Js.Array2.slice(~start=i + 1, ~end_=length)
    prefix->Js.Array2.concat([v])->Js.Array2.concat(suffix)
  }

  let pushBack = (arr: array<'t>, v: 't) => {
    Js.Array2.concat(arr, [v])
  }

  let insertAt = (arr: array<'t>, i: int, v: 't) => {
    Js.Array2.concatMany(
      Js.Array2.slice(arr, ~start=0, ~end_=i),
      [[v], Js.Array2.slice(arr, ~start=i, ~end_=Js.Array2.length(arr))],
    )
  }
}

module Date = {
  let toIso8601 = (d: Js.Date.t): string => {
    let pad = s =>
      if Js.String.length(s) < 2 {
        "0" ++ s
      } else {
        s
      }
    [
      Js.Date.getFullYear(d),
      1.0 +. Js.Date.getMonth(d),
      Js.Date.getDate(d),
    ]->Belt.Array.joinWith("-", f => f->Belt.Float.toString->pad)
  }

  let fromIso8601 = (s: string): Js.Date.t => {
    Js.Date.fromString(s)
  }

  let sameDate = (lhs: Js.Date.t, rhs: Js.Date.t) => {
    Js.Date.getFullYear(lhs) == Js.Date.getFullYear(rhs) &&
    Js.Date.getMonth(lhs) == Js.Date.getMonth(rhs) &&
    Js.Date.getDate(lhs) == Js.Date.getDate(rhs)
  }

  let compareDates = (lhs: Js.Date.t, rhs: Js.Date.t) => {
    let ls = Js.Date.getSeconds(lhs)
    let rs = Js.Date.getSeconds(rhs)
    if ls < rs {
      -1
    } else if ls > rs {
      1
    } else {
      0
    }
  }

  let now = (): Js.Date.t => Js.Date.make()

  let dayBefore = (d: Js.Date.t) => {
    let result = d
    Js.Date.setDate(result, Js.Date.getDate(d) -. 1.)->ignore
    result
  }
}

module Clipboard = {
  @val external write: string => Js.Promise.t<'a> = "navigator.clipboard.writeText"

  let copy = (s: string) => {
    write(s)
    ->Promise.catch(err => {
      Js.log(err)
      Js.Promise.resolve()
    })
    ->ignore
  }
}

module Markdown = {
  type t
  @module("showdown") @new external makeConverter: unit => t = "Converter"

  @send external convertHtml: (t, string) => string = "makeHtml"
}
