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

  let pushBack = (arr: array<'t>, v: 't) => {
    Array.concat(arr, [v])
  }

  let insertAt = (arr: array<'t>, i: int, v: 't) => {
    Array.concatMany(
      Array.slice(arr, ~start=0, ~end_=i),
      [[v], Array.slice(arr, ~start=i, ~end_=Array.length(arr))],
    )
  }
}

module DateUtils = {
  let toIso8861 = (d: Js.Date.t): string => {
    let s = Js.Date.toISOString(d)
    Js.String2.slice(s, ~from=0, ~to_=Js.String2.indexOf(s, "T"))
  }

  let fromIso8861 = (s: string): Js.Date.t => {
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

module ClipboardUtils = {
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
