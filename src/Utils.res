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
