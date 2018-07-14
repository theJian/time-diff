type timeUnit =
  | Years(int)
  | Months(int)
  | Weeks(int)
  | Days(int)
  | Hours(int)
  | Minutes(int)
  | Seconds(int)
  | Milliseconds(int);

type timeDiff = {
  years: int,
  months: int,
  weeks: int,
  days: int,
  hours: int,
  minutes: int,
  seconds: int,
  milliseconds: int,
};

type dateRange = (Js.Date.t, Js.Date.t);

exception UnknownUnit(string);

let yearDiff = (range: dateRange) : (timeUnit, dateRange) => {
  let (s, e) = Js.Date.(
    range |> fst |> getFullYear,
    range |> snd |> getFullYear
  );

  let diff = Js.Date.({
    let includeLastYear = setFullYear(fst(range), e) < getTime(snd(range));
    (includeLastYear ? e : e -. 1.0) -. s
  });

  (
    Years(diff |> int_of_float),
    (
      Js.Date.(fromFloat(setFullYear(fst(range), s +. diff))),
      snd(range)
    )
  )
};

let monthDiff = (range: dateRange) : (timeUnit, dateRange) => {
  let (Years(years), range) = yearDiff(range);
  let (s, e) = Js.Date.(
    range |> fst |> getMonth,
    range |> snd |> getMonth
  );

  let diff = Js.Date.({
    let includeLastMonth = setMonth(fst(range), e) < getTime(snd(range));
    (includeLastMonth ? e : e -. 1.0) -. s
  });

  (
    Months(diff |> int_of_float |> (+)(years * 12)),
    (
      Js.Date.(fromFloat(setMonth(fst(range), s +. diff))),
      snd(range)
    )
  )
};

let weekDiff = (range: dateRange) : (timeUnit, dateRange) => {
  let (s, e) = Js.Date.(
    range |> fst |> getTime,
    range |> snd |> getTime
  );

  let oneWeek = 1000 * 60 * 60 * 24 * 7;
  let diff = ((e -. s) |> int_of_float) / oneWeek;

  (
    Weeks(diff),
    (
      Js.Date.(fromFloat(s +. float(diff * oneWeek))),
      snd(range)
    )
  )
};

let dayDiff = (range: dateRange) : (timeUnit, dateRange) => {
  let (s, e) = Js.Date.(
    range |> fst |> getTime,
    range |> snd |> getTime,
  );

  let oneDay = 1000 * 60 * 60 * 24;
  let diff = ((e -. s) |> int_of_float) / oneDay;

  (
    Days(diff),
    (
      Js.Date.(fromFloat(s +. float(diff * oneDay))),
      snd(range)
    )
  )
};

let hourDiff = (range: dateRange) : (timeUnit, dateRange) => {
  let (s, e) = Js.Date.(
    range |> fst |> getTime,
    range |> snd |> getTime
  );

  let oneHour = 1000 * 60 * 60;
  let diff = ((e -. s) |> int_of_float) / oneHour;

  (
    Hours(diff),
    (
      Js.Date.(fromFloat(s +. float(diff * oneHour))),
      snd(range)
    )
  )
};

let minuteDiff = (range: dateRange) : (timeUnit, dateRange) => {
  let (s, e) = Js.Date.(
    range |> fst |> getTime,
    range |> snd |> getTime
  );

  let oneMinute = 1000 * 60;
  let diff = ((e -. s) |> int_of_float) / oneMinute;

  (
    Minutes(diff),
    (
      Js.Date.(fromFloat(s +. float(diff * oneMinute))),
      snd(range)
    )
  )
};

let secondDiff = (range: dateRange) : (timeUnit, dateRange) => {
  let (s, e) = Js.Date.(
    range |> fst |> getTime,
    range |> snd |> getTime
  );

  let oneSecond = 1000;
  let diff = ((e -. s) |> int_of_float) / oneSecond;

  (
    Seconds(diff),
    (
      Js.Date.(fromFloat(s +. float(diff * oneSecond))),
      snd(range)
    )
  )
}

let millisecondDiff = (range: dateRange) : (timeUnit, dateRange) => {
  let (s, e) = Js.Date.(
    range |> fst |> getTime,
    range |> snd |> getTime
  );

  (
    Milliseconds((e -. s) |> int_of_float),
    (
      snd(range),
      snd(range)
    )
  )
}

let timeDiff = (dateA, dateB, units) : timeDiff => {
  let range = ref(Js.Date.({
    let timeA = getTime(dateA);
    let timeB = getTime(dateB);
    timeA > timeB ? (dateB, dateA) : (dateA, dateB)
  }));
  let diff = ref({
    years: 0,
    months: 0,
    weeks: 0,
    days: 0,
    hours: 0,
    minutes: 0,
    seconds: 0,
    milliseconds: 0,
  });

  String.iter(ch => {
    let (unit_, range_) = switch (ch) {
      | 'Y' => range^ |> yearDiff
      | 'M' => range^ |> monthDiff
      | 'W' => range^ |> weekDiff
      | 'D' => range^ |> dayDiff
      | 'H' => range^ |> hourDiff
      | 'm' => range^ |> minuteDiff
      | 's' => range^ |> secondDiff
      | '.' => range^ |> millisecondDiff
      | _   => raise(UnknownUnit("unknown units: " ++ String.make(1, ch)))
    };

    range := range_;
    diff := switch (unit_) {
      | Years(years)               => {...diff^, years}
      | Months(months)             => {...diff^, months}
      | Weeks(weeks)               => {...diff^, weeks}
      | Days(days)                 => {...diff^, days}
      | Hours(hours)               => {...diff^, hours}
      | Minutes(minutes)           => {...diff^, minutes}
      | Seconds(seconds)           => {...diff^, seconds}
      | Milliseconds(milliseconds) => {...diff^, milliseconds}
    }
  }, units);

  diff^
};
