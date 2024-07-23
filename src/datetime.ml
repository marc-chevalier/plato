#if OCAML_VERSION >= (4, 7, 0)
type nothing = |
let compare (a: nothing) (b: nothing) : int =
  match a, b with
  | _ -> .
[@@warning "-32"]
#else
type nothing
let compare (a: nothing) (b: nothing) : int =
  match a, b with
  | _ -> assert false
[@@warning "-32"]
#endif

module Time_ = Time

let minyear : int = 1
let maxyear : int = 9999
let _MAXORDINAL : int = 3652059

let _DAYS_IN_MONTH : int array = [| -1; 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |]

let _DAYS_BEFORE_MONTH : int array =
  let a = Stdcompat.Array.make 13 ~-1 in
  let dbm = ref 0 in
  for i = 1 to 12 do
    a.(i) <- !dbm;
    dbm := _DAYS_IN_MONTH.(i) + !dbm
  done;
  a

let is_leap (year: int) : bool =
  year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0)

let days_before_year (year: int) : int =
  let y = year - 1 in
  y * 365 + y / 4 - y / 100 + y / 400

let days_in_month (year: int) (month: int) : int =
  assert (1 <= month && month <= 12);
  if month == 2 && is_leap year then
    29
  else
    _DAYS_IN_MONTH.(month)

let days_before_month (year: int) (month: int) : int =
  assert (1 <= month && month <= 12);
  _DAYS_BEFORE_MONTH.(month) + (if month > 2 && is_leap year then 1 else 0)

let ymd2ord (year: int) (month: int) (day: int) : int =
  assert (1 <= month && month <= 12);
  let dim = days_in_month year month in
  assert (1 <= day && day <= dim);
  days_before_year year + days_before_month year month + day

let di400y = days_before_year 401
let di100y = days_before_year 101
let di4y   = days_before_year 5

let () = assert (di4y = 4 * 365 + 1)
let () = assert (di400y == 4 * di100y + 1)
let () = assert (di100y == 25 * di4y - 1)

let divmod (a: int) (b: int) : int * int =
  a / b, a mod b

let ord2ymd (n: int) : int * int * int =
  let n = n - 1 in
  let n400, n = divmod n di400y in
  let year = n400 * 400 + 1 in
  let n100, n = divmod n di100y in
  let n4, n = divmod n di4y in
  let n1, n = divmod n 365 in
  let year = year + n100 * 100 + n4 * 4 + n1 in
  if n1 = 4 || n100 = 4 then
    let () = assert (n = 0) in
    year - 1, 12, 31
  else
    let leapyear = n1 = 3 && (n4 <> 24 || n100 = 3) in
    let () = assert (leapyear = is_leap year) in
    let month = (n + 50) lsr 5 in
    let preceding = _DAYS_BEFORE_MONTH.(month) + (if month > 2 && leapyear then 1 else 0) in
    let month, preceding =
      if preceding > n then
        let month = month - 1 in
        let preceding = preceding - _DAYS_IN_MONTH.(month) + (if month = 2 && leapyear then 1 else 0) in  
        month, preceding
      else
        month, preceding
    in
    let n = n - preceding in
    let () = assert (0 <= n && n < days_in_month year month) in
    year, month, n+1


let _MONTHNAMES : string array = [| ""; "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
let _DAYNAMES : string array = [| ""; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun" |]

let build_struct_time (y: int) (m: int) (d: int) (hh: int) (mm: int) (ss: int) (dstflag: int) : Time.struct_time =
  let wday = ((ymd2ord y m d) + 6) mod 7 in
  let dnum = days_before_month y m + d in
  {
    tm_sec = ss;
    tm_min = mm;
    tm_hour = hh;
    tm_mday = d;
    tm_mon = m;
    tm_year = y;
    tm_wday = wday;
    tm_yday = dnum;
    tm_isdst = dstflag;
  }

let format_time ?(timespec: string = "auto") (hh: int) (mm: int) (ss: int) (us: int) : string =
  let timespec, us =
    if timespec = "auto" then
      begin (if us = 0 then "seconds" else "microseconds"), us end
    else if timespec = "milliseconds" then
      timespec, us / 1000
    else
      timespec, us
  in
  if timespec = "hours" then
    Format.asprintf "%02d"  hh
  else if timespec = "minutes" then
    Format.asprintf "%02d:%02d"  hh mm
  else if timespec = "seconds" then
    Format.asprintf "%02d:%02d:%02d"  hh mm ss
  else if timespec = "milliseconds" then
    Format.asprintf "%02d:%02d:%02d:%03d" hh mm ss us
  else if timespec = "microseconds" then
    Format.asprintf "%02d:%02d:%02d:%06d" hh mm ss us
  else
    raise (Exn.ValueError "Unknown timespec value")

let parse_isoformat_date (dtstr: string) : int * int * int =
  let year = Str.slice ~start:0 ~stop:4 dtstr |> int_of_string in
  if Str.at dtstr 4 <> "-" then
    raise (Exn.ValueError("Invalid date separator: " ^ (Str.at dtstr 4)));
  let month = Str.slice ~start:5 ~stop:7 dtstr |> int_of_string in
  if Str.at dtstr 7 <> "-" then
    raise (Exn.ValueError("Invalid date separator"));
  let day = Str.slice ~start:8 ~stop:10 dtstr |> int_of_string in
  year, month, day

let parse_hh_mm_ss_ff (tstr: string) : int * int * int * int =
  let len_str : int = Str.len tstr in
  let time_comps : int array = [| 0; 0; 0; 0 |] in
  let rec f (pos: int) (comp: int) : int =
    if len_str - pos < 2 then
      raise (Exn.ValueError "Incomplete time component");
    let () = time_comps.(comp) <- Str.slice ~start:pos ~stop:(pos + 2) tstr |> int_of_string in
    let pos = pos + 2 in
    let next_char = Str.slice ~start:pos ~stop:(pos + 2) tstr in
    if Str.bool next_char || comp >= 2 then
      pos
    else if next_char <> ":" then
      raise (Exn.ValueError "Invalid time separator: ^ next_char")
    else
      f (pos + 1) (comp + 1)
  in
  let pos = f 0 0 in
  let () =
    if pos < len_str then
      if Str.at tstr pos <> "." then
        raise (Exn.ValueError "Invalid microsecond component")
      else
        let pos = pos + 1 in
        let len_remainder = len_str - pos in
        if len_remainder <> 3 && len_remainder <> 6 then
          raise (Exn.ValueError "Invalid microsecond component");
        let () = time_comps.(3) <- Str.slice ~start:pos tstr |> int_of_string in
        if len_remainder = 3 then
          time_comps.(3) <- time_comps.(3) * 1000
  in
  time_comps.(0), time_comps.(1), time_comps.(2), time_comps.(3)

let check_date_fields (year: int) (month: int) (day: int) : unit =
  if not (minyear <= year && year <= maxyear) then
    raise (Exn.ValueError (Format.asprintf "Year must be in %d..%d" minyear maxyear));
  if not (1 <= month && month <= 12) then
    raise (Exn.ValueError("month must be in 1..12"));
  let dim = days_in_month year month in
  if not (1 <= day && day <= dim) then
    raise (Exn.ValueError (Format.asprintf "day must be in 1..%d" dim))

let check_time_fields (hour: int) (minute: int) (second: int) (microsecond: int) (fold: int): unit =
  if not (0 <= hour && hour <= 23) then
    raise (Exn.ValueError "hour must be in 0..23");
  if not (0 <= minute && minute <= 59) then 
    raise (Exn.ValueError "minute must be in 0..59");
  if not (0 <= second && second <= 59) then
    raise (Exn.ValueError "second must be in 0..59");
  if not (0 <= microsecond && microsecond <= 999999) then
    raise (Exn.ValueError "microsecond must be in 0..999999");
  if fold <> 0 && fold <> 1 then
    raise (Exn.ValueError "fold must be either 0 or 1")

let divide_and_round (a: int) (b: int) : int =
  let q, r = divmod a b in
  let r = 2 * r in
  let greater_than_half = if b > 0 then r > b else r < b in 
  if greater_than_half || r = b && q mod 2 = 1 then
    q + 1
  else
    q


module Timedelta =
  (struct
    type t = {
      days: int;
      seconds: int;
      microseconds: int;
      mutable hashcode: int;
    }


    let make ?(days: int = 0) ?(seconds: int = 0) ?(microseconds: int = 0)
        ?(milliseconds: int = 0) ?(minutes: int = 0) ?(hours: int = 0)
        ?(weeks: int = 0) ((): unit) : t =
      let s = 0 in
      let days = days + weeks * 7 in
      let seconds = seconds + minutes*60 + hours*3600 in 
      let microseconds = microseconds + milliseconds*1000 in
      let d = days in
      let days, seconds = divmod seconds (24*3600) in
      let d = d + days in
      let s = s + seconds in
      let seconds, microseconds = divmod microseconds 1000000 in
      let days, seconds = divmod seconds (24*3600) in
      let d = d + days in
      let s = s + seconds in
      let seconds, us = divmod microseconds 1000000 in
      let s = s + seconds in
      let days, s = divmod s (24*3600) in
      let d = d + days in
      {
        days = d;
        seconds = s;
        microseconds = us;
        hashcode = ~-1;
      }

    let repr (self: t) : string =
      let args = ref [] in
      if self.days <> 0 then
        args := (Format.asprintf "days=%d"  self.days)::!args;
      if self.seconds <> 0 then
        args := (Format.asprintf "seconds=%d"  self.seconds)::!args;
      if self.microseconds <> 0 then
        args := (Format.asprintf "microseconds=%d"  self.microseconds)::!args;
      if !args = [] then
        args := ["0"];
      Format.asprintf "%s.%s(%s)" __MODULE__ "datetime" (Stdcompat.String.concat "" (Stdcompat.List.rev !args))

    let to_string (self: t) : string =
      let mm, ss = divmod self.seconds 60 in
      let hh, mm = divmod mm 60 in
      let s = Format.asprintf "%d:%02d:%02d" hh mm ss in
      let s =
        if self.days <> 0 then
          let plural (n: int) : string =
            if abs n <> 1 then "s" else ""
          in
          Format.asprintf "%d days%s, %s" self.days (plural self.days) s
        else
          s
      in
      let s =
        if self.microseconds <> 0 then
          Format.asprintf "%s.%06d" s self.microseconds
        else
          s
      in
      s

    let total_seconds (self: t) : float =
      float_of_int ((self.days * 86400 + self.seconds) * 1_000_000 + self.microseconds) /. 1e6

    let days ({days; _}: t) : int =
      days

    let seconds ({seconds; _}: t) : int =
      seconds

    let microseconds ({microseconds; _}: t) : int =
      microseconds

    let add (self: t) (other: t) : t =
      let days = self.days + other.days in
      let seconds = self.seconds + other.seconds in
      let microseconds = self.microseconds + other.microseconds in
      make ~days ~seconds ~microseconds ()

    let sub (self: t) (other: t) : t =
      let days = self.days - other.days in
      let seconds = self.seconds - other.seconds in
      let microseconds = self.microseconds - other.microseconds in
      make ~days ~seconds ~microseconds ()

    let neg (self: t) : t =
      let days = - self.days in
      let seconds = - self.seconds in
      let microseconds = - self.microseconds in
      make ~days ~seconds ~microseconds ()

    let abs (self: t) : t =
      if self.days < 0 then
        neg self
      else
        self

    let mul (self: t) (other: int) : t =
      let days = self.days  * other in
      let seconds = self.seconds  * other in
      let microseconds = self.microseconds  * other in
      make ~days ~seconds ~microseconds ()

    let to_microseconds (self: t) : int =
      ((self.days * (24*3600) + self.seconds) * 1000000 + self.microseconds)

    let floordiv_t (self: t) (other: t) : int =
      to_microseconds self / to_microseconds other

    let floordiv_int (self: t) (other: int) : t =
      make ~microseconds:(to_microseconds self / other) ()

    let truediv_t (self: t) (other: t) : float =
      (to_microseconds self |> float_of_int) /. (to_microseconds other |> float_of_int)

    let truediv_int (self: t) (other: int) : t =
      make ~microseconds:(divide_and_round (to_microseconds self) other) ()

    let modulo (self: t) (other: t) : t =
      let r = to_microseconds self mod to_microseconds other in
      make ~microseconds:r ()

    let divmod (self: t) (other: t) : int * t =
      let q, r = divmod (to_microseconds self) (to_microseconds other) in
      q, make ~microseconds:r ()

    let compare (self: t) (other: t) : int =
      let c = Stdcompat.Int.compare self.days other.days in
      if c <> 0 then
        c
      else
        let c = Stdcompat.Int.compare self.seconds other.seconds in
        if c <> 0 then
          c
        else
          Stdcompat.Int.compare self.microseconds other.microseconds

    let eq (self: t) (other: t) : bool =
      compare self other = 0

    let ne (self: t) (other: t) : bool =
      compare self other <> 0

    let le (self: t) (other: t) : bool =
      compare self other <= 0

    let lt (self: t) (other: t) : bool =
      compare self other < 0

    let ge (self: t) (other: t) : bool =
      compare self other >= 0

    let gt (self: t) (other: t) : bool =
      compare self other > 0

    module Cmp =
      (struct
        let (=) = eq
        let (<>) = ne
        let (<=) = le
        let (<) = lt
        let (>=) = ge
        let (>) = gt
      end)

    let hash (self: t) : int =
      if self.hashcode = ~- 1 then
        self.hashcode <- Stdcompat.Hashtbl.hash (self.days, self.seconds, self.microseconds);
      self.hashcode

    let bool (self: t) : bool =
      self.days <> 0 || self.seconds <> 0 || self.microseconds <> 0

    let min : t = make ~days:~-999999999 ()
    let max : t = make ~days:999999999 ~hours:23 ~minutes:59 ~seconds:59 ~microseconds:999999 ()
    let resolution : t = make ~microseconds:1 ()

  end)

let format_offset (off: Timedelta.t option) : string =
  match off with
  | None -> ""
  | Some off ->
    let off, sign =
      if Timedelta.days off < 0 then
        Timedelta.neg off, "-"
      else
        off, "+"
    in
    let hh, mm = Timedelta.(divmod off (make ~hours:1 ())) in
    let mm, ss = Timedelta.(divmod mm (make ~minutes:1 ())) in
    let s = Format.asprintf "%s%02d:%02d" sign hh mm in
    if Timedelta.bool ss || Timedelta.microseconds ss <> 0 then
      begin
        let s = Format.asprintf "%s:%02d" s ss.seconds in
        if Timedelta.microseconds ss <> 0 then
          Format.asprintf "%s%06d" s ss.microseconds
        else
          s
      end
    else
      s

let isoweek1monday (year: int) : int =
  let _THURSDAY = 3 in
  let firstday = ymd2ord year 1 1 in
  let firstweekday = (firstday + 6) mod 7 in
  let week1monday = firstday - firstweekday in
  if firstweekday > _THURSDAY then
    week1monday + 7
  else
    week1monday

module Date =
  (struct

    type t = {
      year: int;
      month: int;
      day: int;
      mutable hashcode: int;
    }

    let make (year: int) (month: int) (day: int) : t =
      let hashcode : int = ~- 1 in
      {year; month; day; hashcode}

    let fromtimestamp (t: float) : t =
      let Time.{tm_year; tm_mon; tm_mday; _} = Time.localtime t in
      make tm_year tm_mon tm_mday

    let today ((): unit) : t =
      Time.time () |> fromtimestamp

    let fromordinal (n: int) : t =
      let y, m, d = ord2ymd n in
      make y m d

    let fromisoformat(date: string) : t =
      if Str.len date <> 10 then
        raise (Exn.ValueError ("Invalid isoformat string: " ^ date));
      let y, m, d = parse_isoformat_date date in
      make y m d

    let fromisocalendar (year: int) (week: int) (day: int) : t =
      if not (minyear <= year && year <= maxyear) then
        raise (Exn.ValueError (Format.asprintf "Year is out of range: %d" year));
      if not (0 < week && week < 53) then
        begin
          let out_of_range = ref true in
          begin
            if week = 53 then
              let first_weekday = (ymd2ord year 1 1) mod 7 in
              if first_weekday = 4 || (first_weekday = 3 && is_leap year) then
                out_of_range := false
          end;
          if !out_of_range then
            raise (Exn.ValueError (Format.asprintf "Invalid week: %d" week))
        end;
      if not (0 < day && day < 8) then
        raise (Exn.ValueError (Format.asprintf "Invalid weekday: %d (range is [1, 7])" day));
      let day_offset = (week - 1) * 7 + day - 1 in
      let day_1 = isoweek1monday year in
      let ord_day = day_1 + day_offset in
      let y, m, d = ord2ymd ord_day in
      make y m d

    let repr (self: t) : string =
      Format.asprintf "%s.%s(%d, %d, %d)" __MODULE__ "date" self.year self.month self.day

    let toordinal (self: t) : int =
      ymd2ord self.year self.month self.day

    let ctime (self: t) : string =
      let weekday = (toordinal self mod 7) in
      let weekday = if weekday = 0 then 7 else weekday in
      Format.asprintf "%s %s %2d 00:00:00 %04d" _DAYNAMES.(weekday) _MONTHNAMES.(self.month) self.day self.year

    let isoformat (self: t) : string =
      Format.asprintf "%04d-%02d-%02d" self.year self.month self.day

    let str = isoformat

    let year ({year; _}: t) : int =
      year

    let month ({month; _}: t) : int =
      month

    let day ({day; _}: t) : int =
      day

    let timetuple (self: t) : Time_.struct_time =
      build_struct_time self.year self.month self.day 0 0 0 ~-1

    let replace ?(year: int option) ?(month: int option) ?(day: int option) (self: t) : t =
      let year = Stdcompat.Option.value year ~default:self.year in
      let month = Stdcompat.Option.value month ~default:self.month in
      let day = Stdcompat.Option.value day ~default:self.day in
      make year month day

    let compare (self: t) (other: t) : int =
      let c = Stdcompat.Int.compare self.year other.year in
      if c <> 0 then
        c
      else
        let c = Stdcompat.Int.compare self.month other.month in
        if c <> 0 then
          c
        else
          Stdcompat.Int.compare self.day other.day

    let eq (self: t) (other: t) : bool =
      compare self other = 0

    let le (self: t) (other: t) : bool =
      compare self other <= 0

    let lt (self: t) (other: t) : bool =
      compare self other < 0

    let ge (self: t) (other: t) : bool =
      compare self other >= 0

    let gt (self: t) (other: t) : bool =
      compare self other > 0

    let hash (self: t) : int =
      if self.hashcode = ~-1 then
        self.hashcode <- Stdcompat.Hashtbl.hash (self.year, self.month, self.day);
      self.hashcode

    let add (self: t) (other: Timedelta.t) : t =
      let o = toordinal self + Timedelta.days other in
      if 0 < o && o <= _MAXORDINAL then
        fromordinal o
      else
        raise (Exn.OverflowError "Result out of range")

    let sub (self: t) (other: t) : Timedelta.t =
      let days1 = toordinal self in
      let days2 = toordinal other in
      Timedelta.make ~days:(days1-days2) ()

    let sub_delta (self: t) (other: Timedelta.t) : t =
      add self (Timedelta.make ~days:other.days ())

    let weekday (self: t) : int =
      (toordinal self + 6) mod 7

    let isoweekday (self: t) : int =
      let c = toordinal self mod 7 in
      if c <> 0 then
        c
      else
        7

    let isocalendar (self: t) : int * int * int =
      let year = self.year in
      let weed1monday = isoweek1monday year in
      let today = ymd2ord self.year self.month self.day in
      let week, day = divmod (today - weed1monday) 7 in
      if week < 0 then
        let year = year - 1 in
        let weed1monday = isoweek1monday year in
        let week, day = divmod (today - weed1monday) 7 in
        year, week + 1, day + 1
      else if week >= 52 then
        begin
          if today >= isoweek1monday (year + 1) then 
            let year = year + 1 in
            let week = 0 in
            year, week + 1, day + 1
          else 
            year, week + 1, day + 1
        end
      else
        year, week + 1, day + 1

    let min = make 1 1 1
    let max = make 9999 12 31
    let resolution = Timedelta.make ~days:1 ()

  end)

let check_utc_offset (name: string) (offset: Timedelta.t option) : unit =
  assert (name = "utcoffset" || name = "dst");
  match offset with
  | None -> ()
  | Some offset ->
    if not Timedelta.(lt (make ~days:1 () |> neg) offset && lt offset (make ~days:1 ())) then
      raise (Exn.ValueError (Format.asprintf "%s()=%s, must be strictly between -timedelta(hours=24) and timedelta(hours=24)" name (Timedelta.to_string offset)))

module TimeDatetimeTZ =
  (struct

    type datetime = {
      year: int;
      month: int;
      day: int;
      hour: int;
      minute: int;
      second: int;
      microsecond: int;
      tzinfo: tzinfo option;
      mutable hashcode: int;
      fold: int;
    }
    and tzinfo = {
      tzname: datetime option -> string;
      utcoffset: datetime option -> Timedelta.t;
      dst: datetime option -> Timedelta.t option;
      fromutc: datetime -> datetime;
      repr: string;
    }
    type time = {
      hour: int;
      minute: int;
      second: int;
      microsecond: int;
      tzinfo: tzinfo option;
      mutable hashcode: int;
      fold: int;
    }

    module Time =
      (struct
        type t = time

        let make ?(hour: int = 0) ?(minute: int = 0) ?(second: int = 0) ?(microsecond: int = 0) ?(tzinfo: tzinfo option) ?(fold: int = 0) ((): unit) : t =
          check_time_fields hour minute second microsecond fold;
          {hour; minute; second; microsecond; tzinfo; hashcode = ~-1; fold}

        let hour ({hour; _}: t) : int =
          hour

        let minute ({minute; _}: t) : int =
          minute

        let second ({second; _}: t) : int =
          second

        let microsecond ({microsecond; _}: t) : int =
          microsecond

        let tzinfo ({tzinfo; _}: t) : tzinfo option =
          tzinfo

        let fold ({fold; _}: t) : int =
          fold

        let utcoffset (self: t) : Timedelta.t option =
          match self.tzinfo with
          | None -> None
          | Some tzinfo ->
            let offset = tzinfo.utcoffset None in
            let () = check_utc_offset "utcoffset" (Some offset) in
            Some offset

        let compare ?(allow_mixed: bool = false) (self: t) (other: t) : int =
          let mytz = self.tzinfo in
          let ottz = other.tzinfo in
          let myoff, otoff, base_compare =
            if mytz == ottz then
              None, None, true
            else
              let myoff = utcoffset self in
              let otoff = utcoffset other in
              myoff, otoff, myoff = otoff
          in
          if base_compare then
            let c = Stdcompat.Int.compare self.hour other.hour in
            if c <> 0 then c
            else
              let c = Stdcompat.Int.compare self.minute other.minute in
              if c <> 0 then c
              else
                let c = Stdcompat.Int.compare self.second other.second in
                if c <> 0 then c
                else
                  Stdcompat.Int.compare self.microsecond other.microsecond
          else
            match myoff, otoff with
            | _, None | None, _ -> if allow_mixed then 2 else raise (Exn.TypeError "cannot compare naive and aware times")
            | Some myoff, Some otoff ->
              let myhhmm = self.hour * 60 + self.minute - Timedelta.floordiv_t myoff (Timedelta.make ~minutes:1 ()) in
              let othhmm = other.hour * 60 + other.minute - Timedelta.floordiv_t otoff ((Timedelta.make ~minutes:1 ())) in
              let c = Stdcompat.Int.compare myhhmm othhmm in
              if c <> 0 then c
              else
                let c = Stdcompat.Int.compare self.second other.second in
                if c <> 0 then c
                else
                  Stdcompat.Int.compare self.microsecond other.microsecond

        let eq (self: t) (other: t) : bool =
          compare self other = 0

        let le (self: t) (other: t) : bool =
          compare self other <= 0

        let lt (self: t) (other: t) : bool =
          compare self other < 0

        let ge (self: t) (other: t) : bool =
          compare self other >= 0

        let gt (self: t) (other: t) : bool =
          compare self other > 0

        let replace ?(hour: int option) ?(minute: int option) ?(second: int option)
            ?(microsecond: int option) ?(tzinfo: bool = true) ?(fold: int option) (self: t) : t =
          let hour = Stdcompat.Option.value ~default:self.hour hour in
          let minute = Stdcompat.Option.value ~default:self.minute minute in
          let second = Stdcompat.Option.value ~default:self.second second in
          let microsecond = Stdcompat.Option.value ~default:self.microsecond microsecond in
          let tzinfo = if tzinfo then self.tzinfo else None in
          let fold = Stdcompat.Option.value ~default:self.fold fold in
          make ~hour ~minute ~second ~microsecond ?tzinfo ~fold ()

        let hash (self: t) : int =
          if self.hashcode = ~-1  then
            begin
              let t =
                if self.fold <> 0 then
                  replace ~fold:0 self
                else
                  self
              in
              let tzoff = utcoffset t in
              match tzoff with
              | None -> self.hashcode <- Stdcompat.Hashtbl.hash (self.hour, self.minute, self.second, self.microsecond)
              | Some tzoff ->
                let a = Timedelta.(sub (make ~hours:self.hour ~minutes:self.minute ()) tzoff) in
                let b = Timedelta.(make ~hours:1 ()) in
                let h = Timedelta.floordiv_t a b in
                let m = Timedelta.modulo a b in
                assert (Timedelta.(modulo m (make ~minutes:1 ()) |> bool));
                let m = Timedelta.(floordiv_t m (make ~minutes:1 ())) in
                if 0 <= h && h < 24 then
                  self.hashcode <- Stdcompat.Hashtbl.hash (make ~hour:h ~minute:m ~second:self.second ~microsecond:self.microsecond ())
                else 
                  self.hashcode <- Stdcompat.Hashtbl.hash (h, m, self.second, self.microsecond)
            end;
          self.hashcode

        let repr (self: t) : string =
          let s =
            if self.microsecond <> 0 then
              Format.asprintf ", %d, %d" self.second self.microsecond
            else if self.second <> 0 then
              Format.asprintf ", %d" self.second
            else
              ""
          in
          let s = Format.asprintf "%s.%s(%d, %d%s)" __MODULE__ "time" self.hour self.minute s in
          let s =
            match self.tzinfo with
            | None -> s
            | Some tzinfo ->
              assert (Str.slice ~start:~-1 s = ")");
              Format.asprintf "%s, tzinfo=%s)" (Str.slice ~stop:~-1 s) tzinfo.repr
          in
          if self.fold <> 0 then
            let () = assert (Str.slice ~start:~-1 s = ")") in
            Format.asprintf "%s, fold=1)" (Str.slice ~stop:~-1 s)
          else
            s

        let tzstr (self: t) : string =
          let off = utcoffset self in
          format_offset off

        let isoformat ?(timespec: string = "auto") (self: t) : string =
          let s = format_time ~timespec self.hour self.minute self.second self.microsecond in
          let tz = tzstr self in
          if Str.bool tz then
            s ^ tz
          else
            s

        let to_string (self: t) : string = isoformat self

        let tzname ({tzinfo; _}: t) : string option =
          match tzinfo with
          | None -> None
          | Some tzinfo -> Some (tzinfo.tzname None)

        let dst ({tzinfo; _}: t) : Timedelta.t option =
          match tzinfo with
          | None -> None
          | Some tzinfo -> 
            let offset = tzinfo.dst None in
            let () = check_utc_offset "dst" offset in
            offset

        let min = make ()
        let max = make ~hour:23 ~minute:59 ~second:59 ~microsecond:999_999 ()
        let resolution = Timedelta.make ~microseconds:1 ()

      end)

    module Datetime =
      (struct

        type t = datetime

        let make (year: int) (month: int) (day: int)
            ?(hour: int = 0) ?(minute: int = 0) ?(second: int = 0)
            ?(microsecond: int = 0) ?(tzinfo: tzinfo option) ?(fold: int = 0) ((): unit) : t =
          let () = check_date_fields year month day in
          let () = check_time_fields hour minute second microsecond fold in
          {
            year;
            month;
            day;
            hour;
            minute;
            second;
            microsecond;
            tzinfo;
            hashcode = ~-1;
            fold;
          }

        let hour ({hour; _}: t) : int =
          hour

        let minute ({minute; _}: t) : int =
          minute

        let second ({second; _}: t) : int =
          second

        let microsecond ({microsecond; _}: t) : int =
          microsecond

        let tzinfo ({tzinfo; _}: t) : tzinfo option =
          tzinfo

        let fold ({fold; _}: t) : int =
          fold

        let utcoffset (self: t) : Timedelta.t option =
          match self.tzinfo with
          | None -> None
          | Some tzinfo ->
            let offset = tzinfo.utcoffset (Some self) in
            let () = check_utc_offset "utcoffset" (Some offset) in
            Some offset


        let toordinal (self: t) : int =
          ymd2ord self.year self.month self.day

        let sub (self: t) (other: t) : Timedelta.t =
          let days1 = toordinal self in
          let days2 = toordinal other in
          let secs1 = self.second + self.minute*60 + self.hour*3600 in
          let secs2 = other.second + self.minute*60 + self.hour*3600 in
          let base = Timedelta.make ~days:(days1-days2) ~seconds:(secs1-secs2) ~microseconds:(self.microsecond -other.microsecond) () in
          if self.tzinfo == other.tzinfo then
            base
          else
            let myoff = utcoffset self in
            let otoff = utcoffset other in
            match myoff, otoff with
            | None, None -> base
            | None, _ | _, None -> raise (Exn.TypeError "cannot mix maive and timezone-aware time")
            | Some myoff, Some otoff ->
              if Timedelta.eq myoff otoff then
                base
              else
                Timedelta.sub (Timedelta.add base otoff)  myoff

        let replace ?(year: int option) ?(month: int option) ?(day: int option)
            ?(hour: int option) ?(minute: int option) ?(second: int option)
            ?(microsecond: int option) ?(tzinfo: tzinfo option) ?(fold: int option)
            (self: t) : t =
          let module Option = Stdcompat.Option in
          let year = Option.value year ~default:self.year in
          let month = Option.value month ~default:self.month in
          let day = Option.value day ~default:self.day in
          let hour = Option.value hour ~default:self.hour in
          let minute = Option.value minute ~default:self.minute in
          let second = Option.value second ~default:self.second in
          let microsecond = Option.value microsecond ~default:self.microsecond in
          let tzinfo =
            match tzinfo with
            | Some tzinfo -> Some tzinfo
            | None -> self.tzinfo
          in
          let fold = Option.value fold ~default:self.fold in
          make year month day ~hour ~minute ~second ~microsecond ?tzinfo ~fold ()

        let cmp ?(allow_mixed: bool = false) (self: t) (other: t) : int =
          let mytz = self.tzinfo in
          let ottz = other.tzinfo in
          let myoff, otoff, base_compare, return =
            if mytz == ottz then
              None, None, true, None
            else
              let myoff = utcoffset self in
              let otoff = utcoffset other in
              if allow_mixed then
                begin
                  if myoff <> (replace ~fold:(1 - self.fold) self |> utcoffset) then
                    myoff, otoff, true, Some 2
                  else if otoff <> (replace ~fold:(1 - self.fold) other |> utcoffset) then
                    myoff, otoff, true, Some 2
                  else
                    myoff, otoff, (match myoff, otoff with None, None -> false | Some myoff, Some otoff -> Timedelta.eq myoff otoff | Some _, None | None, Some _ -> false), None
                end
              else
                myoff, otoff, (match myoff, otoff with None, None -> false | Some myoff, Some otoff -> Timedelta.eq myoff otoff | Some _, None | None, Some _ -> false), None
          in
          match return with
          | Some return -> return
          | None -> 
            if base_compare then
              Stdcompat.compare
                (self.year, self.month, self.day, self.hour, self.minute, self.second, self.microsecond)
                (other.year, other.month, other.day, other.hour, other.minute, other.second, other.microsecond)
            else
              match myoff, otoff with
              | None, _ | _, None -> if allow_mixed then 2 else raise (Exn.TypeError "cannot compare naive and aware datetimes")
              | Some _, Some _ ->
                let diff = sub self other in
                if diff.days < 0 then
                  ~- 1
                else
                if Timedelta.bool diff then
                  1
                else
                  0

        let compare (self: t) (other: t) : int =
          cmp self other

        let eq (self: t) (other: t) : bool =
          compare self other = 0

        let le (self: t) (other: t) : bool =
          compare self other <= 0

        let lt (self: t) (other: t) : bool =
          compare self other < 0

        let ge (self: t) (other: t) : bool =
          compare self other >= 0

        let gt (self: t) (other: t) : bool =
          compare self other > 0

        let fromtimestamp_ (t: float) (utc: bool) (tz: tzinfo option) : t =
          let frac, t = Stdcompat.Float.modf t in
          let us = frac *. 1e6 |> Stdcompat.Float.round |> int_of_float in
          let t, us =
            if us >= 1_000_000 then
              t +. 1., us - 1_000_000
            else if us < 0 then
              t -. 1., us + 1_000_000
            else
              t, us
          in
          let converter = if utc then Time_.gmttime else Time_.localtime in
          let tm = converter t in
          let y, m, d, hh, mm, ss, _weekday, _jday, _dst =
            tm.tm_year,
            tm.tm_mon,
            tm.tm_mday,
            tm.tm_hour,
            tm.tm_min,
            tm.tm_sec,
            tm.tm_wday,
            tm.tm_yday,
            tm.tm_isdst
          in
          let ss = min ss 59 in
          let result = make y m d ~hour:hh ~minute:mm ~second:ss ~microsecond:us ?tzinfo:tz () in
          match tz with
          | None ->
            let max_fold_seconds = 24 * 3600 in
            if t < float_of_int max_fold_seconds && Sys.win32 then
              result
            else
              let y, m, d, hh, mm, ss =
                let tm = converter (t -. float_of_int max_fold_seconds) in
                tm.tm_year, tm.tm_mon, tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec
              in
              let probe1 = make y m d ~hour:hh ~minute:mm ~second:ss ~microsecond:us ?tzinfo:tz () in
              let trans = Timedelta.sub (sub result probe1) (Timedelta.make ~seconds:max_fold_seconds ()) in
              let result =
                if Timedelta.days trans < 0 then
                  let y, m, d, hh, mm, ss =
                    let tm = converter (t +. float_of_int (Timedelta.(floordiv_t trans (make ~seconds:1 ())))) in
                    tm.tm_year, tm.tm_mon, tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec
                  in
                  let probe2 = make y m d ~hour:hh ~minute:mm ~second:ss ~microsecond:us ?tzinfo:tz () in
                  if eq probe2 result then
                    {result with fold = 1}
                  else
                    result
                else
                  result
              in
              result
          | Some tz -> tz.fromutc result

        let fromtimestamp ?(tz: tzinfo option) (t: float) : t =
          fromtimestamp_ t (Stdcompat.Option.is_some tz) tz

        let utcfromtimestamp (t: float) : t =
          fromtimestamp_ t true None

        let now ?(tz: tzinfo option) ((): unit) : t =
          let t = Time_.time () in
          fromtimestamp ?tz t

        let utcnow ((): unit) : t =
          let t = Time_.time () in
          utcfromtimestamp t

        let combine ?(tzinfo: tzinfo option) (date: Date.t) (time: Time.t) : t =
          let tzinfo =
            match tzinfo with
            | None -> time.tzinfo
            | Some _ -> tzinfo
          in
          make (Date.year date) (Date.month date) (Date.day date)
            ~hour:(Time.hour time) ~minute:(Time.minute time) ~second:(Time.second time)
            ?tzinfo ~fold:(Time.fold time)
            ()

        let add (self: t) (other: Timedelta.t) : t =
          let delta = Timedelta.make ~days:(toordinal self) ~hours:self.hour ~minutes:self.minute ~seconds:self.second ~microseconds:self.microsecond () in
          let delta = Timedelta.add delta other in
          let hour, rem = divmod (Timedelta.seconds delta) 3600 in
          let minute, second = divmod rem 60 in
          if 0 < delta.days && delta.days <= _MAXORDINAL then
            combine (Date.fromordinal (Timedelta.days delta)) (Time.make ~hour ~minute ~second ~microsecond:(Timedelta.microseconds delta) ?tzinfo:self.tzinfo ())
          else
            raise (Exn.OverflowError "result out of range")

        let make_timezone: ?name:string -> Timedelta.t -> tzinfo =
          let maxoffset = Timedelta.make ~hours:24 ~microseconds:~-1 () in
          let minoffset = Timedelta.neg maxoffset in
          fun ?(name: string option) (td: Timedelta.t) : tzinfo ->
            if not Timedelta.(le minoffset td && le td maxoffset) then
              raise (Exn.ValueError "offset must be a timedelta strictly between -timedelta(hours=24) and timedelta(hours=24).");
            let name_from_offset (delta: Timedelta.t) : string =
              if Timedelta.bool delta |> not then
                "UTC"
              else
                let sign, delta =
                  if Timedelta.le delta (Timedelta.make ()) then
                    "-", Timedelta.neg delta
                  else
                    "+", delta
                in
                let hours, rest = Timedelta.divmod delta (Timedelta.make ~hours:1 ()) in
                let minutes, rest = Timedelta.divmod rest (Timedelta.make ~minutes:1 ()) in
                let seconds = Timedelta.seconds rest in
                let microseconds = Timedelta.microseconds rest in
                if microseconds <> 0 then
                  Format.asprintf "UTC%s%02d:%02d:%02d.%06d" sign hours minutes seconds microseconds
                else if seconds <> 0 then
                  Format.asprintf "UTC%s%02d:%02d:%02d" sign hours minutes seconds
                else
                  Format.asprintf "UTC%s%02d:%02d" sign hours minutes
            in
            let tzname (_dt: datetime option) : string =
              match name with
              | None -> name_from_offset td
              | Some name -> name
            in
            let utcoffset (_dt: datetime option) : Timedelta.t =
              td
            in
            let dst (_dt: datetime option) : Timedelta.t option =
              None
            in
            let fromutc (dt: datetime) : datetime =
              add dt td
            in
            let repr: string =
              if Timedelta.bool td |> not then
                "datetime.timezone.utc"
              else
                match name with
                | None -> Format.asprintf "%s.%s(%s)" __MODULE__ "timezone" (Timedelta.repr td)
                | Some name -> Format.asprintf "%s.%s(%s, %s)" __MODULE__ "timezone" (Timedelta.repr td) name
            in
            {
              tzname: datetime option -> string;
              utcoffset: datetime option -> Timedelta.t;
              dst: datetime option -> Timedelta.t option;
              fromutc: datetime -> datetime;
              repr: string;
            }

        let utc_timezone: tzinfo = make_timezone (Timedelta.make ())

        let parse_isoformat_time (tstr: string) : int * int * int * int * tzinfo option =
          let len_str = Str.len tstr in
          if len_str < 2 then
            raise (Exn.ValueError "Isoformat time too short");
          let tz_pos =
            let p = 1 + Str.find "-" tstr in
            if p <> 0 then
              p
            else
              1 + Str.find "+" tstr
          in
          let timestr = if tz_pos > 0 then Str.slice ~stop:(tz_pos - 1) tstr else tstr in
          let hh, mm, ss, ff = parse_hh_mm_ss_ff timestr in
          let tzi =
            if tz_pos > 0 then
              let tzstr = Str.slice ~start:tz_pos tstr in
              if let l = Str.len tzstr in l <> 5 && l <> 8 && l <> 15 then
                raise (Exn.ValueError "Malformed time zone string");
              let hours, minutes, seconds, microseconds = parse_hh_mm_ss_ff tzstr in
              if hours = 0 && minutes = 0 && seconds = 0 && microseconds = 0 then
                Some utc_timezone
              else
                let tzsign = if Str.at tstr (tz_pos - 1) = "-" then ~-1 else 1 in
                let td = Timedelta.make ~hours ~minutes ~seconds ~microseconds () in
                Some (make_timezone (Timedelta.mul td tzsign))
            else
              None
          in
          hh, mm, ss, ff, tzi

        let fromisoformat (date_string: string) : t =
          let dstr = Str.slice ~start:0 ~stop:10 date_string in
          let tstr = Str.slice ~start:11 date_string in
          let y, m, d =
            match parse_isoformat_date dstr with
            | date_components -> date_components
            | exception Exn.ValueError _ -> raise (Exn.ValueError ("Invalid isoformat string: "^date_string))
          in
          let hour, minute, second, microsecond, tzinfo =
            if Str.bool tstr then
              match parse_isoformat_time tstr with
              | time_components -> time_components
              | exception Exn.ValueError _ -> raise (Exn.ValueError ("Invalid isoformat string: "^date_string))
            else
              0, 0, 0, 0, None
          in
          make y m d ~hour ~minute ~second ~microsecond ?tzinfo ()

        let dst (self: t) : Timedelta.t option =
          match self.tzinfo with
          | None -> None
          | Some tzinfo ->
            let offset = tzinfo.dst (Some self) in
            let () = check_utc_offset "dst" offset in
            offset

        let timetuple (self: t) : Time_.struct_time =
          let dst = dst self in
          let dst =
            match dst with
            | None -> ~-1
            | Some dst when Timedelta.bool dst -> 1
            | Some _ -> ~-1
          in        
          build_struct_time self.year self.month self.day self.hour self.minute self.second dst

        let mktime (self: t) : int =
          let epoch = make 1970 1 1 () in
          let max_fold_seconds = 24 * 3600 in
          let t = Timedelta.floordiv_t (sub self epoch) (Timedelta.make ~seconds:1 ()) in
          let local u =
            let tm = Time_.localtime (float_of_int u) in
            Timedelta.floordiv_t
              (sub (make tm.tm_year tm.tm_mon tm.tm_mday ~hour:tm.tm_hour ~minute:tm.tm_min ~second:tm.tm_sec ()) epoch)
              (Timedelta.make ~seconds:1 ())
          in
          let a = local t - t in
          let u1 = t - a in
          let t1 = local u1 in
          let b, return =
            if t1 = t then
              let u2 = u1 + if self.fold = 0 then ~- max_fold_seconds else max_fold_seconds in
              let b = local u2 - u2 in
              if a = b then
                b, Some u1
              else
                b, None
            else
              let b = t1 - u1 in
              let () = assert (a <> b) in
              b, None
          in
          match return with
          | Some return -> return
          | None ->
            let u2 = t - b in
            let t2 = local u2 in
            if t2 = t then
              u2
            else if t1 = t then
              u1
            else
              (if self.fold = 0 then max else min) u1 u2

        let _EPOCH = make 1970 1 1 ~tzinfo:utc_timezone ()

        let timestamp (self: t) : float =
          match self.tzinfo with
          | None ->
            let s = mktime self |> float_of_int in
            s +. (float_of_int self.microsecond) /. 1e6
          | Some _ -> Timedelta.total_seconds (sub self _EPOCH)

        let utctimetuple (self: t) : Time_.struct_time =
          let offset = utcoffset self in
          let self =
            match offset with
            | Some offset when Timedelta.bool offset -> add self (Timedelta.neg offset)
            | Some _ | None -> self
          in
          let y, m, d = self.year, self.month, self.day in
          let hh, mm, ss = self.hour, self.minute, self.second in
          build_struct_time y m d hh mm ss 0

        let date (self: t) : Date.t =
          Date.make self.year self.month self.day

        let time (self: t) : Time.t =
          Time.make ~hour:self.hour ~minute:self.minute ~second:self.second ~microsecond:self.microsecond ~fold:self.fold ()

        let timetz (self: t) : Time.t =
          Time.make ~hour:self.hour ~minute:self.minute ~second:self.second ~microsecond:self.microsecond ?tzinfo:self.tzinfo ~fold:self.fold ()

        let ctime (self: t) : string =
          let weekday = let w = self |> date |> Date.toordinal in if w <> 0 then w else 7 in
          Format.asprintf "%s %s %2d %02d:%02d:%02d %04d" _DAYNAMES.(weekday) _MONTHNAMES.(self.month) self.day self.hour self.minute self.second self.year

        let isoformat ?(sep: char = 'T') ?(timespec: string = "auto") (self: t) : string =
          let s = Format.asprintf "%04d-%02d-%02d%c%s" self.year self.month self.day sep (format_time ~timespec self.hour self.minute self.second self.microsecond) in
          let off = utcoffset self in
          let tz = format_offset off in
          if Str.bool tz then
            s ^ tz
          else
            s

        let repr (self: t) : string =
          let l = [self.year; self.month; self.day; self.hour; self.minute; self.second; self.microsecond] in
          let l =
            if Stdcompat.List.nth l (List.len l - 1) = 0 then
              List.slice ~stop:~-1 l
            else
              l
          in
          let l =
            if Stdcompat.List.nth l (List.len l - 1) = 0 then
              List.slice ~stop:~-1 l
            else
              l
          in
          let s = Format.asprintf "%s.%s(%s)" __MODULE__ "datetime" (Stdcompat.String.concat ", " (Stdcompat.List.map string_of_int l)) in
          match self.tzinfo with
          | Some tzinfo ->
            assert (Str.slice ~start:~-1 s = ")");
            Format.asprintf "%s, tzinfo=%s)" (Str.slice ~stop:~-1 s) (tzinfo.tzname None)
          | None ->
            if self.fold <> 0 then 
              let () = assert (Str.slice ~start:~-1 s = ")") in
              Format.asprintf "%s, fold=)" (Str.slice ~stop:~-1 s)
            else
              s

        let to_string (self: t) : string =
          isoformat ~sep:' ' self

        let tzname (self: t) : string option =
          match self.tzinfo with
          | None -> None
          | Some tzinfo ->
            let name = tzinfo.tzname (Some self) in
            Some name

        let hash (self: t) : int =
          if self.hashcode = ~-1 then
            begin
              let t =
                if self.fold = 1 then
                  replace ~fold:0 self
                else
                  self
              in
              let tzoff = utcoffset t in
              match tzoff with
              | None -> self.hashcode <- Stdcompat.Hashtbl.hash (self.year, self.month, self.day, self.hour, self.minute, self.second, self.microsecond)
              | Some tzoff ->
                let days = ymd2ord self.year self.month self.day in
                let seconds = self.hour * 3600 + self.minute * 60 + self.second in
                self.hashcode <- Stdcompat.Hashtbl.hash (Timedelta.sub (Timedelta.make ~days ~seconds ~microseconds:self.microsecond ()) tzoff)
            end;
          self.hashcode

        let min = make 1 1 1 ()
        let max = make 9999 12 31 ~hour:23 ~minute:59 ~second:59 ~microsecond:999999 ()
        let resolution = Timedelta.make ~microseconds:1 ()

      end)
  end)

module Timezone =
  (struct
    type datetime = TimeDatetimeTZ.datetime
    type tzinfo = TimeDatetimeTZ.tzinfo = {
      tzname: datetime option -> string;
      utcoffset: datetime option -> Timedelta.t;
      dst: datetime option -> Timedelta.t option;
      fromutc: datetime -> datetime;
      repr: string;
    }

    type t = {
      offset: Timedelta.t;
      name: string option;
    }

    let maxoffset = Timedelta.make ~hours:24 ~microseconds:~-1 ()
    let minoffset = Timedelta.neg maxoffset

    let make ?(name:string option) (offset: Timedelta.t) : t =
      if not Timedelta.Cmp.(minoffset <= offset && offset <= maxoffset) then
        raise (Exn.ValueError "");
      {
        offset;
        name;
      }

    let utc = make (Timedelta.make ())

    let eq (self: t) (other: t) : bool =
      Timedelta.eq self.offset other.offset

    let hash (self: t) : int =
      Timedelta.hash self.offset

    let repr (self: t) : string =
      if self == utc then
        "Datetime.Timezone.utc"
      else
        match self.name with
        | None -> Format.asprintf "Datetime.Timezone(%s)" (Timedelta.repr self.offset)
        | Some name -> Format.asprintf "Datetime.Timezone(%s, %s)" (Timedelta.repr self.offset) name

    let dst (_: t) (_: datetime option) : Timedelta.t option =
      None

    let fromutc (self: t) (dt: datetime) : datetime =
      TimeDatetimeTZ.Datetime.add dt self.offset

    let name_from_offset (delta: Timedelta.t) : string =
      if delta |> Timedelta.bool |> not then
        "UTC"
      else
        let sign, delta =
          if Timedelta.lt delta (Timedelta.make ()) then
            "-", Timedelta.neg delta
          else
            "+", delta
        in
        let hours, rest = Timedelta.divmod delta (Timedelta.make ~hours:1 ()) in
        let minutes, rest = Timedelta.divmod rest (Timedelta.make ~minutes:1 ()) in
        let seconds = rest.seconds in
        let microseconds = rest.microseconds in
        if microseconds <> 0 then
          Format.asprintf "UTC%s%02d:%02d:%02d.%06d" sign hours minutes seconds microseconds
        else if seconds <> 0 then
          Format.asprintf "UTC%s%02d:%02d:%02d" sign hours minutes seconds
        else
          Format.asprintf "UTC%s%02d:%02d" sign hours minutes


    let tzname (self: t) (_: datetime option) : string =
      match self.name with
      | None -> name_from_offset self.offset
      | Some name -> name

    let to_string (self: t) : string =
      tzname self None

    let utcoffset (self: t) (_: datetime option) : Timedelta.t =
      self.offset

    let to_tzinfo (self: t) : tzinfo =
      {
        tzname: datetime option -> string = tzname self;
        utcoffset: datetime option -> Timedelta.t = utcoffset self;
        dst: datetime option -> Timedelta.t option = dst self;
        fromutc: datetime -> datetime = fromutc self;
        repr: string = repr self;
      }

    let make_tzinfo ?(name:string option) (offset: Timedelta.t) : tzinfo =
      make ?name offset |> to_tzinfo
  end)

let utc = Timezone.(to_tzinfo utc)

type datetime = TimeDatetimeTZ.datetime
type tzinfo = TimeDatetimeTZ.tzinfo = {
  tzname: datetime option -> string;
  utcoffset: datetime option -> Timedelta.t;
  dst: datetime option -> Timedelta.t option;
  fromutc: datetime -> datetime;
  repr: string;
}
module Time = TimeDatetimeTZ.Time
module Datetime = TimeDatetimeTZ.Datetime
