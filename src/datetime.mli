module Time_ = Time

val _MINYEAR : int
val _MAXYEAR : int

module Timedelta :
sig
  type t
  val make: ?days:int -> ?seconds:int -> ?microseconds:int ->
    ?milliseconds:int -> ?minutes:int -> ?hours:int ->
    ?weeks:int -> unit -> t

  val min: t
  val max: t
  val resolution: t

  val total_seconds: t -> float

  val days: t -> int
  val seconds: t -> int
  val microseconds: t -> int
  val to_microseconds: t -> int

  val add: t -> t -> t
  val sub: t -> t -> t
  val neg: t -> t
  val abs: t -> t
  val mul: t -> int -> t
  val floordiv_t: t -> t -> int
  val floordiv_int: t -> int -> t
  val truediv_t: t -> t -> float
  val truediv_int: t -> int -> t
  val modulo: t -> t -> t
  val divmod: t -> t -> int * t

  val compare: t -> t -> int
  val eq: t -> t -> bool
  val le: t -> t -> bool
  val lt: t -> t -> bool
  val ge: t -> t -> bool
  val gt: t -> t -> bool

  val hash: t -> int
  val bool: t -> bool

  val repr: t -> string
  val to_string: t -> string
end

module Date:
sig
  type t
  val make: int -> int -> int -> t

  val fromtimestamp: float -> t
  val today: unit -> t
  val fromordinal: int -> t
  val fromisoformat: string -> t
  val fromisocalendar: int -> int -> int -> t
  val repr: t -> string
  val toordinal: t -> int

  val ctime: t -> string
  val isoformat: t -> string
  val str: t -> string

  val year: t -> int
  val month: t -> int
  val day: t -> int
  val timetuple: t -> Time.struct_time
  val replace: ?year:int -> ?month:int -> ?day:int -> t -> t

  val compare: t -> t -> int
  val eq: t -> t -> bool
  val le: t -> t -> bool
  val lt: t -> t -> bool
  val ge: t -> t -> bool
  val gt: t -> t -> bool
  val hash: t -> int

  val add: t -> Timedelta.t -> t
  val sub: t -> t -> Timedelta.t
  val sub_delta: t -> Timedelta.t -> t
  val weekday: t -> int
  val isoweekday: t -> int
  val isocalendar: t -> int * int * int

  val min: t
  val max: t
  val resolution: Timedelta.t
end

type datetime
type tzinfo = {
  tzname: datetime option -> string;
  utcoffset: datetime option -> Timedelta.t;
  dst: datetime option -> Timedelta.t option;
  fromutc: datetime -> datetime;
  repr: string;
}

module Time:
sig
  type t

  val make: ?hour:int -> ?minute:int -> ?second:int -> ?microsecond:int -> ?tzinfo:tzinfo -> ?fold:int -> unit -> t
  val hour: t -> int
  val minute: t -> int
  val second: t -> int
  val microsecond: t -> int
  val tzinfo: t -> tzinfo option
  val fold: t -> int

  val utcoffset: t -> Timedelta.t option

  val compare: ?allow_mixed:bool -> t -> t -> int
  val eq: t -> t -> bool
  val le: t -> t -> bool
  val lt: t -> t -> bool
  val ge: t -> t -> bool
  val gt: t -> t -> bool
  val replace: ?hour:int -> ?minute:int -> ?second:int ->
    ?microsecond:int -> ?tzinfo:bool -> ?fold:int -> t -> t

  val hash: t -> int
  val repr: t -> string
  val isoformat: ?timespec:string -> t -> string
  val to_string: t -> string

  val tzname: t -> string option

  val dst: t -> Timedelta.t option 

  val min: t
  val max: t
  val resolution: Timedelta.t

end

module Datetime:
sig
  type t = datetime

  val make: int -> int -> int ->
    ?hour:int -> ?minute:int -> ?second:int ->
    ?microsecond:int -> ?tzinfo:tzinfo -> ?fold:int -> unit -> t

  val hour: t -> int
  val minute: t -> int
  val second: t -> int
  val microsecond: t -> int
  val tzinfo: t -> tzinfo option
  val fold: t -> int

  val utcoffset: t -> Timedelta.t option
  val toordinal: t -> int

  val sub: t -> t -> Timedelta.t

  val replace: ?year:int -> ?month:int -> ?day:int ->
    ?hour:int -> ?minute:int -> ?second:int ->
    ?microsecond:int -> ?tzinfo:tzinfo -> ?fold:int ->
    t -> t

  val cmp: ?allow_mixed:bool -> t -> t -> int

  val compare: t -> t -> int

  val eq: t -> t -> bool
  val le: t -> t -> bool
  val lt: t -> t -> bool
  val ge: t -> t -> bool
  val gt: t -> t -> bool

  val fromtimestamp: ?tz:tzinfo -> float -> t
  val utcfromtimestamp: float -> t
  val now: ?tz:tzinfo -> unit -> t
  val utcnow: unit -> t
  val combine: ?tzinfo:tzinfo -> Date.t -> Time.t -> t
  val add: t -> Timedelta.t -> t
  val make_timezone: ?name:string -> Timedelta.t -> tzinfo
  val utc_timezone: tzinfo
  val parse_isoformat_time: string -> int * int * int * int * tzinfo option
  val fromisoformat: string -> t
  val dst: t -> Timedelta.t option
  val timetuple: t -> Time_.struct_time

  val mktime: t -> int

  val _EPOCH: t
  val timestamp: t -> float
  val utctimetuple: t -> Time_.struct_time

  val date: t -> Date.t
  val time: t -> Time.t
  val timetz: t -> Time.t
  val ctime: t -> string

  val isoformat: ?sep:char -> ?timespec:string -> t -> string
  val repr: t -> string
  val to_string: t -> string
  val tzname: t -> string option

  val hash: t -> int

  val min: t
  val max: t
  val resolution: Timedelta.t
end