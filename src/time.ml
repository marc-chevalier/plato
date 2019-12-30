type struct_time = {
  tm_sec : int;
  tm_min : int;
  tm_hour : int;
  tm_mday : int;
  tm_mon : int;
  tm_year : int;
  tm_wday : int;
  tm_yday : int;
  tm_isdst : int;
}

let st_of_tm (t: Unix.tm) : struct_time =
  {
    tm_sec = t.tm_sec;
    tm_min = t.tm_min;
    tm_hour = t.tm_hour;
    tm_mday = t.tm_mday;
    tm_mon = t.tm_mon;
    tm_year = t.tm_year;
    tm_wday = t.tm_wday;
    tm_yday = t.tm_yday;
    tm_isdst = if t.tm_isdst then 1 else 0;
  }

let gmttime (f: float) : struct_time = f |> Unix.gmtime |> st_of_tm
let localtime (f: float) : struct_time = f |> Unix.localtime |> st_of_tm
let time = Unix.time
