type log_level =
  [ `Error
  | `Warning
  | `Info
  | `Debug
  ]

let to_logs_level (l : log_level) =
  match l with
  | `Error -> Logs.Error
  | `Warning -> Logs.Warning
  | `Info -> Logs.Info
  | `Debug -> Logs.Debug

let reporter ~now () =
  let buffer = Buffer.create 512 in
  let formatter = Fmt.with_buffer ~like:Fmt.stderr buffer in

  let flush () =
    let message = Buffer.contents buffer in
    Buffer.reset buffer;
    message
  in

  let report src level ~over k msgf =
    let k _ =
      let message = flush () in
      prerr_string message;
      Stdlib.flush stderr;
      over ();
      k ()
    in

    let level_style, level =
      match level with
      | Logs.App -> (`White, "     ")
      | Logs.Debug -> (`Blue, "DEBUG")
      | Logs.Error -> (`Red, "ERROR")
      | Logs.Warning -> (`Yellow, " WARN")
      | Logs.Info -> (`Green, " INFO")
    in

    let timestamp =
      let ptime = Option.get (Ptime.of_float_s (now ())) in
      let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ptime in
      let float_time = Ptime.to_float_s ptime in
      let fractional = mod_float float_time 1.0 in
      let fractional_us = fractional *. 1e6 in
      Printf.sprintf "%02d.%02d.%02d %02d:%02d:%02d.%06.0f" d m (y mod 100) hh mm ss fractional_us
    in

    let src =
      let width = 20 in
      let s = Logs.Src.name src in
      if s = Logs.Src.name Logs.default then String.make width ' '
      else
        let length = String.length s in
        if length > width then String.sub s (length - width) width
        else String.make (width - length) ' ' ^ s
    in

    msgf @@ fun ?header:_ ?tags:_ fmt ->
    Format.kfprintf k formatter
      ("%a %a %a @[" ^^ fmt ^^ "@]@.")
      Fmt.(styled `Faint string)
      timestamp
      Fmt.(styled `White string)
      src
      Fmt.(styled level_style string)
      level
  in

  { Logs.report }

let level = ref Logs.Debug
let sources : (string * Logs.src) list ref = ref []

let app_log format_and_arguments =
  Fmt.kstr (fun message -> Logs.app (fun log -> log "%s" message)) format_and_arguments

type ('a, 'b) conditional_log = ((('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b) -> unit

type sub_log = {
  error : 'a. ('a, unit) conditional_log;
  warn : 'a. ('a, unit) conditional_log;
  debug : 'a. ('a, unit) conditional_log;
  info : 'a. ('a, unit) conditional_log;
}

let sub_log ?level:level_ name =
  let wrap (log : _ Logs.log) k = log (fun log -> k (fun m -> log m)) in

  let level = List.find Option.is_some [ Option.map to_logs_level level_; Some !level ] in
  let src = Logs.Src.create name in
  let (module Log) = Logs.src_log src in
  Logs.Src.set_level src level;
  {
    error = (fun m -> wrap Log.err m);
    warn = (fun m -> wrap Log.warn m);
    debug = (fun m -> wrap Log.debug m);
    info = (fun m -> wrap Log.info m);
  }

let init () =
  let now () = Ptime.to_float_s (Ptime_clock.now ()) in
  Logs.set_reporter (reporter ~now ());
  Logs.set_level (Some !level)
