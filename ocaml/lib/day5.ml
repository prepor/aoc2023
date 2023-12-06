open Core

let pp = Stdio.print_endline

let example =
  {|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
|}

(* Input parsing *)

type range = { target : int; start_map : int; r : int }
[@@deriving show { with_path = false }]

type map = { source_cat : string; dest_cat : string; ranges : range list }
[@@deriving show { with_path = false }]

type input = { seeds : (int * int) list; maps : map list }
[@@deriving show { with_path = false }]

let parse input =
  let open Angstrom in
  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string
  in
  let ws =
    skip_while (function
      | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
      | _ -> false)
  in
  let alphas = take_while1 (function 'a' .. 'z' -> true | _ -> false) in
  let range =
    let+ _ = ws
    and+ target = integer
    and+ _ = ws
    and+ start_map = integer
    and+ _ = ws
    and+ r = integer in
    { target; start_map; r }
  in
  let map =
    let+ _ = ws
    and+ source_cat = alphas
    and+ _ = string "-to-"
    and+ dest_cat = alphas
    and+ _ = string " map:"
    and+ ranges = many range in
    { source_cat; dest_cat; ranges }
  in
  let parser =
    let+ _ = string "seeds: "
    and+ seeds =
      sep_by ws
        (lift2 (fun start len -> (start, start + len)) (integer <* ws) integer)
    and+ maps = many map in
    { seeds; maps }
  in
  parse_string ~consume:Prefix parser input |> Result.ok_or_failwith

let%expect_test _ =
  parse example |> show_input |> pp;
  [%expect
    {|
    { seeds = [(79, 93); (55, 68)];
      maps =
      [{ source_cat = "seed"; dest_cat = "soil";
         ranges =
         [{ target = 50; start_map = 98; r = 2 };
          { target = 52; start_map = 50; r = 48 }]
         };
       { source_cat = "soil"; dest_cat = "fertilizer";
         ranges =
         [{ target = 0; start_map = 15; r = 37 };
          { target = 37; start_map = 52; r = 2 };
          { target = 39; start_map = 0; r = 15 }]
         };
       { source_cat = "fertilizer"; dest_cat = "water";
         ranges =
         [{ target = 49; start_map = 53; r = 8 };
          { target = 0; start_map = 11; r = 42 };
          { target = 42; start_map = 0; r = 7 };
          { target = 57; start_map = 7; r = 4 }]
         };
       { source_cat = "water"; dest_cat = "light";
         ranges =
         [{ target = 88; start_map = 18; r = 7 };
          { target = 18; start_map = 25; r = 70 }]
         };
       { source_cat = "light"; dest_cat = "temperature";
         ranges =
         [{ target = 45; start_map = 77; r = 23 };
          { target = 81; start_map = 45; r = 19 };
          { target = 68; start_map = 64; r = 13 }]
         };
       { source_cat = "temperature"; dest_cat = "humidity";
         ranges =
         [{ target = 0; start_map = 69; r = 1 };
          { target = 1; start_map = 0; r = 69 }]
         };
       { source_cat = "humidity"; dest_cat = "location";
         ranges =
         [{ target = 60; start_map = 56; r = 37 };
          { target = 56; start_map = 93; r = 4 }]
         }]
      } |}]

(* Solution *)

let part2 { seeds; maps } =
  let locations = ref [] in
  List.iter seeds ~f:(fun seed ->
      let ranges = ref [ seed ] in
      let results = ref [] in
      List.iter maps ~f:(fun map ->
          while not (List.is_empty !ranges) do
            let start_range', end_range' = List.hd_exn !ranges in
            ranges := List.tl_exn !ranges;
            let start_range = ref start_range' in
            let end_range = ref end_range' in
            let no_overlap = ref true in
            List.iter map.ranges ~f:(fun { target; start_map; r } ->
                let end_map = start_map + r in
                let offset = target - start_map in

                if end_map <= !start_range || !end_range <= start_map then ()
                else (
                  no_overlap := false;
                  if !start_range < start_map then (
                    ranges := (!start_range, start_map) :: !ranges;
                    start_range := start_map);
                  if end_map < !end_range then (
                    ranges := (end_map, !end_range) :: !ranges;
                    end_range := end_map);
                  results :=
                    (!start_range + offset, !end_range + offset) :: !results));
            if !no_overlap then
              results := (!start_range, !end_range) :: !results
          done;
          ranges := !results;
          results := []);
      locations := !ranges @ !locations);
  !locations
  |> List.map ~f:(fun (a, _) -> a)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn

let time f =
  let open Core in
  let start = Time.now () in
  let x = f () in
  let stop = Time.now () in
  printf "Time: %F ms\n" (Time.diff stop start |> Time.Span.to_ms);
  x

let%expect_test _ =
  let open Stdio in
  parse example |> part2 |> Stdio.printf "%d";
  [%expect {|
    46 |}]

let%expect_test _ =
  let open Stdio in
  time (fun () ->
      In_channel.with_file "/Users/prepor/Dropbox/lab/aoc2023/files/input5.txt"
        ~f:(fun f -> In_channel.input_all f))
  |> parse |> part2 |> Stdio.printf "%d";
  [%expect {|
    Time: 0.0410079956055 ms
    137516820 |}]
