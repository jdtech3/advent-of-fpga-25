open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
open! Re
module Day1 = Aoc_day1.Day1
module Harness = Cyclesim_harness.Make (Day1.I) (Day1.O)

let ( <--. ) = Bits.( <--. )
let sample_input = [ "R500"; "L1"; "R200" ]

let parse_input s =
  let direction = if (phys_equal (String.slice s 0 1) "L") then 0 else 1 in
  let value = int_of_string (String.slice s 1 (String.length s)) in
  (direction lsl 10) lor value
;;

let simple_testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in

  (* Helper function for inputting one value *)
  let feed_input x =
    inputs.data_in.value <--. x;
    inputs.data_in.valid := Bits.vdd;
    (* cycle (); *)
    (* inputs.data_in.valid := Bits.gnd;
    cycle (); *)
    (* Wait for result to become valid *)
    while not (Bits.to_bool !(outputs.data_out.valid)) do
      cycle ()
    done;
    inputs.data_in.valid := Bits.gnd;
  in
  
  (* Reset the design *)
  inputs.rst := Bits.vdd;
  cycle ();
  inputs.rst := Bits.gnd;
  cycle ();

  (* Pulse the start signal *)
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;

  (* Input some data *)
  List.iter sample_input ~f:(fun s -> feed_input (parse_input s));
  let answer = Bits.to_unsigned_int !(outputs.data_out.value) in
  print_s [%message "Answer" (answer : int)];
  (* Show in the waveform that [valid] stays high. *)
  cycle ~n:2 ()
;;

(* The [waves_config] argument to [Harness.run] determines where and how to save waveforms
   for viewing later with a waveform viewer. The commented examples below show how to save
   a waveterm file or a VCD file. *)
(* let waves_config = Waves_config.no_waves *)


let waves_config =
  Waves_config.to_directory "/tmp/"
|> Waves_config.as_wavefile_format ~format:Hardcamlwaveform
;;

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(* |> Waves_config.as_wavefile_format ~format:Vcd *)
(* ;; *)

let%expect_test "Simple test, optionally saving waveforms to disk" =
  Harness.run_advanced ~waves_config ~create:Day1.hierarchical simple_testbench;
  [%expect {| (Result (data_out 2)) |}]
;;

let%expect_test "Simple test with printing waveforms directly" =
  (* For simple tests, we can print the waveforms directly in an expect-test (and use the
     command [dune promote] to update it after the tests run). This is useful for quickly
     visualizing or documenting a simple circuit, but limits the amount of data that can
     be shown. *)
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.compile (Re.Glob.glob "day1*"))
    ]
  in
  Harness.run_advanced
    ~create:Day1.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
          (* [display_rules] is optional, if not specified, it will print all named
             signals in the design. *)
        ~signals_width:30
        ~display_width:92
        ~wave_width:1
        (* [wave_width] configures how many chars wide each clock cycle is *)
        waves)
    simple_testbench;
  [%expect
    {|
    (Result (range 146))
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────┐
    │range_finder$i$clear        ││────┐                                                       │
    │                            ││    └───────────────────────────────────────────────────────│
    │range_finder$i$clock        ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │                            ││────────────┬───────┬───────┬───────┬───────────────────────│
    │range_finder$i$data_in      ││ 0          │16     │67     │150    │4                      │
    │                            ││────────────┴───────┴───────┴───────┴───────────────────────│
    │range_finder$i$data_in_valid││            ┌───┐   ┌───┐   ┌───┐   ┌───┐                   │
    │                            ││────────────┘   └───┘   └───┘   └───┘   └───────────────────│
    │range_finder$i$finish       ││                                            ┌───┐           │
    │                            ││────────────────────────────────────────────┘   └───────────│
    │range_finder$i$start        ││        ┌───┐                                               │
    │                            ││────────┘   └───────────────────────────────────────────────│
    │                            ││────────────────┬───────┬───────┬───────────────────────────│
    │range_finder$max            ││ 0              │16     │67     │150                        │
    │                            ││────────────────┴───────┴───────┴───────────────────────────│
    │                            ││────────────┬───┬───────────────────────┬───────────────────│
    │range_finder$min            ││ 0          │65.│16                     │4                  │
    │                            ││────────────┴───┴───────────────────────┴───────────────────│
    │range_finder$o$range$valid  ││                                                ┌───────────│
    │                            ││────────────────────────────────────────────────┘           │
    │                            ││────────────────────────────────────────────────┬───────────│
    │range_finder$o$range$value  ││ 0                                              │146        │
    │                            ││────────────────────────────────────────────────┴───────────│
    └────────────────────────────┘└────────────────────────────────────────────────────────────┘
    |}]
;;
