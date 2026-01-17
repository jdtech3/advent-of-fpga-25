open! Core
open! Hardcaml

(* Bit widths *)
val in_bits : int
val out_bits : int

(* In *)
module I : sig
  type 'a t = {
    clk : 'a;
    rst : 'a;
    start : 'a;
    data_in : 'a With_valid.t;
  }
  [@@deriving hardcaml]
end

(* Out *)
module O : sig
  type 'a t = {
    data_out : 'a With_valid.t;
  }
  [@@deriving hardcaml]
end

val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
