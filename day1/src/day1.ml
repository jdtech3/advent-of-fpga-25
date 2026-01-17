open! Core
open! Hardcaml
open! Signal

(* Bit widths *)
let in_bits = 11        (* max: 1024 (1 bit for L/R) *)
let out_bits = 16       (* max: 65536 *)
let _counter_bits = 16  (* -32768 ~ 32767 *)

(* In *)
module I = struct
  type 'a t = {
    clk : 'a;
    rst : 'a;
    start : 'a;
    data_in : 'a With_valid.t [@bits in_bits];
  }
  [@@deriving hardcaml]
end

(* Out *)
module O = struct
  type 'a t = {
    data_out : 'a With_valid.t [@bits out_bits];
  }
  [@@deriving hardcaml]
end

(* FSM *)
module States = struct
  type t = IDLE | INPUT | DIVIDE | OUTPUT
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clk; rst; start; data_in } : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock:clk ~reset:rst () in
  let open Always in  (* NOTE: this temporarily pulls module into local scope *)
  let fsm = State_machine.create (module States) spec in

  (* Track the current dial position with signed int *)
  let%hw_var rot = Variable.reg spec ~width:_counter_bits in
  let%hw_var direction = Variable.reg spec ~width:1 in
  let%hw_var value = Variable.reg spec ~width:(in_bits-1) in
  let%hw_var div_q = Variable.reg spec ~width:9 in            (* size can be optimized later *)
  let%hw_var div_r = Variable.reg spec ~width:out_bits in
  let%hw_var div_i = Variable.reg spec ~width:out_bits in
  
  let%hw_var ans = Variable.reg spec ~width:out_bits in
  let ans_valid = Variable.wire ~default:gnd () in
  
  (* let mux9 ~sel ~a ~b ~c ~d ~e ~f ~g ~h ~i = mux sel [a; b; c; d; e; f; g; h; i] in *)

  compile [
    fsm.switch [
      (IDLE, [
        when_ start [
          rot <-- zero _counter_bits;
          ans <-- zero out_bits;
          ans_valid <-- gnd;
          fsm.set_next INPUT;
        ]
      ]);
      (INPUT, [
        when_ data_in.valid [
          (* Clear division regs *)
          div_q <-- zero 9;
          div_r <-- zero out_bits;
          div_i <--. 9;   (* -1 for L/R bit, -1 because algo *)

          (* Capture input *)
          direction <-- data_in.value.:[10, 10];
          value <-- data_in.value.:[9, 0];

          fsm.set_next DIVIDE;
        ]
      ]);
      (DIVIDE, [
        (* Implements div_r <<= 1 and div_r[0] = value[i] *)
        div_r <-- (sll div_r.value ~by:1 |: (zero (out_bits-(in_bits-1)) @: ((log_shift value.value ~f:srl ~by:div_i.value) &:. 1)));
        when_ (div_r.value >=:. 100) [
          div_r <-- div_r.value -:. 100;
          div_q <-- (div_q.value |: (log_shift (zero 9 |:. 1) ~f:sll ~by:div_i.value))  (* Implements div_q[i] = 1 *)
        ];
        when_ (div_i.value ==:. 0) [
          rot <-- mux2 (direction.value)
                       (mux2 (div_r.value >: rot.value)          ((rot.value +:. 100) -: div_r.value) (rot.value -: div_r.value))
                       (mux2 ((div_r.value +: rot.value) >:. 99) ((rot.value +: div_r.value) -:. 100) (rot.value +: div_r.value));
          fsm.set_next OUTPUT
        ];
        div_i <-- div_i.value -:. 1;
      ]);
      (OUTPUT, [
        when_ (rot.value ==:. 0) [
          ans <-- ans.value +:. 1;
        ];
        ans_valid <-- vdd;
        fsm.set_next INPUT;
      ]);
    ]
  ];
  {
    data_out = { value = ans.value; valid = ans_valid.value}
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day1" create
;;
