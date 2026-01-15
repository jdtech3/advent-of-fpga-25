# Day 1

## Notes

My first thought for "realistic I/O" was UART, but for simplicity sake, I decided to just use the same interface as the template: parallel I/O with minimal control signals.

Mostly to learn OCaml by doing, but also to determine the min. input bit width, I wrote util/input_size.ml script, which dumps the bit width required given an input file.

## Interface
