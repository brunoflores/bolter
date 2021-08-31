type bit =
  | B0
  | B1

let bin_char = function
  | '0' -> B0
  | '1' -> B1
  | _ -> failwith "Invalid binary character"

let hex_char = function
  | '0' -> [B0; B0; B0; B0]
  | '1' -> [B0; B0; B0; B1]
  | '2' -> [B0; B0; B1; B0]
  | '3' -> [B0; B0; B1; B1]
  | '4' -> [B0; B1; B0; B0]
  | '5' -> [B0; B1; B0; B1]
  | '6' -> [B0; B1; B1; B0]
  | '7' -> [B0; B1; B1; B1]
  | '8' -> [B1; B0; B0; B0]
  | '9' -> [B1; B0; B0; B1]
  | 'A' | 'a' -> [B1; B0; B1; B0]
  | 'B' | 'b' -> [B1; B0; B1; B1]
  | 'C' | 'c' -> [B1; B1; B0; B0]
  | 'D' | 'd' -> [B1; B1; B0; B1]
  | 'E' | 'e' -> [B1; B1; B1; B0]
  | 'F' | 'f' -> [B1; B1; B1; B1]
  | _ -> failwith "Invalid hex character"

let string_of_hex = function
  | [B0; B0; B0; B0] -> "0"
  | [B0; B0; B0; B1] -> "1"
  | [B0; B0; B1; B0] -> "2"
  | [B0; B0; B1; B1] -> "3"
  | [B0; B1; B0; B0] -> "4"
  | [B0; B1; B0; B1] -> "5"
  | [B0; B1; B1; B0] -> "6"
  | [B0; B1; B1; B1] -> "7"
  | [B1; B0; B0; B0] -> "8"
  | [B1; B0; B0; B1] -> "9"
  | [B1; B0; B1; B0] -> "A"
  | [B1; B0; B1; B1] -> "B"
  | [B1; B1; B0; B0] -> "C"
  | [B1; B1; B0; B1] -> "D"
  | [B1; B1; B1; B0] -> "E"
  | [B1; B1; B1; B1] -> "F"
  | _ -> failwith "Cannot convert binary sequence to hex"

type iop =
  | ADDI
  | SLTI
  | SLTIU
  | ANDI
  | ORI
  | XORI

let string_of_op = function
  | ADDI -> "addi"
  | SLTI -> "slti"
  | SLTIU -> "sltiu"
  | XORI -> "xori"
  | ORI -> "ori"
  | ANDI -> "andi"

type bits = bit list
type regidx = bits

type ast =
  | ITYPE of (bits * regidx * regidx * iop)

type retired =
  | RETIRE_SUCCESS
  | RETIRE_FAIL

let float_of_bit = function
  | B0 -> 0.
  | B1 -> 1.

let uint (bs : bit list) =
  let uint_bit b (n, pos) =
    n +. ((2. ** pos) *. (float_of_bit b)), pos +. 1.
  in
  int_of_float (fst (List.fold_right uint_bit bs (0., 0.)))

let rec undefined_vector (len, item) =
  if len == 0
  then []
  else item :: undefined_vector (len - 1, item)

let undefined_bitvector len =
  if len == 0
  then []
  else B0 :: undefined_vector (len -1, B0)

let rec drop n xs =
  match n, xs with
  | 0, xs -> xs
  | _, [] -> []
  | n, (_ :: xs) -> drop (n -1) xs

let rec take n xs =
  match n, xs with
  | 0, _ -> []
  | n, (x :: xs) -> x :: take (n - 1) xs
  | _, [] -> []

let count_leading_zeros xs =
  let rec aux bs acc = match bs with
    | (B0 :: bs') -> aux bs' (acc + 1)
    | _ -> acc
  in
  aux xs 0

let rec replicate_bits (bits, n) =
  if n <= 0
  then []
  else bits @ replicate_bits (bits, n - 1)

let zero_extend (n, vec) =
  if n <= List.length vec
  then take n vec
  else replicate_bits ([B0], n - List.length vec) @ vec

let x1 : regidx ref = ref (undefined_bitvector 32)
let x2 : regidx ref = ref (undefined_bitvector 32)
let x3 : regidx ref = ref (undefined_bitvector 32)
let x4 : regidx ref = ref (undefined_bitvector 32)
let x5 : regidx ref = ref (undefined_bitvector 32)
let x6 : regidx ref = ref (undefined_bitvector 32)
let x7 : regidx ref = ref (undefined_bitvector 32)
let x8 : regidx ref = ref (undefined_bitvector 32)
let x9 : regidx ref = ref (undefined_bitvector 32)
let x10 : regidx ref = ref (undefined_bitvector 32)
let x11 : regidx ref = ref (undefined_bitvector 32)
let x12 : regidx ref = ref (undefined_bitvector 32)
let x13 : regidx ref = ref (undefined_bitvector 32)
let x14 : regidx ref = ref (undefined_bitvector 32)
let x15 : regidx ref = ref (undefined_bitvector 32)
let x16 : regidx ref = ref (undefined_bitvector 32)
let x17 : regidx ref = ref (undefined_bitvector 32)
let x18 : regidx ref = ref (undefined_bitvector 32)
let x19 : regidx ref = ref (undefined_bitvector 32)
let x20 : regidx ref = ref (undefined_bitvector 32)
let x21 : regidx ref = ref (undefined_bitvector 32)
let x22 : regidx ref = ref (undefined_bitvector 32)
let x23 : regidx ref = ref (undefined_bitvector 32)
let x24 : regidx ref = ref (undefined_bitvector 32)
let x25 : regidx ref = ref (undefined_bitvector 32)
let x26 : regidx ref = ref (undefined_bitvector 32)
let x27 : regidx ref = ref (undefined_bitvector 32)
let x28 : regidx ref = ref (undefined_bitvector 32)
let x29 : regidx ref = ref (undefined_bitvector 32)
let x30 : regidx ref = ref (undefined_bitvector 32)
let x31 : regidx ref = ref (undefined_bitvector 32)

let zero_reg = zero_extend (32, [B0; B0; B0; B0])

let rX = function
  | 0 -> zero_reg
  | 1 -> !x1
  | 2 -> !x2
  | 3 -> !x3
  | 4 -> !x4
  | 5 -> !x5
  | 6 -> !x6
  | 7 -> !x7
  | 8 -> !x8
  | 9 -> !x9
  | 10 -> !x10
  | 11 -> !x11
  | 12 -> !x12
  | 13 -> !x13
  | 14 -> !x14
  | 15 -> !x15
  | 16 -> !x16
  | 17 -> !x17
  | 18 -> !x18
  | 19 -> !x19
  | 20 -> !x20
  | 21 -> !x21
  | 22 -> !x22
  | 23 -> !x23
  | 24 -> !x24
  | 25 -> !x25
  | 26 -> !x26
  | 27 -> !x27
  | 28 -> !x28
  | 29 -> !x29
  | 30 -> !x30
  | 31 -> !x31
  | _ -> failwith "Invalid register"

let wX r data =
  match r with
  | 0 -> ()
  | 1 -> x1 := data
  | 2 -> x2 := data
  | 3 -> x3 := data
  | 4 -> x4 := data
  | 5 -> x5 := data
  | 6 -> x6 := data
  | 7 -> x7 := data
  | 8 -> x8 := data
  | 9 -> x9 := data
  | 10 -> x10 := data
  | 11 -> x11 := data
  | 12 -> x12 := data
  | 13 -> x13 := data
  | 14 -> x14 := data
  | 15 -> x15 := data
  | 16 -> x16 := data
  | 17 -> x17 := data
  | 18 -> x18 := data
  | 19 -> x19 := data
  | 20 -> x20 := data
  | 21 -> x21 := data
  | 22 -> x22 := data
  | 23 -> x23 := data
  | 24 -> x24 := data
  | 25 -> x25 := data
  | 26 -> x26 := data
  | 27 -> x27 := data
  | 28 -> x28 := data
  | 29 -> x29 := data
  | 30 -> x30 := data
  | 31 -> x31 := data
  | _ -> failwith "Invalid register"

(*let wX*)

let rX_bits i = rX (uint i)
let wX_bits i data = wX (uint i) data

let execute = function
  | ITYPE (_) -> RETIRE_SUCCESS

let () = print_endline "Hello, World!"
