type 'a stream = {
  mutable buffer : int;
  mutable len : int;
  mutable index : int;
  channel : 'a;
}

exception Invalid_stream
exception End_of_stream

let invalid_stream () = raise Invalid_stream

type istream = in_channel stream

let of_in_channel (channel : in_channel) =
  let b0 = try input_byte channel with End_of_file -> invalid_stream () in
  let b1 = try input_byte channel with End_of_file -> invalid_stream () in
  let buffer, len =
    try
      let b2 = input_byte channel in
      (b0 lor (b1 lsl 8) lor (b2 lsl 16), 24)
    with End_of_file -> (b0 lor (b1 lsl 8), 16)
  in
  { buffer; len; index = 0; channel }

let read_bit (is : istream) =
  let i = is.index in
  if is.len = 24 then
    if is.index < 7 then begin
      is.index <- i + 1;
      (is.buffer lsr i) land 1
    end
    else begin
      (* i = 7 *)
      is.index <- 0;
      let res = (is.buffer lsr 7) land 1 in
      is.buffer <- (is.buffer lsr 8) land 0xffff;
      let () =
        try
          let b2 = input_byte is.channel in
          is.buffer <- is.buffer lor (b2 lsl 16)
        with End_of_file -> is.len <- 16
      in
      res
    end
  else (* len = 16 *)
    let num_bits = (is.buffer lsr 8) land 0xff in
    if num_bits >= 8 then invalid_stream ()
    else if i < num_bits then begin
      is.index <- i + 1;
      (is.buffer lsr i) land 1
    end
    else raise End_of_stream


let read_n_bits (is : istream) n =
  let rec loop i acc =
    if i < n then loop (i + 1) (acc lor (read_bit is lsl i)) else acc
  in
  if n < 0 || n > Sys.int_size then
    raise (Invalid_argument (Format.sprintf "read_n_bits _ %d" n))
  else loop 0 0

let read_byte (is : istream) = read_n_bits is 8
let read_short (is : istream) = read_n_bits is 16
let read_int (is : istream) = read_n_bits is Sys.int_size

type ostream = out_channel stream

let of_out_channel channel = { buffer = 0; len = 0; index = 0; channel }

let write_bit (oc : ostream) b =
  let b = b land 1 in
  let i = oc.index in
  if i < 0 then invalid_stream ();
  oc.buffer <- oc.buffer lor (b lsl i);
  if i < 7 then oc.index <- i + 1
  else begin
    oc.index <- 0;
    output_byte oc.channel oc.buffer;
    oc.buffer <- 0
  end


let finalize (oc : ostream) =
  output_byte oc.channel (0xff land oc.buffer);
  output_byte oc.channel oc.index;
  oc.index <- -1


let write_n_bits (os : ostream) n b =
  let rec loop i b =
    if i < n then begin
      write_bit os (b land 1);
      loop (i+1) (b lsr 1)
    end
  in
  if n < 0 || n > Sys.int_size then
    raise (Invalid_argument (Format.sprintf "write_n_bits _ %d" n))
  else loop 0 b

let write_byte os b = write_n_bits os 8 b
let write_short os b = write_n_bits os 16 b
let write_int os b = write_n_bits os Sys.int_size b
