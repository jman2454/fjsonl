(* maybe also parameterize how many reactions a cell can do in a given step -- maybe they can have charge based on neighbors or sumn *)

include Yaml

let parse_yaml filename = 
  let yaml = 
    try
      let ch = open_in filename in
      let yaml_str = really_input_string ch (in_channel_length ch) in
      close_in ch;
      yaml_str
    with _ -> failwith ("Could not read file: " ^ filename)
  in
  
  match yaml_of_string yaml with
  | Ok yaml_value -> yaml_value
  | Error (`Msg msg) -> failwith ("YAML parsing error: " ^ msg)

(* note that we don't allow for recursion in these struct definitions. meant to be simple data *)
type loggable_struct = { name : string; members : member list }
and member_type = 
| String
| UInt8
| Int8
| UInt16
| Int16
| UInt32
| Int32
| UInt64
| Int64
| Char
| Bool
| Float
| Double
| Nested of string
and member = string * member_type

(* maybe delineate cpp type names and abstract names in separate methods -- for now, conflate *)
let string_of_member_type typ = 
  match typ with 
  | String -> "std::string"
  | UInt8 -> "uint8_t"
  | Int8 -> "int8_t"
  | UInt16 -> "uint16_t"
  | Int16 -> "int16_t"
  | UInt32 -> "uint32_t"
  | Int32 -> "int32_t"
  | UInt64 -> "uint64_t"
  | Int64 -> "int64_t"
  | Char -> "char"
  | Bool -> "bool"
  | Float -> "float"
  | Double -> "double"
  | Nested n -> n

  (* random idea-- static time constructor surjectivity analysis for sum types *)
let parse_type type_string = 
  match type_string with 
  | "string" -> String
  | "uint8" -> UInt8
  | "int8" -> Int8
  | "uint16" -> UInt16
  | "int16" -> Int16
  | "uint32" -> UInt32
  | "int32" -> Int32
  | "uint64" -> UInt64
  | "int64" -> Int64
  | "char" -> Char
  | "bool" -> Bool
  | "float" -> Float
  | "double" -> Double
  | s -> Nested(s)

let string_of_member member = 
  (fst member) ^ ": " ^ (string_of_member_type (snd member))

let string_join s_of_v sep l = 
  if l = [] then "" else 
  List.fold_left (fun acc v -> acc ^ sep ^ s_of_v v) (s_of_v (List.hd l)) (List.tl l)

let string_of_list s_of_v l = "[" ^ string_join s_of_v ", " l ^ "]"

let string_of_schema ls = ls.name ^ ": " ^ string_of_list string_of_member ls.members

let parse_member (yaml : yaml) = 
  match yaml with 
  | `O { m_members = (`Scalar name, `Scalar typ)::[]; _ } -> (name.value, parse_type typ.value)
  | _ -> failwith "bad"

let read_structs (yaml : yaml) = 
  (match yaml with 
  | `O mapping -> 
    mapping.m_members
    |> List.fold_left (fun (state, names, members) member -> 
      match member with 
      | (`Scalar name_field, `Scalar name_value) when name_field.value = "name" && state = `Name ->
        (`Member, name_value.value::names, members)
      | (`Scalar members_field, `A members_list) when members_field.value = "members" && state = `Member -> 
        (`Name, names, List.map parse_member members_list.s_members::members)
      | _ -> failwith "Bad input"
    ) (`Name, [], [])
  | _ -> failwith "Invalid structs")
  |> (fun (_, names, members) -> 
    List.combine names members
    |> List.map (fun (name, members) -> { name = name; members = members } )
    |> List.rev
  )

module StringMap = Map.Make(String)

(* not going to do the topo sort yet -- we'll assume things are given in DAG order *)

type context = { template : string }

let value_length_expr ctx_lookup = function
| Float -> "max_float_chars"
| Double -> "max_double_chars"
| Char -> "1"
| Int8 | UInt8 -> "4"
| Bool | Int16 | UInt16 -> "6"
| Int32 | UInt32 -> "12"
| Int64 | UInt64 -> "22"
| Nested s -> 
  (match StringMap.find_opt s ctx_lookup with
  | Some ctx -> String.length (ctx.template |> Str.global_replace (Str.regexp {|\\|}) "") |> string_of_int
  | _ -> failwith ("unknown type: " ^ s))
| String -> failwith "bad"

(* todo make offsets constexpr *)
let compute_offsets members ctx_lookup = 
  List.fold_left (fun (prev_length_expr, offsets) (name, typ) -> 
    (* 4 because (comma|open brace), (quote), (quote), (colon) *)
    (value_length_expr ctx_lookup typ, offsets@[prev_length_expr ^ "+ 4 + " ^ string_of_int (String.length name)])
  ) ("", []) members
  |> snd

let member_type_template typ ctx_lookup = 
  match typ with 
  | Nested s -> 
    (match StringMap.find_opt s ctx_lookup with
    | Some ctx -> ctx.template
    | _ -> failwith ("unknown type: " ^ s))
  | _ -> "std::string{" ^ value_length_expr ctx_lookup typ ^ ", ' '}"

let member_template ctx_lookup (name, typ) = 
  "\\\"" ^ name ^ "\\\":" ^ member_type_template typ ctx_lookup

let make_empty_template members ctx_lookup = 
  "{"
  ^ string_join (member_template ctx_lookup) "," members
  ^ "}"

let generate_template_method members ctx_lookup = 
  "\tstatic std::string empty()\n\t{\n\t\treturn \"" ^ make_empty_template members ctx_lookup ^ "\";\n\t}"

let generate_format_call (name, typ) offset_expr ctx_lookup = 
  "head += " 
  ^ offset_expr 
  ^ ";\n\t\t"
  ^ (
    match typ with 
    | Nested _ -> name ^ ".format(head);"
    | Char -> "*head = " ^ name ^ ";" 
    | String -> failwith "bad"
      (* special case, no need to fill padding because we only ever write one char *)
    | Int8 | Int16 | Int32 | Int64 | UInt8 | UInt16 | UInt32 | UInt64 | Bool | Float | Double
    -> 
      "write_backwards(" 
      ^ name
      ^ ", head" ^ " + " ^ value_length_expr ctx_lookup typ ^ " - 1"
        (* TODO: migrate all writes to write_backwards and then we can change the head += things
          so that we don't need to add to it here *)
      ^ ", " 
      ^ value_length_expr ctx_lookup typ 
      ^ ", ' ');"
  )

let generate_format_method members offsets ctx_lookup = 
  "\tvoid format(char* buf) const\n\t{\n\t\tauto head { buf };\n\t\t"
  ^ string_join (
      fun (member, offset) -> generate_format_call member offset ctx_lookup
    ) "\n\t\t" (List.combine members offsets)
  ^ "\n\t}"

let generate_member_defn (name, typ) = 
  string_of_member_type typ ^ " " ^ name ^ ";"

let generate_member_defns members = 
  string_join (generate_member_defn) "\n\t" members

let generate_struct def ctx_lookup = 
  let template = make_empty_template def.members ctx_lookup in 
  StringMap.add def.name { template = template } ctx_lookup,
  "struct " ^ def.name ^ "\n{\t"
  ^ generate_member_defns def.members
  ^ "\n"
  ^ generate_template_method def.members ctx_lookup
  ^ "\n"
  ^ generate_format_method def.members (compute_offsets def.members ctx_lookup) ctx_lookup
  ^ "\n};"

let generate_structs definitions = 
  List.fold_left (fun (ctx, structs) definition -> 
    let ctx, cpp_generated = generate_struct definition ctx in 
    (ctx, cpp_generated::structs)
  ) (StringMap.empty, []) definitions
  |> snd
  |> List.rev

let () = 
parse_yaml "/users/jamesmeyers/life/test_file.yaml"
|> read_structs
|> (fun ss -> string_join string_of_schema "\n" ss |> ignore; ss)
|> generate_structs
|> string_join (fun x -> x) "\n"
|> (fun s -> {|#include <cstdint>
#include <iostream>
#include <string>
#include "util.h"|} ^ "\n\n" ^ s)
|> print_endline