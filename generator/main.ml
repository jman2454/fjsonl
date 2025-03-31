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

let value_length_expr = function
| Float -> "max_float_length"
| Double -> "max_double_length"
| Char -> "1"
| Int8 | UInt8 -> "4"
| Bool | Int16 | UInt16 -> "6"
| Int32 | UInt32 -> "12"
| Int64 | UInt64 -> "22"
| Nested s -> s ^ {|::length|}
| String -> failwith "bad"

let compute_offset_exprs members = 
  List.fold_left (fun (prev_length_expr, offsets) (name, typ) -> 
    (* 4 because (comma|open brace), (quote), (quote), (colon) *)
    (value_length_expr typ, offsets@[
      (if prev_length_expr <> "" then prev_length_expr ^ " + " else "")
      ^ "4 + " ^ string_of_int (String.length name)])
  ) ("", []) members
  |> snd

let member_type_template typ = 
  match typ with 
  | Nested t -> t ^ {|::empty()|}
  | _ -> "std::string(" ^ value_length_expr typ ^ ", ' ')"

let member_template (name, typ) = 
  "std::string(\"\\\"" ^ name ^ "\\\":\") + " ^ member_type_template typ

let make_empty_template members = 
  if List.length members = 0 then {|"{}"|} else 
  {|std::string("{") + |}
  ^ string_join (fun s -> member_template s) {| + std::string(",") + |} members
  ^ {| + std::string("}")|}

let generate_template_method members = 
  "\tstatic std::string empty()\n\t{\n\t\treturn " ^ make_empty_template members ^ ";\n\t}"

let generate_member_length_expr (name, typ) = 
  (* quotes and colon *)
  "3 + " ^ string_of_int (String.length name) ^ " + " ^ value_length_expr typ

let generate_type_length_expr members = 
  "2 + " (* opening and ending braces *)
  ^ string_join generate_member_length_expr " + " members
  ^ " + "
  ^ (List.length members - 1 |> string_of_int) (* commas *)

let generate_length_definition members = 
  "static inline constexpr int length = " ^ generate_type_length_expr members ^ ";"

let generate_format_call (name, typ) = 
  "head += " 
  ^ "_gen_offset_" ^ name
  ^ ";\n\t\t"
  ^ (
    match typ with 
    | Nested _ -> name ^ ".format(head);"
    | Char -> "*head = " ^ name ^ ";" 
    | String -> failwith "bad"
    | Float | Double -> 
      "write_forwards("
      ^ name
      ^ ", head"
      ^ ", "
      ^ value_length_expr typ
      ^ ", ' ');"
      (* special case, no need to fill padding because we only ever write one char *)
    | Int8 | Int16 | Int32 | Int64 | UInt8 | UInt16 | UInt32 | UInt64 | Bool
    -> 
      "write_backwards("
      ^ name
      ^ ", head" ^ " + " ^ value_length_expr typ ^ " - 1"
      ^ ", " 
      ^ value_length_expr typ 
      ^ ", ' ');"
  )

let generate_format_method members  = 
  "\tvoid format(char* buf) const\n\t{\n\t\tauto head { buf };\n\t\t"
  ^ string_join generate_format_call "\n\t\t" members
  ^ "\n\t}"

let generate_member_defn (name, typ) = 
  string_of_member_type typ ^ " " ^ name ^ ";"

let generate_member_defns members = 
  string_join generate_member_defn "\n\t" members

let generate_offset_definition ((name, _), offset_expr) = 
  "static constexpr int _gen_offset_" ^ name ^ " = " ^ offset_expr ^ ";"

let generate_offsets_exprs members_offsets = 
  string_join generate_offset_definition "\n\t" members_offsets

let generate_struct def = 
  "struct " ^ def.name ^ "\n{\t"
  ^ generate_member_defns def.members
  ^ "\n"
  ^ generate_length_definition def.members
  ^ "\n"
  ^ generate_template_method def.members
  ^ "\n"
  ^ generate_offsets_exprs (List.combine def.members (compute_offset_exprs def.members))
  ^ "\n"
  ^ generate_format_method def.members
  ^ "\n};"

let generate_structs definitions = 
  List.fold_left (fun structs definition -> 
    (generate_struct definition::structs)
  ) [] definitions
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