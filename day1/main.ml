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
  | Nested n -> n

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

let read_struct (yaml : yaml) = 
  (match yaml with 
  | `O mapping -> 
    mapping.m_members
    |> List.fold_left (fun def member -> 
      match member with 
      | (`Scalar name_field, `Scalar name_value) when name_field.value = "name" -> (Some (name_value.value), snd def)
      | (`Scalar members_field, `A members_list) when members_field.value = "members" -> (fst def, Some (List.map parse_member members_list.s_members))
      | _ -> failwith "bad"
    ) (None, None)
  | _ -> failwith "Invalid struct")
  |> (fun (name_opt, members_opt) -> 
    if Option.is_none name_opt || Option.is_none members_opt then 
      failwith ""
    else
      { name = Option.get name_opt; members = Option.get members_opt } 
  )

(* we'll probably want a dynamic mapping of max len for (nested) structs *)

(* maybe migrate to inttypes.h for macro usage *)
let format_string = function 
| Int8 -> "%*hhd"
| Int16 -> "%*hd"
| Int32 -> "%*d"
| Int64 -> "%*lld"
| UInt8 | UInt16 | UInt32 -> "%*u"
| UInt64 -> "%*llu"
| String | Nested _ | Bool -> "%s"
| Char -> "%c"

(* so max chars will take in context containing mappings for already-parsed struct types that may be Nested elsewhere *)
let max_chars = function
| Char -> 1
| Int8 | UInt8 -> 4
| Bool | Int16 | UInt16 -> 6
| Int32 | UInt32 -> 11
| Int64 | UInt64 -> 21
| Nested _ -> failwith "not implemented"
| String -> failwith "bad"

let compute_offsets members = 
  List.fold_left (fun (prev_max_chars, offsets) member -> 
    (* 4 because (comma|open brace), (quote), (quote), (colon) *)
    (max_chars (snd member), offsets@[prev_max_chars + 4 + (String.length (fst member))])
  ) (0, []) members
  |> snd

let member_type_template typ = 
  match typ with 
  | Nested _ -> failwith "not implemented " (* will be a lookup *)
  | t -> String.make (max_chars t) ' '

let member_template (name, typ) = 
  "\\\"" ^ name ^ "\\\":" ^ member_type_template typ

let make_empty_template members = 
  "{"
  ^ string_join member_template "," members
  ^ "}"

let generate_template_method members = 
  "\tstatic std::string empty()\n\t{\n\t\treturn \"" ^ make_empty_template members ^ "\";\n\t}"

let format_value (name, typ) = 
  match typ with 
  | Bool -> name ^ " ? \"true\" : \"false\""
  | _ -> name

let generate_format_call (name, typ) offset = 
  match typ with 
  | Nested _ -> "\t\thead += " ^ string_of_int offset ^ ";\n\t\t" ^ name ^ ".format(head);"
  | _ -> 
    "head += " 
    ^ (string_of_int offset) 
    ^ ";\n\t\tsnprintf(head, " 
    ^ string_of_int (max_chars typ) 
    ^ ", \""
    ^ format_string typ
    ^ "\", " 
    ^ string_of_int (max_chars typ - 1)
    ^ ", "
    ^ format_value (name, typ)
    ^ ");\n"

let generate_format_method members offsets = 
  "\tvoid format(char* buf) const\n\t{\n\t\tauto head { buf };\n\t\t"
  ^ string_join (
      fun (member, offset) -> generate_format_call member offset
    ) "\n\t\t" (List.combine members offsets)
  ^ "\t}"

let generate_member_defn (name, typ) = 
  string_of_member_type typ ^ " " ^ name ^ ";"

let generate_member_defns members = 
  string_join (generate_member_defn) "\n\t" members

let generate_struct_definition def = 
  "struct " ^ def.name ^ "\n{\t"
  ^ generate_member_defns def.members
  ^ "\n"
  ^ generate_template_method def.members
  ^ "\n"
  ^ generate_format_method def.members (compute_offsets def.members)
  ^ "\n};"

(* let logger_defn = {|template <typename TStruct>
class json_logger
{
    std::string _buf{TStruct::empty()};
public:
    json_logger() {}

    void log(const TStruct& t) {
        t.format(_buf.data());
        std::cout << _buf << std::endl;
    }
};|} *)

let () = 
parse_yaml "/users/jamesmeyers/life/test_file.yaml"
|> read_struct
|> fun def -> "definition: " ^ (string_of_schema def) 
          ^ "\ngenerated :   \n" ^ generate_struct_definition def
|> print_endline