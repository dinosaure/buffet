module type S = sig
  type le = [`le]
  type be = [`be]
  type ne = [`ne]
  type signed = [`signed]
  type unsigned = [`unsigned]
  type nonrec 'a int8 = private int constraint 'a = [< unsigned | signed]
  type nonrec 'a int16 = private int constraint 'a = [< unsigned | le | be | ne]
  type nonrec 'a int32 = private int32 constraint 'a = [< le | be | ne]
  type nonrec 'a int64 = private int64 constraint 'a = [< le | be | ne]

  external unsafe_to_int8 : int -> signed int8 = "%identity"
  external unsafe_to_uint8 : int -> unsigned int8 = "%identity"
  external unsafe_to_int16_le : int -> le int16 = "%identity"
  external unsafe_to_int16_be : int -> be int16 = "%identity"
  external unsafe_to_uint16_le : int -> [unsigned | le] int16 = "%identity"
  external unsafe_to_uint16_be : int -> [unsigned | be] int16 = "%identity"
  external unsafe_to_int32_le : Int32.t -> le int32 = "%identity"
  external unsafe_to_int32_be : Int32.t -> be int32 = "%identity"
  external unsafe_to_int64_le : Int64.t -> le int64 = "%identity"
  external unsafe_to_int64_be : Int64.t -> be int64 = "%identity"
end

type le = [`le]
type be = [`be]
type ne = [`ne]
type signed = [`signed]
type unsigned = [`unsigned]
type nonrec 'a int8 = private int constraint 'a = [< unsigned | signed]
type nonrec 'a int16 = int constraint 'a = [< unsigned | le | be | ne]
type nonrec 'a int32 = int32 constraint 'a = [< le | be | ne]
type nonrec 'a int64 = int64 constraint 'a = [< le | be | ne]

external unsafe_to_int8 : int -> signed int8 = "%identity"
external unsafe_to_uint8 : int -> unsigned int8 = "%identity"
external unsafe_to_int16_le : int -> le int16 = "%identity"
external unsafe_to_int16_be : int -> be int16 = "%identity"
external unsafe_to_uint16_le : int -> [unsigned | le] int16 = "%identity"
external unsafe_to_uint16_be : int -> [unsigned | be] int16 = "%identity"
external unsafe_to_int32_le : Int32.t -> le int32 = "%identity"
external unsafe_to_int32_be : Int32.t -> be int32 = "%identity"
external unsafe_to_int64_le : Int64.t -> le int64 = "%identity"
external unsafe_to_int64_be : Int64.t -> be int64 = "%identity"
