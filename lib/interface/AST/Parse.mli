type result = CALL.tag list * CALL.t

val parse : string -> result

val program : result -> Core.Program.t
val queries : result -> Core.Query.t list
val evidence : result -> Core.Evidence.t list
val parameters : result -> Core.Parameter.t list