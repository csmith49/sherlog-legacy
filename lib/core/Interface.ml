let var s = Term.Variable (Data.Identifier.of_string s)
let int i = Term.Integer i
let bool b = Term.Boolean b
let atom s = Term.Atom s

let (:=) head body = {
    Basic.head = head;
    body = body;
}
