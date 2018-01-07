type ide = string
type 't env = ide -> 't
let applyEnv (env : 't env) (id : ide) : 't = env id
let emptyEnv : 't env = fun (x : ide) -> failwith ("Binding for " ^ x ^ " not defined")
let bind (id : ide) (exp : 'e) (env : 't env) : 't env= fun (x : ide) ->
    if x = id then exp else applyEnv env x
