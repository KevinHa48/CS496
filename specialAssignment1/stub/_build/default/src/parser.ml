
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TIMES
    | THEN
    | SIZE
    | SETREF
    | SET
    | SEMICOLON
    | RPAREN
    | REMOVE
    | RBRACE
    | PROC
    | PLUS
    | NEWREF
    | MINUS
    | LPAREN
    | LOOKUP
    | LETREC
    | LET
    | LBRACE
    | ISZERO
    | ISEMPTY
    | INT of (
# 22 "src/parser.mly"
       (int)
# 31 "src/parser.ml"
  )
    | INSERT
    | IN
    | IF
    | ID of (
# 23 "src/parser.mly"
       (string)
# 39 "src/parser.ml"
  )
    | EQUALS
    | EOF
    | END
    | EMPTYTABLE
    | ELSE
    | DIVIDED
    | DEREF
    | DEBUG
    | COMMA
    | BEGIN
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState111
  | MenhirState108
  | MenhirState106
  | MenhirState105
  | MenhirState104
  | MenhirState103
  | MenhirState101
  | MenhirState100
  | MenhirState99
  | MenhirState97
  | MenhirState95
  | MenhirState93
  | MenhirState91
  | MenhirState89
  | MenhirState87
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState77
  | MenhirState75
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState62
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState56
  | MenhirState55
  | MenhirState54
  | MenhirState52
  | MenhirState50
  | MenhirState49
  | MenhirState44
  | MenhirState43
  | MenhirState41
  | MenhirState37
  | MenhirState36
  | MenhirState33
  | MenhirState31
  | MenhirState29
  | MenhirState26
  | MenhirState20
  | MenhirState18
  | MenhirState17
  | MenhirState16
  | MenhirState14
  | MenhirState9
  | MenhirState7
  | MenhirState4
  | MenhirState2
  | MenhirState0

# 8 "src/parser.mly"
  
open Ast

# 136 "src/parser.ml"

let rec _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv419) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv417) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_expr_) : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__ = 
# 144 "<standard.mly>"
    ( x )
# 158 "src/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv418)) : 'freshtv420)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv423 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv421 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_expr_) : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_expr)), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_expr_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 175 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv422)) : 'freshtv424)
    | _ ->
        _menhir_fail ()

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | DEBUG ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | EMPTYTABLE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | ID _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | IF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INSERT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | ISEMPTY ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | ISZERO ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LETREC ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LOOKUP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | PROC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | REMOVE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | SIZE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | DEBUG ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | EMPTYTABLE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | ID _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | IF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | INSERT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | ISEMPTY ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | ISZERO ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LETREC ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LOOKUP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | PROC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | REMOVE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | SIZE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | DEBUG ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | EMPTYTABLE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | ID _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | IF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INSERT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | ISEMPTY ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | ISZERO ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LETREC ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LOOKUP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | PROC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | REMOVE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | SIZE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | DEBUG ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | EMPTYTABLE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | ID _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | IF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | INSERT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | ISEMPTY ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | ISZERO ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LETREC ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LOOKUP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | PROC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | REMOVE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | SIZE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState52 | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState49 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | DEBUG ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | EMPTYTABLE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | ID _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | IF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | INSERT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | ISEMPTY ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | ISZERO ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | LETREC ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | LOOKUP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | PROC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | REMOVE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | SIZE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv248)
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_expr_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 462 "src/parser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv250)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv252)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv255 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv253 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 147 "src/parser.mly"
                                  ( Mul(e1,e2) )
# 479 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv254)) : 'freshtv256)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv259 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | EMPTYTABLE | END | EOF | ID _ | IF | IN | INSERT | INT _ | ISEMPTY | ISZERO | LET | LETREC | LOOKUP | LPAREN | MINUS | NEWREF | PLUS | PROC | RBRACE | REMOVE | RPAREN | SEMICOLON | SET | SETREF | SIZE | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv257 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 145 "src/parser.mly"
                                 ( Add(e1,e2) )
# 500 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv258)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55) : 'freshtv260)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv263 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv261 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 148 "src/parser.mly"
                                    ( Div(e1,e2) )
# 517 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv262)) : 'freshtv264)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv267 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | EMPTYTABLE | END | EOF | ID _ | IF | IN | INSERT | INT _ | ISEMPTY | ISZERO | LET | LETREC | LOOKUP | LPAREN | MINUS | NEWREF | PLUS | PROC | RBRACE | REMOVE | RPAREN | SEMICOLON | SET | SETREF | SIZE | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv265 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 146 "src/parser.mly"
                                  ( Sub(e1,e2) )
# 538 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv266)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59) : 'freshtv268)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv273 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv271 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState60 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv269 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 149 "src/parser.mly"
                                   ( Debug(e) )
# 572 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)) : 'freshtv272)
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv274)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv279 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv277 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState62 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv275 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 156 "src/parser.mly"
                                      ( DeRef(e) )
# 608 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv276)) : 'freshtv278)
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv280)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv283 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv281 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState64 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | DEBUG ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | EMPTYTABLE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | ID _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | IF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | INSERT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | ISEMPTY ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | ISZERO ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LETREC ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LOOKUP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | PROC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | REMOVE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | SIZE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv282)
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv284)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv287 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv285 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState66 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | DEBUG ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | EMPTYTABLE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | ID _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | IF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | INSERT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | ISEMPTY ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | ISZERO ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | LETREC ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | LOOKUP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | PROC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | REMOVE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | SIZE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67) : 'freshtv286)
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66) : 'freshtv288)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv291 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | EMPTYTABLE | END | EOF | ID _ | IF | IN | INSERT | INT _ | ISEMPTY | ISZERO | LET | LETREC | LOOKUP | LPAREN | NEWREF | PROC | RBRACE | REMOVE | RPAREN | SEMICOLON | SET | SETREF | SIZE | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv289 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)), _), _, (e3 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 158 "src/parser.mly"
                                                      ( ITE(e1,e2,e3) )
# 781 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv290)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv292)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv295 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv293 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState69 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | DEBUG ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | EMPTYTABLE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | ID _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | IF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | INSERT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | ISEMPTY ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | ISZERO ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LETREC ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LOOKUP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | PROC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | REMOVE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | SIZE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv294)
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69) : 'freshtv296)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv299 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv297 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState71 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | DEBUG ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | EMPTYTABLE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | ID _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | IF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | INSERT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | ISEMPTY ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | ISZERO ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LETREC ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LOOKUP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | PROC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | REMOVE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | SIZE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv298)
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv300)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv305 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv303 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState73 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv301 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)), _), _, (e3 : 'tv_expr)) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 161 "src/parser.mly"
                                                                            ( Insert(e1,e2,e3) )
# 957 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv302)) : 'freshtv304)
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv306)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv311 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv309 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState75 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv307 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 164 "src/parser.mly"
                                         ( IsEmpty(e1) )
# 993 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)) : 'freshtv310)
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75) : 'freshtv312)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv317 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv315 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState77 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv313 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 154 "src/parser.mly"
                                       ( IsZero(e) )
# 1029 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)) : 'freshtv316)
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77) : 'freshtv318)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv321 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1043 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv319 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1055 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState79 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | DEBUG ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | EMPTYTABLE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | ID _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | IF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | INSERT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | ISEMPTY ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | ISZERO ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | LETREC ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | LOOKUP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | PROC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | REMOVE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | SIZE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80) : 'freshtv320)
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv322)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv325 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1121 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | EMPTYTABLE | END | EOF | ID _ | IF | IN | INSERT | INT _ | ISEMPTY | ISZERO | LET | LETREC | LOOKUP | LPAREN | NEWREF | PROC | RBRACE | REMOVE | RPAREN | SEMICOLON | SET | SETREF | SIZE | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv323 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1139 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), (x : (
# 23 "src/parser.mly"
       (string)
# 1144 "src/parser.ml"
            ))), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 150 "src/parser.mly"
                                                    ( Let(x,e1,e2) )
# 1152 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv324)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv326)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv329 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1164 "src/parser.ml"
        ))) * (
# 23 "src/parser.mly"
       (string)
# 1168 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv327 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1180 "src/parser.ml"
            ))) * (
# 23 "src/parser.mly"
       (string)
# 1184 "src/parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState82 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | DEBUG ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | EMPTYTABLE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | ID _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | IF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | INSERT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | ISEMPTY ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | ISZERO ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | LETREC ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | LOOKUP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | PROC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | REMOVE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | SIZE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83) : 'freshtv328)
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82) : 'freshtv330)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv333 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1250 "src/parser.ml"
        ))) * (
# 23 "src/parser.mly"
       (string)
# 1254 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | EMPTYTABLE | END | EOF | ID _ | IF | IN | INSERT | INT _ | ISEMPTY | ISZERO | LET | LETREC | LOOKUP | LPAREN | NEWREF | PROC | RBRACE | REMOVE | RPAREN | SEMICOLON | SET | SETREF | SIZE | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv331 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1272 "src/parser.ml"
            ))) * (
# 23 "src/parser.mly"
       (string)
# 1276 "src/parser.ml"
            )))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), (x : (
# 23 "src/parser.mly"
       (string)
# 1281 "src/parser.ml"
            ))), (y : (
# 23 "src/parser.mly"
       (string)
# 1285 "src/parser.ml"
            ))), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 151 "src/parser.mly"
                                                                               ( Letrec(x,y,e1,e2) )
# 1295 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv332)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv334)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv337 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv335 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState85 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | DEBUG ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | EMPTYTABLE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | ID _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
            | IF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | INSERT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
            | ISEMPTY ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | ISZERO ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | LETREC ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | LOOKUP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | PROC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | REMOVE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | SIZE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv336)
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85) : 'freshtv338)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv343 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv341 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState87 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv339 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 162 "src/parser.mly"
                                                          ( Lookup(e1,e2) )
# 1400 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv340)) : 'freshtv342)
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87) : 'freshtv344)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv349 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv347 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState89 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv345 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), _), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 168 "src/parser.mly"
                                      ( Sub(Int 0, e) )
# 1436 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv346)) : 'freshtv348)
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89) : 'freshtv350)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv355 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | DEBUG ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | DEREF ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | EMPTYTABLE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | ID _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | IF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | INSERT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | ISEMPTY ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | ISZERO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LETREC ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LOOKUP ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PROC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | REMOVE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv353 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState91 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv351 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 166 "src/parser.mly"
                               (e)
# 1505 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv352)) : 'freshtv354)
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | SIZE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91) : 'freshtv356)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv361 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv359 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState93 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv357 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 153 "src/parser.mly"
                                           ( App(e1,e2) )
# 1546 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv358)) : 'freshtv360)
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv362)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv367 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv365 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState95 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv363 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 155 "src/parser.mly"
                                       ( NewRef(e) )
# 1582 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv364)) : 'freshtv366)
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv368)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv373 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 1596 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv371 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 1612 "src/parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState97 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv369 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 1620 "src/parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), (x : (
# 23 "src/parser.mly"
       (string)
# 1626 "src/parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 152 "src/parser.mly"
                                                             ( Proc(x,e) )
# 1636 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv370)) : 'freshtv372)
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97) : 'freshtv374)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv377 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv375 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState99 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | DEBUG ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | EMPTYTABLE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | ID _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | IF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | INSERT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | ISEMPTY ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | ISZERO ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | LETREC ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | LOOKUP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | PROC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | REMOVE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | SIZE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100) : 'freshtv376)
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv378)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv383 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv381 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState101 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv379 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 163 "src/parser.mly"
                                                          ( Remove(e1,e2) )
# 1743 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv380)) : 'freshtv382)
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv384)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv387 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1757 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | EMPTYTABLE | END | EOF | ID _ | IF | IN | INSERT | INT _ | ISEMPTY | ISZERO | LET | LETREC | LOOKUP | LPAREN | NEWREF | PROC | RBRACE | REMOVE | RPAREN | SEMICOLON | SET | SETREF | SIZE | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv385 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 1775 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (x : (
# 23 "src/parser.mly"
       (string)
# 1780 "src/parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 159 "src/parser.mly"
                                    ( Set(x,e) )
# 1787 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv386)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103) : 'freshtv388)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv391 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv389 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState104 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | DEBUG ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | EMPTYTABLE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | ID _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | IF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | INSERT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | ISEMPTY ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | ISZERO ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | LETREC ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | LOOKUP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | PROC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | REMOVE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | SIZE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv390)
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104) : 'freshtv392)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv397 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv395 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState106 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv393 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 157 "src/parser.mly"
                                                          ( SetRef(e1,e2) )
# 1892 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv394)) : 'freshtv396)
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106) : 'freshtv398)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv403 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv401 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState108 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv399 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 165 "src/parser.mly"
                                      ( Size(e1) )
# 1928 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv400)) : 'freshtv402)
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108) : 'freshtv404)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv415 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv413 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState111 in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv411 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (_menhir_stack, _menhir_s, (e : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 81 "src/parser.mly"
       (Ast.expr)
# 1957 "src/parser.ml"
            ) = 
# 113 "src/parser.mly"
                 (  e )
# 1961 "src/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv409) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 81 "src/parser.mly"
       (Ast.expr)
# 1969 "src/parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv407) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 81 "src/parser.mly"
       (Ast.expr)
# 1977 "src/parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv405) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 81 "src/parser.mly"
       (Ast.expr)
# 1985 "src/parser.ml"
            )) : (
# 81 "src/parser.mly"
       (Ast.expr)
# 1989 "src/parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv406)) : 'freshtv408)) : 'freshtv410)) : 'freshtv412)) : 'freshtv414)
        | MINUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | PLUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | TIMES ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111) : 'freshtv416)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv245) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv243) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((xs : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__) : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__) = _v in
    ((let _v : 'tv_exprs = let es = 
# 232 "<standard.mly>"
    ( xs )
# 2018 "src/parser.ml"
     in
    
# 172 "src/parser.mly"
                                            ( es )
# 2023 "src/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv241) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_exprs) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv239 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv235 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv233 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (es : 'tv_exprs)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_expr = 
# 160 "src/parser.mly"
                             ( BeginEnd(es) )
# 2047 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv234)) : 'freshtv236)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv237 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv238)) : 'freshtv240)) : 'freshtv242)) : 'freshtv244)) : 'freshtv246)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv105 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv107 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv109 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv111 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv113 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv115 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2091 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv117 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv119 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv121 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv123 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2115 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv125 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv127 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv129 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv131 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv133 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv135 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv137 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv139 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2159 "src/parser.ml"
        ))) * (
# 23 "src/parser.mly"
       (string)
# 2163 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv141 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2172 "src/parser.ml"
        ))) * (
# 23 "src/parser.mly"
       (string)
# 2176 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv143 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2185 "src/parser.ml"
        ))) * (
# 23 "src/parser.mly"
       (string)
# 2189 "src/parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv145 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2198 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv147 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2207 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv149 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2216 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv151 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv153 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv155 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv157 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv159 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv161 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv163 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv165 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv167 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv169 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv171 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv173 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv175 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv177 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv179 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv181 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv183 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv185 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv187 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv191 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv197 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv199 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv203 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv205 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv207 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv209 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2370 "src/parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv211 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2379 "src/parser.ml"
        ))) * (
# 23 "src/parser.mly"
       (string)
# 2383 "src/parser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv213 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv215 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv219 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv221 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2412 "src/parser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv222)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv223 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv225 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2426 "src/parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv227 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv229 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv231) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv232)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DEBUG ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DEREF ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | EMPTYTABLE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | ID _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | IF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | INSERT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | ISEMPTY ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | ISZERO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LETREC ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LOOKUP ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | PROC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | REMOVE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SIZE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2) : 'freshtv102)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | DEBUG ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | DEREF ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | EMPTYTABLE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | ID _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | IF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | INSERT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | ISEMPTY ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | ISZERO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | LETREC ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | LOOKUP ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | PROC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | REMOVE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | SIZE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv98)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 23 "src/parser.mly"
       (string)
# 2585 "src/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv89 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2596 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | DEBUG ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | EMPTYTABLE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | ID _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
            | IF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | INSERT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
            | ISEMPTY ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | ISZERO ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | LETREC ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | LOOKUP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | PROC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | REMOVE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | SIZE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv90)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv91 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 2652 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)) : 'freshtv94)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | DEBUG ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | DEREF ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | EMPTYTABLE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | ID _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | IF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | INSERT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | ISEMPTY ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | ISZERO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | LETREC ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | LOOKUP ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | PROC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | REMOVE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | SIZE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv86)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv77 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_v : (
# 23 "src/parser.mly"
       (string)
# 2746 "src/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv73 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2757 "src/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LBRACE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv69 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2767 "src/parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | BEGIN ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | DEBUG ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | DEREF ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | EMPTYTABLE ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | ID _v ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
                    | IF ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | INSERT ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | INT _v ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
                    | ISEMPTY ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | ISZERO ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | LET ->
                        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | LETREC ->
                        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | LOOKUP ->
                        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | LPAREN ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | NEWREF ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | PROC ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | REMOVE ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | SET ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | SETREF ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | SIZE ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv70)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv71 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2823 "src/parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)) : 'freshtv74)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv75 * _menhir_state)) * (
# 23 "src/parser.mly"
       (string)
# 2834 "src/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)) : 'freshtv78)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv79 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)) : 'freshtv82)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | DEBUG ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | DEREF ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | EMPTYTABLE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | ID _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | IF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | INSERT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | ISEMPTY ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | ISZERO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LETREC ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LOOKUP ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | PROC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | REMOVE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | SIZE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16) : 'freshtv66)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | DEBUG ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | EMPTYTABLE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | ID _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | IF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | INSERT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | ISEMPTY ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | ISZERO ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LETREC ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LOOKUP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | MINUS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState17 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | DEBUG ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | DEREF ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | EMPTYTABLE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | ID _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | IF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | INSERT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | ISEMPTY ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | ISZERO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | LETREC ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | LOOKUP ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | PROC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | REMOVE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | SIZE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv64)
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | PROC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | REMOVE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | SIZE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | DEBUG ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | DEREF ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | EMPTYTABLE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | ID _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | IF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | INSERT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | ISEMPTY ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | ISZERO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | LETREC ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | LOOKUP ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | PROC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | REMOVE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | SIZE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20) : 'freshtv60)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 23 "src/parser.mly"
       (string)
# 3096 "src/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv51 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3107 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv47 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3117 "src/parser.ml"
                ))) = Obj.magic _menhir_stack in
                let (_v : (
# 23 "src/parser.mly"
       (string)
# 3122 "src/parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | RPAREN ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv43 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3133 "src/parser.ml"
                    ))) * (
# 23 "src/parser.mly"
       (string)
# 3137 "src/parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | EQUALS ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv39 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3147 "src/parser.ml"
                        ))) * (
# 23 "src/parser.mly"
       (string)
# 3151 "src/parser.ml"
                        ))) = Obj.magic _menhir_stack in
                        ((let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | BEGIN ->
                            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | DEBUG ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | DEREF ->
                            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | EMPTYTABLE ->
                            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | ID _v ->
                            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
                        | IF ->
                            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | INSERT ->
                            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | INT _v ->
                            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
                        | ISEMPTY ->
                            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | ISZERO ->
                            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | LET ->
                            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | LETREC ->
                            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | LOOKUP ->
                            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | LPAREN ->
                            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | NEWREF ->
                            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | PROC ->
                            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | REMOVE ->
                            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | SET ->
                            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | SETREF ->
                            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | SIZE ->
                            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv40)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv41 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3207 "src/parser.ml"
                        ))) * (
# 23 "src/parser.mly"
       (string)
# 3211 "src/parser.ml"
                        ))) = Obj.magic _menhir_stack in
                        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)) : 'freshtv44)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv45 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3222 "src/parser.ml"
                    ))) * (
# 23 "src/parser.mly"
       (string)
# 3226 "src/parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)) : 'freshtv48)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv49 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3237 "src/parser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)) : 'freshtv52)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv53 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3248 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)) : 'freshtv56)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 23 "src/parser.mly"
       (string)
# 3272 "src/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv31 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3283 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | DEBUG ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | EMPTYTABLE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | ID _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | IF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | INSERT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | ISEMPTY ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | ISZERO ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LETREC ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LOOKUP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | PROC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | REMOVE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | SIZE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv32)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv33 * _menhir_state) * (
# 23 "src/parser.mly"
       (string)
# 3339 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)) : 'freshtv36)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | DEBUG ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | DEREF ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | EMPTYTABLE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | ID _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | IF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | INSERT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | ISEMPTY ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | ISZERO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | LETREC ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | LOOKUP ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | PROC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | REMOVE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | SIZE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31) : 'freshtv28)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | DEBUG ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | DEREF ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | EMPTYTABLE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | ID _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
        | IF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | INSERT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
        | ISEMPTY ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | ISZERO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | LETREC ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | LOOKUP ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | PROC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | REMOVE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | SIZE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv24)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "src/parser.mly"
       (int)
# 3482 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv21) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 22 "src/parser.mly"
       (int)
# 3492 "src/parser.ml"
    )) : (
# 22 "src/parser.mly"
       (int)
# 3496 "src/parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 142 "src/parser.mly"
              ( Int i )
# 3501 "src/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv22)

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | DEBUG ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | DEREF ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | EMPTYTABLE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | ID _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | IF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | INSERT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | ISEMPTY ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | ISZERO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LETREC ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LOOKUP ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | PROC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | REMOVE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | SIZE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36) : 'freshtv18)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | DEBUG ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | EMPTYTABLE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | ID _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INSERT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | ISEMPTY ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | ISZERO ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LETREC ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LOOKUP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | PROC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | REMOVE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | SIZE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "src/parser.mly"
       (string)
# 3623 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x : (
# 23 "src/parser.mly"
       (string)
# 3633 "src/parser.ml"
    )) : (
# 23 "src/parser.mly"
       (string)
# 3637 "src/parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 143 "src/parser.mly"
             ( Var x )
# 3642 "src/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv16)

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_expr = 
# 144 "src/parser.mly"
                 ( EmptyTable )
# 3656 "src/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv14)

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | DEBUG ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | DEREF ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | EMPTYTABLE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | ID _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | IF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | INSERT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | ISEMPTY ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | ISZERO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LETREC ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LOOKUP ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | PROC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | REMOVE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | SIZE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv10)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | DEBUG ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | DEREF ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | EMPTYTABLE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | ID _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | IF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | INSERT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | ISEMPTY ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | ISZERO ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | LETREC ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | LOOKUP ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | PROC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | REMOVE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | SIZE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43) : 'freshtv6)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | DEBUG ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | EMPTYTABLE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | ID _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | IF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | INSERT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | ISEMPTY ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | ISZERO ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LETREC ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LOOKUP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | PROC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | REMOVE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | SIZE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | END ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState44 in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__ = 
# 142 "<standard.mly>"
    ( [] )
# 3841 "src/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 81 "src/parser.mly"
       (Ast.expr)
# 3864 "src/parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEBUG ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EMPTYTABLE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INSERT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ISEMPTY ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ISZERO ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LETREC ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LOOKUP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PROC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | REMOVE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SIZE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "<standard.mly>"
  

# 3926 "src/parser.ml"
