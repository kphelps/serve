use nom::{alpha, is_digit, IResult};
use std::str;
use std::str::FromStr;
use std::string::String;

#[derive(Debug, Eq, PartialEq)]
enum Expression {
    FunctionCall(String, Vec<Expression>),
    IntLiteral(i64),
    StringLiteral(String),
    Identifier(String)
}

#[derive(Debug, Eq, PartialEq)]
struct FunctionParameter {
    name: String,
    tipe: String,
}

impl FunctionParameter {
    fn new(name: &str, tipe: &str) -> FunctionParameter {
        FunctionParameter {
            name: name.to_owned(),
            tipe: tipe.to_owned(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Statement {
    Application(String, Vec<u8>),
    Endpoint(String, Vec<FunctionParameter>, String, Vec<Expression>)
}

named!(parser<Vec<Statement>>,
    many0!(statement)
);

named!(statement<Statement>,
    alt_complete!(class_definition | method_definition)
);

named!(expression<Expression>,
    alt_complete!(
        function_call |
        string_literal |
        int_literal |
        map!(map_res!(identifier, FromStr::from_str), Expression::Identifier)
    )
);

named!(function_call<Expression>,
    dbg_dmp!(do_parse!(
        name: identifier >>
        body: delimited!(
            ws!(tag!("(")),
            ws!(separated_list!(ws!(tag!(",")), ws!(expression))),
            ws!(tag!(")"))
        ) >>

        (Expression::FunctionCall(name.to_owned(), body))
    )
    )

);

named!(int_literal<Expression>,
    map!(
        map_res!(
            map_res!(
                ws!(take_while!(is_digit)),
                str::from_utf8
            ),
            FromStr::from_str
        ),
        Expression::IntLiteral
    )
);

named!(string_literal<Expression>,
    map!(
        map_res!(
            map_res!(
                delimited!(
                    tag!("\""),
                    take_until!("\""),
                    tag!("\"")
               ),
               str::from_utf8
            ),
            FromStr::from_str
        ),
        Expression::StringLiteral
    )
);

named!(identifier<&str>,
    map_res!(ws!(alpha), str::from_utf8)
);

named!(class_definition<Statement>,
    ws!(
        do_parse!(
            tag!("application") >>
            name: identifier >>
            body: take_until_and_consume_s!("end") >>

            (Statement::Application(name.to_owned(), body.to_owned()))
         )
    )
);

named!(method_definition<Statement>,
    ws!(
        do_parse!(
            tag!("endpoint") >>
            name: identifier >>
            args: delimited!(
                ws!(tag!("(")),
                separated_list!(
                    ws!(tag!(",")),
                    map!(
                        separated_pair!(identifier, ws!(tag!(":")), identifier),
                        |(x,y)| { FunctionParameter::new(x, y) }
                    )
                ),
                ws!(tag!(")"))
            ) >>
            ws!(tag!("->")) >>
            return_type: identifier >>
            body: map!(
                many_till!(ws!(expression), ws!(tag!("end"))),
                |(exprs, _)| { exprs }
            ) >>

            (Statement::Endpoint(name.to_owned(), args, return_type.to_string(), body))
       )
    )
);

named!(comma_separated_exprs<Vec<Expression>>,
    separated_list!(ws!(tag!(",")), expression)
) ;

#[test]
fn test_empty_application() {
    assert_eq!(
        parser(b"application App end"),
        IResult::Done(
            &b""[..],
            vec![Statement::Application("App".to_string(), Vec::new())]
        )
    )
}

#[test]
fn test_empty_endpoint() {
    assert_eq!(
        parser(b"endpoint index() -> Return end"),
        IResult::Done(
            &b""[..],
            vec![
                Statement::Endpoint(
                    "index".to_string(),
                    Vec::new(),
                    "Return".to_string(),
                    Vec::new()
                )
            ]
        )
    )
}

#[test]
fn test_endpoint_with_arg() {
    assert_eq!(
        parser(b"endpoint index(a: String) -> Return end"),
        IResult::Done(
            &b""[..],
            vec![
                Statement::Endpoint(
                    "index".to_string(),
                    vec![FunctionParameter::new("a", "String")],
                    "Return".to_string(),
                    vec![]
                )
            ]
        )
    )
}

#[test]
fn test_endpoint_with_two_args() {
    assert_eq!(
        parser(b"endpoint index(a: String, b: Integer) -> Return end"),
        IResult::Done(
            &b""[..],
            vec![
                Statement::Endpoint(
                    "index".to_string(),
                    vec![
                        FunctionParameter::new("a", "String"),
                        FunctionParameter::new("b", "Integer"),
                    ],
                    "Return".to_string(),
                    vec![]
                )
            ]
        )
    )
}

#[test]
fn test_endpoint_with_many_args() {
    assert_eq!(
        parser(b"endpoint index(a: String, b: Integer, c: Json) -> Return end"),
        IResult::Done(
            &b""[..],
            vec![
                Statement::Endpoint(
                    "index".to_string(),
                    vec![
                        FunctionParameter::new("a", "String"),
                        FunctionParameter::new("b", "Integer"),
                        FunctionParameter::new("c", "Json"),
                    ],
                    "Return".to_string(),
                    vec![]
                )
            ]
        )
    )
}

#[test]
fn test_endpoint_with_function() {
    assert_eq!(
        parser(b"endpoint index(a: String) -> Return fn(a) end"),
        IResult::Done(
            &b""[..],
            vec![
                Statement::Endpoint(
                    "index".to_string(),
                    vec![FunctionParameter::new("a", "String")],
                    "Return".to_string(),
                    vec![
                        Expression::FunctionCall(
                            "fn".to_string(),
                            vec![Expression::Identifier("a".to_string())]
                        )
                    ]
                )
            ]
        )
    )
}

#[test]
fn test_function_call_no_args() {
    assert_eq!(
        function_call(b"fn()"),
        IResult::Done(
            &b""[..],
            Expression::FunctionCall("fn".to_string(), vec![])
        )
    );
}

#[test]
fn test_function_call_one_identifier_arg() {
    assert_eq!(
        function_call(b"fn(a)"),
        IResult::Done(
            &b""[..],
            Expression::FunctionCall(
                "fn".to_string(),
                vec![Expression::Identifier("a".to_string())]
            )
        )
    );
}

#[test]
fn test_function_call_two_identifier_args() {
    assert_eq!(
        function_call(b"fn(a, b)"),
        IResult::Done(
            &b""[..],
            Expression::FunctionCall(
                "fn".to_string(),
                vec![
                    Expression::Identifier("a".to_string()),
                    Expression::Identifier("b".to_string()),
                ]
            )
        )
    );
}

#[test]
fn test_function_call_four_identifier_args() {
    assert_eq!(
        function_call(b"fn(a,b,c ,d, e,f)"),
        IResult::Done(
            &b""[..],
            Expression::FunctionCall(
                "fn".to_string(),
                vec![
                    Expression::Identifier("a".to_string()),
                    Expression::Identifier("b".to_string()),
                    Expression::Identifier("c".to_string()),
                    Expression::Identifier("d".to_string()),
                    Expression::Identifier("e".to_string()),
                    Expression::Identifier("f".to_string()),
                ]
            )
        )
    );
}

#[test]
fn test_function_call_mixed() {
    assert_eq!(
        function_call(b"fn(a, \"b\", 123)"),
        IResult::Done(
            &b""[..],
            Expression::FunctionCall(
                "fn".to_string(),
                vec![
                    Expression::Identifier("a".to_string()),
                    Expression::StringLiteral("b".to_string()),
                    Expression::IntLiteral(123i64),
                ]
            )
        )
    );
}

#[test]
fn test_function_call_nested() {
    assert_eq!(
        function_call(b"fn(fn())"),
        IResult::Done(
            &b""[..],
            Expression::FunctionCall(
                "fn".to_string(),
                vec![
                    Expression::FunctionCall(
                        "fn".to_string(),
                        vec![]
                    ),
                ]
            )
        )
    );
}

#[test]
fn test_function_call_nested_multiple() {
    assert_eq!(
        function_call(b"fn(fn(fn()))"),
        IResult::Done(
            &b""[..],
            Expression::FunctionCall(
                "fn".to_string(),
                vec![
                    Expression::FunctionCall(
                        "fn".to_string(),
                        vec![
                            Expression::FunctionCall(
                                "fn".to_string(),
                                vec![]
                            ),
                        ]
                    ),
                ]
            )
        )
    );
}

#[test]
fn test_function_call_nested_multiple_args() {
    assert_eq!(
        function_call(b"fn(a, fn(b, fn(c, d, e)), f)"),
        IResult::Done(
            &b""[..],
            Expression::FunctionCall(
                "fn".to_string(),
                vec![
                    Expression::Identifier("a".to_string()),
                    Expression::FunctionCall(
                        "fn".to_string(),
                        vec![
                            Expression::Identifier("b".to_string()),
                            Expression::FunctionCall(
                                "fn".to_string(),
                                vec![
                                    Expression::Identifier("c".to_string()),
                                    Expression::Identifier("d".to_string()),
                                    Expression::Identifier("e".to_string()),
                                ]
                            ),
                        ]
                    ),
                    Expression::Identifier("f".to_string()),
                ]
            )
        )
    );
}
