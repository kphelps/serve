use env_logger;
use nom::{alphanumeric, IResult, is_digit};
use std::str;
use std::str::FromStr;
use std::string::String;
use super::ast::{AST, Expression, FunctionParameter, Statement};
use super::helpers;
use super::symbol::{Symbol, SymbolRegistry};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
    Application(),
    Endpoint(),
    Serializer(),
    Return(),
    End(),
    RightArrow(),
    OpenParen(),
    CloseParen(),
    Comma(),
    Period(),
    Colon(),
    OpenSquareBracket(),
    CloseSquareBracket(),
    StringLiteral(String),
    IntLiteral(i64),
    Identifier(String),
}

named!(pub lex<Vec<Token>>,
    many0!(alt_complete!(
        kw0!("application", Application) |
        kw0!("endpoint", Endpoint) |
        kw0!("serializer", Serializer) |
        kw0!("return", Return) |
        kw0!("end", End) |
        kw0!("->", RightArrow) |
        kw0!("(", OpenParen) |
        kw0!(")", CloseParen) |
        kw0!(",", Comma) |
        kw0!(".", Period) |
        kw0!(":", Colon) |
        kw0!("[", OpenSquareBracket) |
        kw0!("]", CloseSquareBracket) |
        kw1!(string_literal, StringLiteral) |
        kw1!(int_literal, IntLiteral) |
        kw1!(identifier, Identifier)
    ))
);

named!(string_literal<String>,
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
    )
);

named!(int_literal<i64>,
    map_res!(
        map_res!(
            ws!(take_while!(is_digit)),
            str::from_utf8
        ),
        FromStr::from_str
    )
);

named!(identifier<String>,
    map_res!(
        map_res!(
            ws!(helpers::identifier),
            str::from_utf8
        ),
        FromStr::from_str
    )
);

struct Parser {
    tokens: Vec<Token>,
    position: usize,
    checkpoints: Vec<usize>,
    value_symbols: SymbolRegistry,
    type_symbols: SymbolRegistry,
}

type ParserResult<T> = Result<T, String>;

pub fn parse(tokens: Vec<Token>) -> ParserResult<(AST, SymbolRegistry, SymbolRegistry)> {
    debug!("Input: {:?}", tokens);
    Parser::new(tokens).parse()
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens,
            position: 0,
            checkpoints: vec![],
            value_symbols: SymbolRegistry::new(),
            type_symbols: SymbolRegistry::new(),
        }
    }

    fn parse(mut self) -> ParserResult<(AST, SymbolRegistry, SymbolRegistry)> {
        debug!("parse()");
        let output = self.many(Parser::parse_statement)?;
        if !self.empty() {
            return Err(format!("Remaining input: {:?}", self.tokens));
        }
        Ok((output, self.value_symbols, self.type_symbols))
    }

    fn parse_statement(&mut self) -> ParserResult<Statement> {
        debug!("parse_statement()");
        parse_first!(self()
            Parser::parse_application,
            Parser::parse_serializer
        )
    }

    fn parse_application(&mut self) -> ParserResult<Statement> {
        debug!("parse_application()");
        self.skip(&Token::Application());
        let name = self.parse_type_identifier()?;
        let body = self.until_end(Parser::parse_application_context)?;
        Ok(Statement::Application(name, body))
    }

    fn parse_application_context(&mut self) -> ParserResult<Statement> {
        debug!("parse_application_context()");
        parse_first!(self()
            Parser::parse_endpoint,
            Parser::parse_serializer,
            Parser::parse_item_function_call
        )
    }

    fn parse_endpoint(&mut self) -> ParserResult<Statement> {
        debug!("parse_endpoint()");
        self.skip(&Token::Endpoint())?;
        let name = self.parse_identifier()?;
        let args = self.parse_function_parameters()?;
        self.skip(&Token::RightArrow())?;
        let return_type = self.parse_type_identifier()?;
        let body = self.until_end(Parser::parse_expression)?;
        Ok(Statement::Endpoint(name, args, return_type, body))
    }

    fn parse_serializer(&mut self) -> ParserResult<Statement> {
        debug!("parse_serializer()");
        self.skip(&Token::Serializer())?;
        let name = self.parse_type_identifier()?;
        let body = self.until_end(Parser::parse_expression)?;
        Ok(Statement::Serializer(name, body))
    }

    fn parse_item_function_call(&mut self) -> ParserResult<Statement> {
        let name = self.parse_identifier()?;
        let params = self.parse_function_call_arguments()?;
        Ok(Statement::ItemFunctionCall(name, params))
    }

    fn parse_expression(&mut self) -> ParserResult<Expression> {
        debug!("parse_expression()");
        let mut e = parse_first!(self()
            Parser::parse_return,
            Parser::parse_function_call,
            Parser::parse_identifier_expression,
            Parser::parse_int_literal,
            Parser::parse_string_literal
        )?;

        loop {
            let result = self.parse_expression_with_predecessor(&e);
            if result.is_err() {
                break
            } else {
                e = result.unwrap();
            }
        }

        Ok(e)
    }

    fn parse_expression_with_predecessor(&mut self, e: &Expression)
        -> ParserResult<Expression>
    {
        debug!("parse_expression_with_predecessor({:?})", e);
        parse_first!(self(e)
            Parser::parse_method_call
        )
    }

    fn parse_int_literal(&mut self) -> ParserResult<Expression> {
        match self.consume()? {
            Token::IntLiteral(n) => Ok(Expression::IntLiteral(n)),
            token => self.error(token),
        }
    }

    fn parse_string_literal(&mut self) -> ParserResult<Expression> {
        match self.consume()? {
            Token::StringLiteral(s) => Ok(Expression::StringLiteral(s)),
            token => self.error(token),
        }
    }

    fn parse_return(&mut self) -> ParserResult<Expression> {
        self.skip(&Token::Return())?;
        let return_expression = self.parse_expression()?;
        Ok(Expression::Return(Box::new(return_expression)))
    }

    fn parse_method_call(&mut self, receiver: &Expression) -> ParserResult<Expression> {
        self.skip(&Token::Period())?;
        let name = self.parse_identifier()?;
        let params = self.parse_function_call_arguments()?;
        Ok(Expression::MethodCall(Box::new(receiver.clone()), name, params))
    }

    fn parse_function_call(&mut self) -> ParserResult<Expression> {
        let name = self.parse_identifier()?;
        let params = self.parse_function_call_arguments()?;
        Ok(Expression::FunctionCall(name, params))
    }

    fn parse_function_call_arguments(&mut self) -> ParserResult<Vec<Expression>> {
        self.skip(&Token::OpenParen())?;
        self.separated_with_until(
            Parser::parse_expression,
            Token::Comma(),
            Token::CloseParen(),
        )
    }

    fn parse_function_parameters(&mut self)
        -> ParserResult<Vec<FunctionParameter>>
    {
        debug!("parse_function_parameters()");
        self.skip(&Token::OpenParen())?;
        self.separated_with_until(
            Parser::parse_function_parameter,
            Token::Comma(),
            Token::CloseParen(),
        )
    }

    fn parse_function_parameter(&mut self)
        -> ParserResult<FunctionParameter>
    {
        debug!("parse_function_parameter()");
        let name = self.parse_identifier()?;
        self.skip(&Token::Colon())?;
        let type_name = self.parse_type_identifier()?;
        Ok(FunctionParameter::new(name, type_name))
    }

    fn parse_identifier(&mut self) -> ParserResult<Symbol> {
        debug!("parse_identifier()");
        match self.consume()? {
            Token::Identifier(name) => Ok(self.value_symbols.get_symbol(name)),
            token => self.error(token),
        }
    }

    fn parse_type_identifier(&mut self) -> ParserResult<Symbol> {
        debug!("parse_type_identifier()");
        match self.consume()? {
            Token::Identifier(name) => Ok(self.type_symbols.get_symbol(name)),
            token => self.error(token),
        }
    }

    fn parse_identifier_expression(&mut self) -> ParserResult<Expression> {
        debug!("parse_identifier_expression()");
        self.parse_identifier().map(Expression::Identifier)
    }

    fn consume(&mut self) -> ParserResult<Token> {
        if self.empty() {
            return Err("EOF".to_string());
        }
        let t = self.tokens[self.position].clone();
        self.position += 1;
        debug!("Consume: {:?}", t);
        Ok(t)
    }

    fn consume_n(&mut self, n: usize) -> ParserResult<Vec<Token>> {
        debug!("consume_n({:?})", n);
        let mut v = vec![];
        for _ in 0..n {
            v.push(self.consume()?);
        }
        Ok(v)
    }

    fn peek(&mut self) -> ParserResult<&Token> {
        if self.empty() {
            return Err("EOF".to_string());
        }
        let t = &self.tokens[self.position];
        debug!("Peek: {:?}", t);
        Ok(t)
    }

    fn peek_for(&mut self, t: &Token) -> bool {
        self.peek()
            .map(|peeked| peeked == t)
            .unwrap_or(false)
    }

    fn skip(&mut self, t: &Token) -> ParserResult<()> {
        debug!("skip({:?})", t);
        let found = self.peek()?.clone();
        if &found == t {
            self.consume();
            Ok(())
        } else {
            self.error_expected(found, t.clone())
        }
    }

    fn many<F, T>(&mut self, f: F) -> ParserResult<Vec<T>>
        where F: Fn(&mut Parser) -> ParserResult<T>
    {
        debug!("many()");
        let mut v = vec![];
        loop {
            if self.empty() {
                return Ok(v);
            }
            match f(self) {
                Ok(value) => v.push(value),
                Err(err) => {
                    debug!("many error: {:?}", err);
                    return Ok(v);
                },
            }
        }
    }

    fn many_until<F, T>(&mut self, f: F, t: Token) -> ParserResult<Vec<T>>
        where F: Fn(&mut Parser) -> ParserResult<T>
    {
        debug!("many_until()");
        let mut v = vec![];
        loop {
            if self.peek_for(&t) {
                self.consume();
                return Ok(v);
            }
            match f(self) {
                Ok(value) => {
                    v.push(value);
                },
                Err(e) => return Err(e),
            }
        }
    }

    fn separated_with_until<F, T>(&mut self, f: F, sep: Token, end: Token) -> ParserResult<Vec<T>>
        where F: Fn(&mut Parser) -> ParserResult<T>
    {
        debug!("separated_with_until({:?}, {:?})", sep, end);
        let mut v = vec![];
        loop {
            if self.peek_for(&end) {
                self.consume();
                return Ok(v);
            }
            match f(self) {
                Ok(value) => {
                    v.push(value);
                },
                Err(e) => return Err(e),
            };
            if self.peek_for(&end) {
                self.consume();
                return Ok(v);
            }
            self.skip(&sep)?;
        }
    }

    fn until_end<F, T>(&mut self, f: F) -> ParserResult<Vec<T>>
        where F: Fn(&mut Parser) -> ParserResult<T>
    {
        self.many_until(f, Token::End())
    }

    fn error<T>(&self, token: Token) -> ParserResult<T> {
        Err(format!("Unexpected token: {:?}", token))
    }

    fn error_expected<T>(&self, found: Token, expected: Token) -> ParserResult<T> {
        Err(format!("Expecting {:?}, found {:?}", expected, found))
    }

    fn checkpoint(&mut self) {
        self.checkpoints.push(self.position)
    }

    fn revert_checkpoint(&mut self) {
        self.position = self.checkpoints.pop().unwrap()
    }

    fn empty(&self) -> bool {
        self.tokens.len() - self.position <= 0
    }

    fn remaining(&self) -> Vec<Token> {
        self.tokens[self.position..].to_owned()
    }
}

fn check_input(input: &str, expected: AST) {
    let lexed = lex(input.as_bytes()).to_result().unwrap();
    let (parsed, _, _) = parse(lexed).unwrap();
    assert_eq!(parsed, expected);
}

#[test]
fn test_simple_application() {
    check_input(
        "application App end",
        vec![Statement::Application(0, vec![])]
    );
}

#[test]
fn test_application_with_simple_endpoint() {
    check_input(
        "application App endpoint index() -> String end end",
        vec![Statement::Application(
            0,
            vec![
                Statement::Endpoint(
                    0,
                    vec![],
                    1,
                    vec![],
                ),
            ]
        )]
    )
}

#[test]
fn test_application_with_single_arg_endpoint() {
    check_input(
        "
        application App
            endpoint get(id: Uuid) -> Model
            end
        end
        ",
        vec![Statement::Application(
            0,
            vec![
                Statement::Endpoint(
                    0,
                    vec![FunctionParameter::new(1, 1)],
                    2,
                    vec![],
                ),
            ]
        )]
    )
}

#[test]
fn test_application_with_multi_arg_endpoint() {
    check_input(
        "
        application App
            endpoint get(id: Uuid, other: String) -> Model
            end
        end
        ",
        vec![Statement::Application(
            0,
            vec![
                Statement::Endpoint(
                    0,
                    vec![
                        FunctionParameter::new(1, 1),
                        FunctionParameter::new(2, 2),
                    ],
                    3,
                    vec![],
                ),
            ]
        )]
    )
}


#[test]
fn test_application_with_endpoint_with_body() {
    check_input(
        "
        application App
            endpoint get(id: Uuid) -> Model
                return get(id)
            end
        end
        ",
        vec![Statement::Application(
            0,
            vec![
                Statement::Endpoint(
                    0,
                    vec![FunctionParameter::new(1, 1)],
                    2,
                    vec![
                        Expression::Return(
                            Box::new(Expression::FunctionCall(
                                0,
                                vec![Expression::Identifier(1)]
                            ))
                        )
                    ],
                ),
            ]
        )]
    )
}

#[test]
fn test_application_with_endpoint_with_multiline_body() {
    check_input(
        "
        application App
            endpoint get(id: Uuid) -> Model
                symbol
                call(function, 123, \"string\", func2(meh))
                return another()
            end
        end
        ",
        vec![Statement::Application(
            0,
            vec![
                Statement::Endpoint(
                    0,
                    vec![FunctionParameter::new(1, 1)],
                    2,
                    vec![
                        Expression::Identifier(2),
                        Expression::FunctionCall(
                            3,
                            vec![
                                Expression::Identifier(4),
                                Expression::IntLiteral(123),
                                Expression::StringLiteral("string".to_string()),
                                Expression::FunctionCall(
                                    5,
                                    vec![Expression::Identifier(6)]
                                )
                            ]
                        ),
                        Expression::Return(
                            Box::new(Expression::FunctionCall(
                                7,
                                vec![]
                            ))
                        )
                    ],
                ),
            ]
        )]
    )
}

#[test]
fn test_application_with_host_port() {
    check_input(
        "
        application App
            host(\"127.0.0.1\")
            port(8080)

            endpoint get(id: Uuid) -> Model
                return another()
            end
        end
        ",
        vec![Statement::Application(
            0,
            vec![
                Statement::ItemFunctionCall(
                    0,
                    vec![Expression::StringLiteral("127.0.0.1".to_string())]
                ),
                Statement::ItemFunctionCall(
                    1,
                    vec![Expression::IntLiteral(8080)],
                ),
                Statement::Endpoint(
                    2,
                    vec![FunctionParameter::new(3, 1)],
                    2,
                    vec![
                        Expression::Return(
                            Box::new(Expression::FunctionCall(
                                4,
                                vec![]
                            ))
                        )
                    ],
                ),
            ]
        )]
    )
}

#[test]
fn test_serializer_with_method_call() {
    env_logger::init().unwrap();
    check_input(
        "
        serializer String
            self.to_bytes(a, b)
        end
        ",
        vec![
            Statement::Serializer(
                0,
                vec![
                    Expression::MethodCall(
                        Box::new(Expression::Identifier(0)),
                        1,
                        vec![
                            Expression::Identifier(2),
                            Expression::Identifier(3),
                        ]
                    )
                ]
            )
        ]
   )
}

#[test]
fn test_serializer_with_chained_method_call() {
    check_input(
        "
        serializer String
            self.reverse().to_bytes(a, b)
        end
        ",
        vec![
            Statement::Serializer(
                0,
                vec![
                    Expression::MethodCall(
                        Box::new(
                            Expression::MethodCall(
                                Box::new(Expression::Identifier(0)),
                                1,
                                vec![]
                            )
                        ),
                        2,
                        vec![
                            Expression::Identifier(3),
                            Expression::Identifier(4),
                        ]
                    )
                ]
            )
        ]
   )
}

#[test]
fn test_endpoint_returns_method_call() {
    check_input(
        "
        application App
            endpoint show() -> String
                return \"Hello World!\".string_to_bytes()
            end
        end
        ",
        vec![
            Statement::Application(
                0,
                vec![
                    Statement::Endpoint(
                        0,
                        vec![],
                        1,
                        vec![
                            Expression::Return(
                                Box::new(
                                    Expression::MethodCall(
                                        Box::new(Expression::StringLiteral("Hello World!".to_string())),
                                        1,
                                        vec![],
                                    )
                                )
                            )
                        ]
                    )
                ]
            )
        ]
    )
}
