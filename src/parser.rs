use nom::{alphanumeric, IResult, is_digit};
use std::str;
use std::str::FromStr;
use std::string::String;

#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    FunctionCall(String, Vec<Expression>),
    Return(Box<Expression>),
    IntLiteral(i64),
    StringLiteral(String),
    Identifier(String),
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionParameter {
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
pub enum Statement {
    Application(String, Vec<Statement>),
    Endpoint(String, Vec<FunctionParameter>, String, Vec<Expression>)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
    Application(),
    Endpoint(),
    Return(),
    End(),
    RightArrow(),
    OpenParen(),
    CloseParen(),
    Comma(),
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
        kw0!("return", Return) |
        kw0!("end", End) |
        kw0!("->", RightArrow) |
        kw0!("(", OpenParen) |
        kw0!(")", CloseParen) |
        kw0!(",", Comma) |
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
            ws!(alphanumeric),
            str::from_utf8
        ),
        FromStr::from_str
    )
);

struct Parser {
    tokens: Vec<Token>,
    position: usize,
    checkpoints: Vec<usize>,
}

type ParserResult<T> = Result<T, String>;

pub fn parse(tokens: Vec<Token>) -> ParserResult<Vec<Statement>> {
    println!("Input: {:?}", tokens);
    Parser::new(tokens).parse()
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens,
            position: 0,
            checkpoints: vec![],
        }
    }

    fn parse(&mut self) -> ParserResult<Vec<Statement>> {
        println!("parse()");
        let output = self.many(Parser::parse_statement)?;
        if !self.empty() {
            return Err(format!("Remaining input: {:?}", self.tokens));
        }
        Ok(output)
    }

    fn parse_statement(&mut self) -> ParserResult<Statement> {
        println!("parse_statement()");
        match self.consume()? {
            Token::Application() => self.parse_application(),
            token => self.error(token),
        }
    }

    fn parse_application_context(&mut self) -> ParserResult<Statement> {
        println!("parse_application_context()");
        match self.consume()? {
            Token::Endpoint() => self.parse_endpoint(),
            token => self.error(token),
        }
    }

    fn parse_application(&mut self) -> ParserResult<Statement> {
        println!("parse_application()");
        let name = self.parse_identifier()?;
        let body = self.until_end(Parser::parse_application_context)?;
        Ok(Statement::Application(name, body))
    }

    fn parse_endpoint(&mut self) -> ParserResult<Statement> {
        println!("parse_endpoint()");
        let name = self.parse_identifier()?;
        let args = self.parse_function_parameters()?;
        self.skip(&Token::RightArrow())?;
        let return_type = self.parse_identifier()?;
        let body = self.until_end(Parser::parse_expression)?;
        Ok(Statement::Endpoint(name, args, return_type, body))
    }

    fn parse_expression(&mut self) -> ParserResult<Expression> {
        println!("parse_expression()");
        parse_first!(self,
            Parser::parse_return,
            Parser::parse_function_call,
            Parser::parse_identifier_expression,
            Parser::parse_int_literal,
            Parser::parse_string_literal
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
        println!("parse_function_parameters()");
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
        println!("parse_function_parameter()");
        let name = self.parse_identifier()?;
        self.skip(&Token::Colon())?;
        let type_name = self.parse_identifier()?;
        Ok(FunctionParameter::new(&name, &type_name))
    }

    fn parse_identifier(&mut self) -> ParserResult<String> {
        println!("parse_identifier()");
        match self.consume()? {
            Token::Identifier(name) => Ok(name),
            token => self.error(token),
        }
    }

    fn parse_identifier_expression(&mut self) -> ParserResult<Expression> {
        println!("parse_identifier_expression()");
        self.parse_identifier().map(Expression::Identifier)
    }

    fn consume(&mut self) -> ParserResult<Token> {
        if self.empty() {
            return Err("EOF".to_string());
        }
        let t = self.tokens[self.position].clone();
        self.position += 1;
        println!("Consume: {:?}", t);
        Ok(t)
    }

    fn consume_n(&mut self, n: usize) -> ParserResult<Vec<Token>> {
        println!("consume_n({:?})", n);
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
        println!("Peek: {:?}", t);
        Ok(t)
    }

    fn peek_for(&mut self, t: &Token) -> bool {
        self.peek()
            .map(|peeked| peeked == t)
            .unwrap_or(false)
    }

    fn skip(&mut self, t: &Token) -> ParserResult<()> {
        println!("skip({:?})", t);
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
        println!("many()");
        let mut v = vec![];
        loop {
            if self.empty() {
                return Ok(v);
            }
            match f(self) {
                Ok(value) => v.push(value),
                Err(err) => {
                    println!("many error: {:?}", err);
                    return Ok(v);
                },
            }
        }
    }

    fn many_until<F, T>(&mut self, f: F, t: Token) -> ParserResult<Vec<T>>
        where F: Fn(&mut Parser) -> ParserResult<T>
    {
        println!("many_until()");
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
        println!("separated_with_until({:?}, {:?})", sep, end);
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

fn check_input(input: &str, expected: Vec<Statement>) {
    let lexed = lex(input.as_bytes()).to_result().unwrap();
    let parsed = parse(lexed).unwrap();
    assert_eq!(parsed, expected);
}

#[test]
fn test_simple_application() {
    check_input(
        "application App end",
        vec![Statement::Application("App".to_string(), vec![])]
    );
}

#[test]
fn test_application_with_simple_endpoint() {
    check_input(
        "application App endpoint index() -> String end end",
        vec![Statement::Application(
            "App".to_string(),
            vec![
                Statement::Endpoint(
                    "index".to_string(),
                    vec![],
                    "String".to_string(),
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
            "App".to_string(),
            vec![
                Statement::Endpoint(
                    "get".to_string(),
                    vec![FunctionParameter::new("id", "Uuid")],
                    "Model".to_string(),
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
            "App".to_string(),
            vec![
                Statement::Endpoint(
                    "get".to_string(),
                    vec![
                        FunctionParameter::new("id", "Uuid"),
                        FunctionParameter::new("other", "String"),
                    ],
                    "Model".to_string(),
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
            "App".to_string(),
            vec![
                Statement::Endpoint(
                    "get".to_string(),
                    vec![FunctionParameter::new("id", "Uuid")],
                    "Model".to_string(),
                    vec![
                        Expression::Return(
                            Box::new(Expression::FunctionCall(
                                "get".to_string(),
                                vec![Expression::Identifier("id".to_string())]
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
            "App".to_string(),
            vec![
                Statement::Endpoint(
                    "get".to_string(),
                    vec![FunctionParameter::new("id", "Uuid")],
                    "Model".to_string(),
                    vec![
                        Expression::Identifier("symbol".to_string()),
                        Expression::FunctionCall(
                            "call".to_string(),
                            vec![
                                Expression::Identifier("function".to_string()),
                                Expression::IntLiteral(123),
                                Expression::StringLiteral("string".to_string()),
                                Expression::FunctionCall(
                                    "func2".to_string(),
                                    vec![Expression::Identifier("meh".to_string())]
                                )
                            ]
                        ),
                        Expression::Return(
                            Box::new(Expression::FunctionCall(
                                "another".to_string(),
                                vec![]
                            ))
                        )
                    ],
                ),
            ]
        )]
    )
}
