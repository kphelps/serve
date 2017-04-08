use nom::{
    AsChar,
    ErrorKind,
    InputIter,
    InputLength,
    IResult,
    is_alphabetic,
    is_alphanumeric,
    Needed,
    Slice,
};

use std::ops::{Range, RangeTo, RangeFrom};

named!(pub space, eat_separator!(&b" \t"[..]));

#[macro_export]
macro_rules! sp (
    ($i:expr, $($args:tt)*) => (
        {
            use $crate::helpers::space;
            sep!($i, space, $($args)*)
        }
    )
);

#[macro_export]
macro_rules! punct {
    ($i:expr, $punct:expr) => {
        sp!($i, tag!($punct))
    };
}

macro_rules! kw0 {
    ($i:expr, $word:expr, $ctor:ident) => {
        map!($i, punct!($word), |_| Token::$ctor())
    }
}

macro_rules! kw1 {
    ($i:expr, $word:expr, $ctor:ident) => {
        map!($i, call!($word), Token::$ctor)
    }
}

macro_rules! parse_first {
    ($parser:ident($($args:expr),*) $($subparsers:expr),+) => {{
        let mut result = Err(format!("Could not match subparsers, remaining: {:?}", $parser.remaining()));
        for parser in [$($subparsers),+].iter() {
            $parser.checkpoint();
            let parser_result = parser($parser, $($args),*);
            if parser_result.is_ok() {
                result = parser_result;
                break;
            }
            $parser.revert_checkpoint();
        };
        result
    }}
}

fn is_valid_identifier_char(index: usize, chr: u8) -> bool {
    if index == 0 {
        is_alphabetic(chr)
    } else {
        is_alphanumeric(chr)
            || chr == 0x5F // _
            || chr == 0x3F // ?
            || chr == 0x21 // !
    }
}

pub fn identifier<T>(input: T) -> IResult<T, T> where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength
{
    let input_length = input.input_len();
    if input_length == 0 {
        return IResult::Incomplete(Needed::Unknown);
    }

    for (idx, item) in input.iter_indices() {
        if !is_valid_identifier_char(idx, item.as_char() as u8) {
            if idx == 0 {
                return IResult::Error(error_position!(ErrorKind::AlphaNumeric, input))
            } else {
                return IResult::Done(input.slice(idx..), input.slice(0..idx))
            }
        }
    }
    IResult::Done(input.slice(input_length..), input)
}

#[test]
fn test_valid_identifiers() {
    assert_eq!(identifier("hello"), IResult::Done("", "hello"));
    assert_eq!(identifier("hello_world"), IResult::Done("", "hello_world"));
    assert_eq!(identifier("hello_world!"), IResult::Done("", "hello_world!"));
    assert_eq!(identifier("h?ello123"), IResult::Done("", "h?ello123"));
}
