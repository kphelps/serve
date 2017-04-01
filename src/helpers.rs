
#[macro_export]
macro_rules! punct {
    ($i:expr, $punct:expr) => {
        ws!($i, tag!($punct))
    };
}

macro_rules! keyword {
    ($i:expr, $word:expr) => {
        punct!($i, $word)
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
    ($parser:expr, $($subparsers:expr),+) => {{
        for parser in [$($subparsers),+].iter() {
            $parser.checkpoint();
            let result = parser($parser);
            if result.is_ok() {
                return result
            }
            $parser.revert_checkpoint();
        };
        Err(format!("Could not match subparsers, remaining: {:?}", $parser.remaining()))
    }}
}
