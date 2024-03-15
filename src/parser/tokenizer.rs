use super::locations::{Span, Location};
use const_format::concatcp;
use once_cell::sync::Lazy;
use regex::{Match, Regex};
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

#[derive(Clone, Default, Debug)]
pub struct Token {
    pub(crate) typ: TokenType,
    pub(crate) lexeme: String,
    pub(crate) span: Span,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}('{}')", self.typ, self.lexeme)
    }
}

#[allow(non_camel_case_types)]
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum TokenType {
    ENDMARKER,
    NAME,
    NUMBER,
    STRING,
    NEWLINE,
    INDENT,
    DEDENT,
    LPAR,
    RPAR,
    LSQB,
    RSQB,
    COLON,
    COMMA,
    SEMI,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    VBAR,
    AMPER,
    LESS,
    GREATER,
    EQUAL,
    DOT,
    PERCENT,
    LBRACE,
    RBRACE,
    EQEQUAL,
    NOTEQUAL,
    LESSEQUAL,
    GREATEREQUAL,
    TILDE,
    CIRCUMFLEX,
    LEFTSHIFT,
    RIGHTSHIFT,
    DOUBLESTAR,
    PLUSEQUAL,
    MINEQUAL,
    STAREQUAL,
    SLASHEQUAL,
    PERCENTEQUAL,
    AMPEREQUAL,
    VBAREQUAL,
    CIRCUMFLEXEQUAL,
    LEFTSHIFTEQUAL,
    RIGHTSHIFTEQUAL,
    DOUBLESTAREQUAL,
    DOUBLESLASH,
    DOUBLESLASHEQUAL,
    AT,
    ATEQUAL,
    RARROW,
    ELLIPSIS,
    COLONEQUAL,
    EXCLAMATION,
    TYPE_COMMENT,
    SOFT_KEYWORD,
    KEYWORD,
    FSTRING_START,
    FSTRING_MIDDLE,
    FSTRING_END,
    NL,
    ERRORTOKEN,
}

impl Default for TokenType {
    fn default() -> Self {
        Self::ERRORTOKEN
    }
}

const LEFTSHIFTEQUAL: (&str, TokenType) = ("<<=", TokenType::LEFTSHIFTEQUAL);
const RIGHTSHIFTEQUAL: (&str, TokenType) = (">>=", TokenType::RIGHTSHIFTEQUAL);
const DOUBLESTAREQUAL: (&str, TokenType) = ("**=", TokenType::DOUBLESTAREQUAL);
const DOUBLESLASHEQUAL: (&str, TokenType) = ("//=", TokenType::DOUBLESLASHEQUAL);
const CIRCUMFLEXEQUAL: (&str, TokenType) = ("^=", TokenType::CIRCUMFLEXEQUAL);
const ELLIPSIS: (&str, TokenType) = ("...", TokenType::ELLIPSIS);
const EQEQUAL: (&str, TokenType) = ("==", TokenType::EQEQUAL);
const NOTEQUAL: (&str, TokenType) = ("!=", TokenType::NOTEQUAL);
const LESSEQUAL: (&str, TokenType) = ("<=", TokenType::LESSEQUAL);
const GREATEREQUAL: (&str, TokenType) = (">=", TokenType::GREATEREQUAL);
const LEFTSHIFT: (&str, TokenType) = ("<<", TokenType::LEFTSHIFT);
const RIGHTSHIFT: (&str, TokenType) = (">>", TokenType::RIGHTSHIFT);
const DOUBLESTAR: (&str, TokenType) = ("**", TokenType::DOUBLESTAR);
const PLUSEQUAL: (&str, TokenType) = ("+=", TokenType::PLUSEQUAL);
const MINEQUAL: (&str, TokenType) = ("-=", TokenType::MINEQUAL);
const STAREQUAL: (&str, TokenType) = ("*=", TokenType::STAREQUAL);
const SLASHEQUAL: (&str, TokenType) = ("/=", TokenType::SLASHEQUAL);
const PERCENTEQUAL: (&str, TokenType) = ("%=", TokenType::PERCENTEQUAL);
const AMPEREQUAL: (&str, TokenType) = ("&=", TokenType::AMPEREQUAL);
const VBAREQUAL: (&str, TokenType) = ("|=", TokenType::VBAREQUAL);
const DOUBLESLASH: (&str, TokenType) = ("//", TokenType::DOUBLESLASH);
const ATEQUAL: (&str, TokenType) = ("@=", TokenType::ATEQUAL);
const RARROW: (&str, TokenType) = ("->", TokenType::RARROW);
const COLONEQUAL: (&str, TokenType) = (":=", TokenType::COLONEQUAL);
const LPAR: (&str, TokenType) = ("(", TokenType::LPAR);
const RPAR: (&str, TokenType) = (")", TokenType::RPAR);
const LSQB: (&str, TokenType) = ("[", TokenType::LSQB);
const RSQB: (&str, TokenType) = ("]", TokenType::RSQB);
const COLON: (&str, TokenType) = (":", TokenType::COLON);
const COMMA: (&str, TokenType) = (",", TokenType::COMMA);
const SEMI: (&str, TokenType) = (";", TokenType::SEMI);
const PLUS: (&str, TokenType) = ("+", TokenType::PLUS);
const MINUS: (&str, TokenType) = ("-", TokenType::MINUS);
const STAR: (&str, TokenType) = ("*", TokenType::STAR);
const SLASH: (&str, TokenType) = ("/", TokenType::SLASH);
const VBAR: (&str, TokenType) = ("|", TokenType::VBAR);
const AMPER: (&str, TokenType) = ("&", TokenType::AMPER);
const LESS: (&str, TokenType) = ("<", TokenType::LESS);
const GREATER: (&str, TokenType) = (">", TokenType::GREATER);
const EQUAL: (&str, TokenType) = ("=", TokenType::EQUAL);
const DOT: (&str, TokenType) = (".", TokenType::DOT);
const PERCENT: (&str, TokenType) = ("%", TokenType::PERCENT);
const LBRACE: (&str, TokenType) = ("{", TokenType::LBRACE);
const RBRACE: (&str, TokenType) = ("}", TokenType::RBRACE);
const TILDE: (&str, TokenType) = ("~", TokenType::TILDE);
const CIRCUMFLEX: (&str, TokenType) = ("^", TokenType::CIRCUMFLEX);
const AT: (&str, TokenType) = ("@", TokenType::AT);
const EXCLAMATION: (&str, TokenType) = ("!", TokenType::EXCLAMATION);

const SIMPLE_TOKENS: [(&str, TokenType); 48] = [
    LEFTSHIFTEQUAL,
    RIGHTSHIFTEQUAL,
    DOUBLESTAREQUAL,
    DOUBLESLASHEQUAL,
    CIRCUMFLEXEQUAL,
    ELLIPSIS,
    EQEQUAL,
    NOTEQUAL,
    LESSEQUAL,
    GREATEREQUAL,
    LEFTSHIFT,
    RIGHTSHIFT,
    DOUBLESTAR,
    PLUSEQUAL,
    MINEQUAL,
    STAREQUAL,
    SLASHEQUAL,
    PERCENTEQUAL,
    AMPEREQUAL,
    VBAREQUAL,
    DOUBLESLASH,
    ATEQUAL,
    RARROW,
    COLONEQUAL,
    LPAR,
    RPAR,
    LSQB,
    RSQB,
    COLON,
    COMMA,
    SEMI,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    VBAR,
    AMPER,
    LESS,
    GREATER,
    EQUAL,
    DOT,
    PERCENT,
    LBRACE,
    RBRACE,
    TILDE,
    CIRCUMFLEX,
    AT,
    EXCLAMATION,
];

macro_rules! alternative {
    ($t:expr) => {{
        $t
    }};
    ($t:expr, $($ts:expr),+) => {{
        concatcp!($t, "|", alternative!($($ts),+))
    }}
}

macro_rules! group {
    ($($ts:expr),+) => {{
        concatcp!(r"(", alternative!($($ts),+), ")")
    }}
}

macro_rules! maybe {
    ($($ts:expr),+) => {
        concatcp!(group!($($ts),+), r"?")
    }
}

const S_WHITESPACE: &str = r"^[ \f\t]+";
const S_COMMENT: &str = r"^#[^\r\n]*";
const S_NAME: &str = r"^\w+";
const S_HEXNUMBER: &str = r"0[xX](?:_?[0-9a-fA-F])+";
const S_BINNUMBER: &str = r"0[bB](?:_?[01])+";
const S_OCTNUMBER: &str = r"0[oO](?:_?[0-7])+";
const S_DECNUMBER: &str = r"(?:0(?:_?0)*|[1-9](?:_?[0-9])*)";
const S_WHOLE_HEXNUMBER: &str = concatcp!("^", S_HEXNUMBER, "$");
const S_WHOLE_BINNUMBER: &str = concatcp!("^", S_BINNUMBER, "$");
const S_WHOLE_OCTNUMBER: &str = concatcp!("^", S_OCTNUMBER, "$");
const S_WHOLE_DECNUMBER: &str = concatcp!("^", S_DECNUMBER, "$");
const S_INTNUMBER: &str = group!(S_HEXNUMBER, S_BINNUMBER, S_OCTNUMBER, S_DECNUMBER);
const S_EXPONENT: &str = r"[eE][-+]?[0-9](?:_?[0-9])*";
const S_POINTFLOAT: &str = concatcp!(
    group!(
        r"^[0-9](?:_?[0-9])*\.(?:[0-9](?:_?[0-9])*)?",
        r"^\.[0-9](?:_?[0-9])*"
    ),
    maybe!(S_EXPONENT)
);
const S_EXPFLOAT: &str = concatcp!(r" [0-9](?:_?[0-9])*", S_EXPONENT);
const S_IMFLOAT: &str = concatcp!(S_FLOATNUMBER, r"[jJ]");
const S_FLOATNUMBER: &str = group!(S_POINTFLOAT, S_EXPFLOAT);
const S_IMAGNUMBER: &str = group!(r"[0-9](?:_?[0-9])*[jJ]", S_IMFLOAT);
const S_WHOLE_FLOATNUMBER: &str = concatcp!("^", S_FLOATNUMBER, "$");
const S_WHOLE_IMAGNUMBER: &str = concatcp!("^", S_IMAGNUMBER, "$");
const S_NUMBER: &str = concatcp!(r"^", group!(S_IMAGNUMBER, S_FLOATNUMBER, S_INTNUMBER));
const S_KEYWORDS: &str = r"^(\bFalse\b|\bNone\b|\bTrue\b|\band\b|\bas\b|\bassert\b|\basync\b|\bawait\b|\bbreak\b|\bclass\b|\bcontinue\b|\bdef\b|\bdel\b|\belif\b|\belse\b|\bexcept\b|\bfinally\b|\bfor\b|\bfrom\b|\bglobal\b|\bif\b|\bimport\b|\bin\b|\bis\b|\blambda\b|\bnonlocal\b|\bnot\b|\bor\b|\bpass\b|\braise\b|\breturn\b|\btry\b|\bwhile\b|\bwith\b|\byield\b)";
const S_SOFT_KEYWORDS: &str = r"^(match\b|\bcase\b|\btype\b|\b_\b)";
const S_STRING_START: &str = r#"^("{3}|'{3}|"{1}|'{1})"#;

static WHITESPACE: Lazy<Regex> =
    Lazy::new(|| Regex::new(S_WHITESPACE).expect("Error compiling regex."));
static COMMENT: Lazy<Regex> = Lazy::new(|| Regex::new(S_COMMENT).expect("Error compiling regex."));
static NAME: Lazy<Regex> = Lazy::new(|| Regex::new(S_NAME).expect("Error compiling regex."));
pub(super) static HEXNUMBER: Lazy<Regex> =
    Lazy::new(|| Regex::new(S_WHOLE_HEXNUMBER).expect("Error compiling regex."));
pub(super) static BINNUMBER: Lazy<Regex> =
    Lazy::new(|| Regex::new(S_WHOLE_BINNUMBER).expect("Error compiling regex."));
pub(super) static OCTNUMBER: Lazy<Regex> =
    Lazy::new(|| Regex::new(S_WHOLE_OCTNUMBER).expect("Error compiling regex."));
pub(super) static DECNUMBER: Lazy<Regex> =
    Lazy::new(|| Regex::new(S_WHOLE_DECNUMBER).expect("Error compiling regex."));
pub(super) static FLOATNUMBER: Lazy<Regex> =
    Lazy::new(|| Regex::new(S_WHOLE_FLOATNUMBER).expect("Error compiling regex."));
pub(super) static IMAGNUMBER: Lazy<Regex> =
    Lazy::new(|| Regex::new(S_WHOLE_IMAGNUMBER).expect("Error compiling regex."));
static NUMBER: Lazy<Regex> = Lazy::new(|| Regex::new(S_NUMBER).expect("Error compiling regex."));
static KEYWORDS: Lazy<Regex> =
    Lazy::new(|| Regex::new(S_KEYWORDS).expect("Error compiling regex."));
static SOFT_KEYWORDS: Lazy<Regex> =
    Lazy::new(|| Regex::new(S_SOFT_KEYWORDS).expect("Error compiling regex."));
static STRING_START: Lazy<Regex> =
    Lazy::new(|| Regex::new(S_STRING_START).expect("Error compiling regex."));

enum StringDelimiter {
    SingleQuotes,
    DoubleQuotes,
    MultilineSingleQuotes,
    MultilineDoubleQuotes,
    None,
}

impl StringDelimiter {
    fn from_match(m: Match) -> Self {
        let double = m.as_str().chars().nth(0) == Some('\"');
        if m.end() == 1 {
            if double {
                Self::DoubleQuotes
            } else {
                Self::SingleQuotes
            }
        } else if double {
            Self::MultilineDoubleQuotes
        } else {
            Self::MultilineSingleQuotes
        }
    }
    fn matching_end(&self) -> (char, usize) {
        match self {
            Self::SingleQuotes => ('\'', 1),
            Self::MultilineSingleQuotes => ('\'', 3),
            Self::DoubleQuotes => ('"', 1),
            Self::MultilineDoubleQuotes => ('"', 3),
            Self::None => unreachable!(),
        }
    }
}

pub struct Tokenizer {
    tokens: Vec<Token>,
    current: Token,
    start: usize,
    end: usize,
    paren_lvl: isize,
    indent: Vec<usize>,
    in_string: bool,
    current_string: String,
    string_start: StringDelimiter,
    tokens_added: usize,
}

impl Tokenizer {
    pub fn new() -> Result<Self, regex::Error> {
        let tokens = vec![];
        let current = Token::default();
        Ok(Self {
            tokens,
            current,
            start: 0,
            end: 0,
            paren_lvl: 0,
            indent: vec![0],
            in_string: false,
            current_string: String::new(),
            string_start: StringDelimiter::None,
            tokens_added: 0,
        })
    }
    pub fn tokenize(&mut self, input: impl Iterator<Item = String>) -> ParserState {
        for (lineno, line) in input.enumerate() {
            // println!("line {lineno}:");
            match self.tokenize_line(line.as_str(), lineno) {
                Ok(()) => continue,
                Err(s) => return ParserState::Error(s),
            }
        }
        if self.in_string || self.paren_lvl > 0 {
            return ParserState::ContinuationNeeded;
        }
        ParserState::Ok
    }
    pub fn extract(self) -> Result<Vec<Token>, String> {
        Ok(self.tokens)
    }
    pub fn finalize(mut self) -> Result<Vec<Token>, String> {
        let lvl = self
            .tokens
            .iter()
            .filter(|t| t.typ == TokenType::INDENT)
            .count()
            - self
                .tokens
                .iter()
                .filter(|t| t.typ == TokenType::DEDENT)
                .count();
        let span = self.current.span.clone();
        for _ in 0..lvl {
            self.tokens.push(Token {
                typ: TokenType::DEDENT,
                span: span.clone(),
                lexeme: "".to_string(),
            });
        }
        self.tokens.push(Token {
            typ: TokenType::ENDMARKER,
            span,
            lexeme: "".to_string(),
        });
        Ok(self.tokens)
    }
    fn tokenize_line(&mut self, line: &str, lineno: usize) -> Result<(), String> {
        self.start = 0;
        self.end = 1;
        self.tokens_added = 0;

        while self.end <= line.len() {
            if !self.in_string {
                if COMMENT.is_match(&line[self.start..]) {
                    break;
                }

                if let Some(m) = WHITESPACE.find(&line[self.start..]) {
                    let mut current_indent = *self.indent.last().unwrap();
                    if self.start == 0 && m.end() != current_indent && self.paren_lvl == 0 {
                        if m.end() > current_indent {
                            self.current.typ = TokenType::INDENT;
                            self.indent.push(m.end());
                            self.current.lexeme = m.as_str().to_string();
                            self.end = m.end();
                            self.current.span = Span::new(lineno, self.start, lineno, self.end);
                            self.push();
                        } else {
                            while current_indent > m.end() {
                                self.current.typ = TokenType::DEDENT;
                                self.current.lexeme = "".to_string();
                                self.end = m.end();
                                self.current.span = Span::new(lineno, self.start, lineno, self.end);
                                self.push();
                                self.indent.pop().unwrap();
                                current_indent = *self.indent.last().unwrap();
                            }
                            if current_indent < m.end() {
                                return Err(format!("Parser error: indentation level of block starting on line {lineno} does not match any previous indentation level."));
                            }
                        };
                    } else {
                        self.advance(m.end() - m.start());
                    }
                    continue;
                }
                if self.start == 0 && !self.indent.is_empty() {
                    let dedents = self.indent.len() - 1;
                    for _ in 0..dedents {
                        self.current.typ = TokenType::DEDENT;
                        self.current.span = Span::new(lineno, 0, lineno, 0);
                        self.end = 0;
                        self.push();
                        self.indent.pop().unwrap();
                    }
                }

                if self.find_by_regex(&KEYWORDS, TokenType::KEYWORD, line, lineno) {
                    continue;
                }
                if self.find_by_regex(&SOFT_KEYWORDS, TokenType::SOFT_KEYWORD, line, lineno) {
                    continue;
                }
                if self.find_by_regex(&NUMBER, TokenType::NUMBER, line, lineno) {
                    continue;
                }
                if self.find_by_regex(&NAME, TokenType::NAME, line, lineno) {
                    continue;
                }

                for (lexeme, tok_type) in SIMPLE_TOKENS {
                    if line[self.start..].starts_with(lexeme) {
                        self.current.typ = tok_type;
                        self.current.lexeme = lexeme.to_string();
                        self.end = self.start + lexeme.len();
                        self.current.span = Span::new(lineno, self.start, lineno, self.end);
                        self.push();
                        if tok_type == TokenType::LPAR
                            || tok_type == TokenType::LSQB
                            || tok_type == TokenType::LBRACE
                        {
                            self.paren_lvl += 1;
                        } else if tok_type == TokenType::RPAR
                            || tok_type == TokenType::RSQB
                            || tok_type == TokenType::RBRACE
                        {
                            self.paren_lvl -= 1;
                        }
                        break;
                    }
                }
                if let Some(m) = STRING_START.find(&line[self.start..]) {
                    self.string_start = StringDelimiter::from_match(m);
                    self.current_string = "".to_string();
                    self.in_string = true;
                    self.current.typ = TokenType::STRING;
                    self.current.span.start(Location {
                        line: lineno,
                        column: self.start,
                    });
                    self.start += m.end();
                    self.end += m.end();
                    continue;
                }
            } else {
                let mut quotes = "".to_string();
                let mut escaped = false;
                let (string_end, tok_len) = self.string_start.matching_end();
                for chr in line[self.start..].chars() {
                    self.end += chr.len_utf8();
                    if escaped || chr != string_end && chr != '\\' {
                        self.current_string.push(chr);
                        quotes.clear();
                        escaped = false;
                        continue;
                    }
                    if chr == string_end && quotes.len() < tok_len {
                        quotes.push(chr);
                    }
                    if chr == string_end && quotes.len() == tok_len {
                        self.in_string = false;
                        self.end -= tok_len;
                        self.current.span.end(Location {
                            line: lineno,
                            column: self.end,
                        });
                        self.current.lexeme = self.current_string.clone();
                        self.push();
                        break;
                    }
                    escaped = chr == '\\';
                    self.current_string.push(chr);
                }
            }
        }
        if self.tokens_added > 0 && !self.in_string {
            let mut token = Token::default();
            token.span = Span::new(lineno, self.start, lineno, self.end);
            token.lexeme = "".to_string();
            token.typ = if self.paren_lvl == 0 {
                TokenType::NEWLINE
            } else {
                TokenType::NL
            };
            self.tokens.push(token);
        }
        Ok(())
    }

    fn advance(&mut self, len: usize) {
        self.start += len;
        self.end = self.start + 1;
    }

    fn push(&mut self) {
        self.start = self.end;
        self.end += 1;
        self.tokens_added += 1;
        self.tokens.push(self.current.clone());
        // println!("{}", self.current);
        self.current = Token::default();
    }

    fn find_by_regex(
        &mut self,
        regex: &Regex,
        token_type: TokenType,
        line: &str,
        lineno: usize,
    ) -> bool {
        if let Some(m) = regex.find(&line[self.start..]) {
            self.current.typ = token_type;
            self.current.lexeme = m.as_str().to_string();
            self.end = self.start + m.end();
            self.current.span = Span::new(lineno, self.start, lineno, self.end);
            self.push();
            return true;
        }
        false
    }
}

pub fn tokenize_file<P>(path: P) -> Result<Vec<Token>, String>
where
    P: AsRef<Path> + std::fmt::Display,
{
    if let Ok(lines) = read_lines(&path) {
        let mut tokenizer = Tokenizer::new().expect("Could not build tokenizer.");
        tokenizer.tokenize(lines.flatten());
        tokenizer.finalize()
    } else {
        Err(format!("{} not found. No such file or directory.", &path))
    }
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

pub enum ParserState {
    Ok,
    ContinuationNeeded,
    Error(String),
}
