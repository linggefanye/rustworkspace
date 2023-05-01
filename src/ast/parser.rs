use glob::glob;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{
        alpha1, alphanumeric1, anychar, digit1, hex_digit1, multispace0,
        none_of, one_of, space0,
    },
    combinator::{opt, recognize},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};
use std::{fs::read_to_string, path::PathBuf};

use crate::{
    ASTInt, ASTNode, ASTString, CallNode, ASTField, Ident, IntFlagsNode, IntFmt,
    Pos, ResourceNode, StrFlagsNode, StrFmt, StructNode, ASTType, TypeDefNode,
};

#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    OutOfData,
    NotAnIdent,
    NotAnInteger,
    IntegerParseError,
    NotAChar,
    CharParseError,
    NotAnInt,
    IntParseError,
    NotAString,
    StringParseError,
    NotAComment,
    NotAType,
    TypeParseError,
    NotAField,
    FieldParseError,
    NotAFlags,
    FlagsParseError,
    NotACall,
    CallParseError,
    NotAResource,
    ResourceParseError,
    NotATypeDef,
    TypeDefParseError,
    NotAStruct,
    StructParseError,
    NotAUnion,
    UnionParseError,
}

impl TryFrom<nom::Err<ParserError>> for ParserError {
    type Error = ();
    fn try_from(value: nom::Err<ParserError>) -> Result<Self, Self::Error> {
        match value {
            nom::Err::Error(err_code) => Ok(err_code),
            _ => Err(()),
        }
    }
}

impl Into<nom::Err<ParserError>> for ParserError {
    fn into(self) -> nom::Err<ParserError> {
        nom::Err::Error(self)
    }
}

pub struct Parser {
    files: Option<Vec<PathBuf>>,
    pos: Pos,
}

impl Parser {
    pub fn new(paths: Option<&str>) -> Parser {
        if let Some(glob_str) = paths {
            let entries = glob(glob_str).expect("failed to parse glob pattern");
            let mut path_vec: Vec<PathBuf> = Vec::new();
            for entry in entries {
                if let Ok(p) = entry {
                    path_vec.push(p);
                }
            }
            if path_vec.is_empty() {
                panic!("No files matched by glob {glob_str}");
            }
            Parser {
                files: Some(path_vec),
                pos: Pos {
                    file: PathBuf::new(),
                    line: 0,
                    col: 0,
                }
            }
        } else {
            Parser {
                files: None,
                pos: Pos {
                    file: PathBuf::new(),
                    line: 0,
                    col: 0,
                }
            }
        }
    }

    pub fn parse_ident<'a>(
        &'a self,
        input: &'a str,
    ) -> IResult<&str, Ident, ParserError> {
        let res = recognize::<_, _, nom::error::Error<_>, _>(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        ))(input);
        if let Ok((out, ident_str)) = res {
            Ok((
                out,
                Ident {
                    pos: self.pos.clone(),
                    name: ident_str.to_owned(),
                },
            ))
        } else {
            Err(ParserError::NotAnIdent.into())
        }
    }

    pub fn parse_integer<'a>(
        &'a self,
        input: &'a str,
    ) -> IResult<&str, ASTInt, ParserError> {
        let res = recognize::<_, _, nom::error::Error<_>, _>(pair(
            alt((digit1, tag("-"))),
            many0(alt((hex_digit1, tag("x")))),
        ))(input);
        if let Ok((out, integer_str)) = res {
            // validate the integer
            if integer_str.starts_with('-') {
                if let Ok(i) = integer_str.parse::<i64>() {
                    Ok((
                        out,
                        ASTInt {
                            pos: self.pos.clone(),
                            value: Some((i as u64, IntFmt::Neg)),
                            ident: None,
                            cexpr: None,
                        },
                    ))
                } else {
                    Err(ParserError::IntegerParseError.into())
                }
            } else if integer_str.starts_with("0x") {
                if let Ok(i) = u64::from_str_radix(
                    integer_str.trim_start_matches("0x"),
                    16,
                ) {
                    Ok((
                        out,
                        ASTInt {
                            pos: self.pos.clone(),
                            value: Some((i, IntFmt::Hex)),
                            ident: None,
                            cexpr: None,
                        },
                    ))
                } else {
                    Err(ParserError::IntegerParseError.into())
                }
            } else {
                if let Ok(i) = integer_str.parse::<u64>() {
                    Ok((
                        out,
                        ASTInt {
                            pos: self.pos.clone(),
                            value: Some((i, IntFmt::Dec)),
                            ident: None,
                            cexpr: None,
                        },
                    ))
                } else {
                    Err(ParserError::IntegerParseError.into())
                }
            }
        } else {
            Err(ParserError::NotAnInteger.into())
        }
    }

    pub fn parse_char<'a>(
        &'a self,
        input: &'a str,
    ) -> IResult<&str, ASTInt, ParserError> {
        let res = delimited::<_, _, _, _, nom::error::Error<_>, _, _, _>(
            tag("\'"),
            anychar,
            tag("\'"),
        )(input);
        if let Ok((output, the_char)) = res {
            if the_char == '\r' || the_char == '\n' {
                Err(ParserError::CharParseError.into())
            } else {
                Ok((
                    output,
                    ASTInt {
                        pos: self.pos.clone(),
                        value: Some((the_char as u64, IntFmt::Char)),
                        ident: None,
                        cexpr: None,
                    },
                ))
            }
        } else {
            Err(ParserError::NotAChar.into())
        }
    }

    pub fn parse_int<'a>(
        &'a self,
        input: &'a str,
    ) -> IResult<&str, ASTInt, ParserError> {
        let res = self.parse_integer(input);
        if res.is_ok() {
            return res;
        }
        let err_code: ParserError = res.unwrap_err().try_into().unwrap();
        match err_code {
            ParserError::NotAnInteger => (),
            ParserError::IntegerParseError => {
                return Err(ParserError::IntParseError.into());
            }
            _ => unreachable!(),
        }

        let res = self.parse_char(input);
        if res.is_ok() {
            return res;
        }
        let err_code: ParserError = res.unwrap_err().try_into().unwrap();
        match err_code {
            ParserError::NotAChar => Err(ParserError::NotAnInt.into()),
            ParserError::CharParseError => {
                Err(ParserError::IntParseError.into())
            }
            _ => unreachable!(),
        }
    }

    pub fn decode_hex_str(input: &str) -> Vec<u8> {
        (0..input.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&input[i..i + 2], 16).unwrap())
            .collect::<Vec<u8>>()
    }

    pub fn parse_string<'a>(
        &'a self,
        input: &'a str,
    ) -> IResult<&str, ASTString, ParserError> {
        let res = input.chars().next();
        if let None = res {
            return Err(ParserError::NotAString.into());
        }
        let closing = res.unwrap();
        if closing != '\"' && closing != '`' {
            return Err(ParserError::NotAString.into());
        }
        let is_hex = closing == '`';
        if is_hex {
            let res = delimited::<_, _, _, _, nom::error::Error<_>, _, _, _>(
                tag("`"),
                hex_digit1,
                tag("`"),
            )(input);
            if let Ok((output, str_str)) = res {
                if str_str.len() & 2 == 0 {
                    let r = Parser::decode_hex_str(str_str);
                    Ok((
                        output,
                        ASTString {
                            pos: self.pos.clone(),
                            value: r,
                            fmt: StrFmt::Hex,
                        },
                    ))
                } else {
                    Err(ParserError::StringParseError.into())
                }
            } else {
                Err(ParserError::StringParseError.into())
            }
        } else {
            let res = delimited::<_, _, _, _, nom::error::Error<_>, _, _, _>(
                tag("\""),
                recognize(many1(none_of("\""))),
                tag("\""),
            )(input);
            if let Ok((output, str_str)) = res {
                Ok((
                    output,
                    ASTString {
                        pos: self.pos.clone(),
                        value: str_str.as_bytes().to_owned(),
                        fmt: StrFmt::Raw,
                    },
                ))
            } else {
                Err(ParserError::StringParseError.into())
            }
        }
    }

    pub fn parse_comment<'a>(
        &'a self,
        input: &'a str,
    ) -> IResult<&str, (), ParserError> {
        let res = recognize::<_, _, nom::error::Error<_>, _>(tuple((
            tag("#"),
            many0(none_of("\n")),
            tag("\n"),
        )))(input);
        if let Ok((output, _)) = res {
            Ok((output, ()))
        } else {
            Err(ParserError::NotAComment.into())
        }
    }

    pub fn parse_type<'a>(
        &'a self,
        input: &'a str,
    ) -> IResult<&str, ASTType, ParserError> {
        let Ok((input, ident_res)) = opt::<_, _, nom::error::Error<_>, _>(|i| self.parse_ident(i))(input);
        if let Some(ident) = ident_res {
            let (input, args_res) = opt::<_, _, nom::error::Error<_>, _>(|i| delimited(
                tag::<_, _, nom::error::Error<_>>("["),
                |i| {
                    let Ok((i, head)) = self.parse_type(i);
                    let Ok((i, tail)) = many0::<_, _, nom::error::Error<_>, _>(preceded(tag(","), |i| self.parse_type(i)))(i);
                    let mut args = vec![head];
                    args.extend(tail);
                    Ok((i, args))
                },
                tag::<_, _, nom::error::Error<_>>("]"),
            ))(input);
            let args = args_res.unwrap_or_default();
            Ok((
                input,
                ASTType {
                    pos: self.pos.clone(),
                    value: None,
                    ident: Some(ident),
                    string: None,
                    colon: Vec::new(),
                    args,
                },
            ))
        } else {
            let Ok((input, value_res)) = opt::<_, _, nom::error::Error<_>, _>(|i| self.parse_int(i))(input);
            if let Some(value) = value_res {
                let Ok((input, colon_res)) = opt::<_, _, nom::error::Error<_>, _>(|i| preceded(tag::<_, _, nom::error::Error<_>>(":"), |i| self.parse_int(i)))(input);
                let mut colon = Vec::new();
                if let Some(colon_value) = colon_res {
                    colon.push(ASTType {
                        pos: self.pos.clone(),
                        value: Some(colon_value),
                        ident: None,
                        string: None,
                        colon: Vec::new(),
                        args: Vec::new(),
                    });
                }
                Ok((
                    input,
                    ASTType {
                        pos: self.pos.clone(),
                        value: Some((value, IntFmt::Dec)),
                        ident: None,
                        string: None,
                        colon,
                        args: Vec::new(),
                    },
                ))
            } else {
                let Ok((input, string)) = self.parse_string(input);
                Ok((
                    input,
                    ASTType {
                        pos: self.pos.clone(),
                        value: None,
                        ident: None,
                        string: Some((string, StrFmt::Raw)),
                        colon: Vec::new(),
                        args: Vec::new(),
                    },
                ))
            }
        }
    }
    

    // TODO: implement this
    pub fn parse_field<'a>(
        &'a self,
        input: &'a str,
    ) -> IResult<&str, ASTField, ParserError> {
        let Ok((input, _)) = multispace0::<_, nom::error::Error<_>>(input);
        let Ok((input, name)) = self.parse_ident(input);							
        let Ok((input, _)) = tag::<_, _, nom::error::Error<_>>(":")(input);								
        let Ok((input, typ)) = self.parse_type(input);						
        let Ok((input, _)) = multispace0::<_, nom::error::Error<_>>(input);
        
        let (input, attrs) = opt(delimited(
            tag::<_, _, nom::error::Error<_>>("("),
            separated_list0(multispace0, self::parse_type),
            tag::<_, _, nom::error::Error<_>>(")"),
        ))(input);
    
        let attrs = attrs.unwrap_or_default();
    
        Ok((
            input,
            ASTField {
                pos: self.pos.clone(),
                name,
                typ,
                attrs,
                new_block: false,
            },
        ))


        /* Err(ParserError::NotAField.into()) */
    }

    pub fn parse_flags<'a>(
        &'a self,
        input: &'a str,
    ) -> IResult<&str, (Option<IntFlagsNode>, Option<StrFlagsNode>), ParserError>
    {
        let res = self.parse_ident(input);
        if res.is_err() {
            return Err(ParserError::NotAFlags.into());
        }
        let (remaining, ident) = res.unwrap();
        // try to locate space | '=' | space
        let res =
            tuple::<_, _, nom::error::Error<_>, _>((space0, tag("="), space0))(
                remaining,
            );
        if res.is_err() {
            return Err(ParserError::NotAFlags.into());
        }
        let (remaining, _) = res.unwrap();
        // deal with the list
        // int flags
        let res = self.parse_int(remaining);
        if let Ok((remaining, ast_int)) = res {
            let mut values: Vec<ASTInt> = Vec::new();
            values.push(ast_int);
            let mut cursor = remaining;
            loop {
                let res = preceded(
                    many0(one_of::<_, _, nom::error::Error<_>>(" \r\t")),
                    one_of(",\n"),
                )(cursor);
                if res.is_err() {
                    return Err(ParserError::FlagsParseError.into());
                }
                let res = res.unwrap();
                cursor = res.0;
                let the_char = res.1;
                if the_char == '\n' {
                    let int_flags = IntFlagsNode {
                        pos: self.pos.clone(),
                        name: ident,
                        values: values,
                    };
                    return Ok((cursor, (Some(int_flags), None)));
                }
                (cursor, _) = many0(one_of::<_, _, nom::error::Error<_>>(
                    " \r\t",
                ))(cursor)
                .unwrap();
                let res = self.parse_int(cursor);
                if res.is_err() {
                    return Err(ParserError::FlagsParseError.into());
                }
                let res = res.unwrap();
                cursor = res.0;
                values.push(res.1);
            }
        }

        // str flags
        let res = self.parse_string(remaining);
        if res.is_err() {
            return Err(ParserError::FlagsParseError.into());
        }
        let mut values: Vec<ASTString> = Vec::new();
        let (remaining, ast_string) = res.unwrap();
        values.push(ast_string);
        let mut cursor = remaining;
        loop {
            let res = preceded(
                many0(one_of::<_, _, nom::error::Error<_>>(" \r\t")),
                one_of(",\n"),
            )(cursor);
            if res.is_err() {
                return Err(ParserError::FlagsParseError.into());
            }
            let res = res.unwrap();
            cursor = res.0;
            let the_char = res.1;
            if the_char == '\n' {
                let str_flags = StrFlagsNode {
                    pos: self.pos.clone(),
                    name: ident,
                    values: values,
                };
                return Ok((cursor, (None, Some(str_flags))));
            }
            (cursor, _) =
                many0(one_of::<_, _, nom::error::Error<_>>(" \r\t"))(cursor)
                    .unwrap();
            let res = self.parse_string(cursor);
            if res.is_err() {
                return Err(ParserError::FlagsParseError.into());
            }
            let res = res.unwrap();
            cursor = res.0;
            values.push(res.1);
        }

        // FUTURE: support identifiers after we have define and const files
    }

    // TODO: implement this
    pub fn parse_call<'a>(
        &'a self,
        input: &'a str,
    ) -> IResult<&str, CallNode, ParserError> {
        
        let Ok((input, name)) = self.parse_ident(input);
        let call_name = name.name;

        let (input, args) = delimited(
            tag::<_, _, nom::error::Error<_>>("("),
            separated_list0(
                delimited(multispace0, tag(","), multispace0),
                self::parse_field,
            ),
            tag::<_, _, nom::error::Error<_>>(")"),
        )(input);

        let (input, ret) = opt::<_, _, nom::error::Error<_>, _>(self::parse_type)(input);

    
        /* Err(ParserError::NotACall.into()) */
        Ok((
            input,
            CallNode {
                pos: self.pos.clone(),
                name,
                call_name,
                args,
                ret,
                attrs: vec![],
            },
        ))
    }

    // TODO: implement this
    pub fn parse_resource<'a>(
        &'a self,
        input: &'a str,
    ) -> IResult<&str, ResourceNode, ParserError> {

        Err(ParserError::NotAResource.into())
    }

    // TODO: implement this
    pub fn parse_typedef<'a>(
        &'a self,
        input: &'a str,
    ) -> IResult<&str, TypeDefNode, ParserError> {


        Err(ParserError::NotATypeDef.into())
    }

    // TODO: implement this
    pub fn parse_struct<'a>(
        &'a self,
        input: &'a str,
    ) -> IResult<&str, StructNode, ParserError> {


        Err(ParserError::NotAStruct.into())
    }

    // TODO: implement this
    pub fn parse_union<'a>(
        &'a self,
        input: &'a str,
    ) -> IResult<&str, StructNode, ParserError> {


        Err(ParserError::NotAUnion.into())
    }

    pub fn do_parse<'a>(&'a self, data: &'a str, nodes: &mut Vec<Box<dyn ASTNode>>) {
        let mut cursor = data;
        loop {
            if cursor.is_empty() {
                break;
            }
            // remove spaces
            (cursor, _) =
                multispace0::<_, nom::error::Error<_>>(cursor).unwrap();

            // ignore comments
            let res = self.parse_comment(cursor);
            if let Ok((remaining, _)) = res {
                cursor = remaining;
                // remove spaces
                (cursor, _) =
                    multispace0::<_, nom::error::Error<_>>(cursor).unwrap();
            }

            // probe ident
            let (remaining, name) =
                self.parse_ident(cursor).expect("wrong syntax");
            match name.name.as_str() {
                "resource" => {
                    let (remaining, resource) = self
                        .parse_resource(cursor)
                        .expect("wrong resource syntax");
                    cursor = remaining;
                    nodes.push(Box::new(resource));
                    continue;
                }
                "type" => {
                    let (remaining, typedef) = self
                        .parse_typedef(cursor)
                        .expect("wrong typedef syntax");
                    cursor = remaining;
                    nodes.push(Box::new(typedef));
                    continue;
                }
                _ => (),
            }
            // try flags, call, struct, union one by one
            // remove spaces
            let (remaining, _) =
                multispace0::<_, nom::error::Error<_>>(remaining).unwrap();
            let res = one_of::<_, _, nom::error::Error<_>>("{[(=")(remaining);
            if res.is_err() {
                panic!("wrong syntax");
            }

            let (_, delimiter) = res.unwrap();
            match delimiter {
                '[' => {
                    let (remaining, union) =
                        self.parse_union(cursor).expect("wrong union syntax");
                    cursor = remaining;
                    nodes.push(Box::new(union));
                    continue;
                }
                '{' => {
                    let (remaining, structure) =
                        self.parse_struct(cursor).expect("wrong union syntax");
                    cursor = remaining;
                    nodes.push(Box::new(structure));
                    continue;
                }
                '=' => {
                    let (remaining, flags) =
                        self.parse_flags(cursor).expect("wrong union syntax");
                    cursor = remaining;
                    match flags {
                        (Some(int_flags), None) => {
                            nodes.push(Box::new(int_flags));
                        }
                        (None, Some(str_flags)) => {
                            nodes.push(Box::new(str_flags));
                        }
                        _ => unreachable!(),
                    }
                    continue;
                }
                '(' => {
                    let (remaining, call) =
                        self.parse_call(cursor).expect("wrong call syntax");
                    cursor = remaining;
                    nodes.push(Box::new(call));
                    continue;
                }
                _ => unreachable!(),
            }
        }
    }

    pub fn parse(&self) -> Vec<Box<dyn ASTNode>> {
        let mut nodes = Vec::new();
        if self.files.is_none() {
            return nodes;
        }
        
        if let Some(files) = self.files.as_ref() {
            for f in files {
                let contents =
                    read_to_string(f).expect("should be able to read files");
                self.do_parse(&contents, &mut nodes);
            }
        }
        nodes
    }
}

#[cfg(test)]
mod tests {
    use crate::{IntFmt, Parser, ParserError, StrFmt};

    #[test]
    fn test_parse_integer() {
        let parser = Parser::new(None);

        // positive
        let res = parser
            .parse_integer("12345")
            .expect("should be good")
            .1
            .value
            .expect("shoud be good");
        assert!(res.0 == 12345 && res.1 == IntFmt::Dec);

        let res = parser
            .parse_integer("0x1af79")
            .expect("should be good")
            .1
            .value
            .expect("shoud be good");
        assert!(res.0 == 0x1af79 && res.1 == IntFmt::Hex);

        let res = parser
            .parse_integer("-13456")
            .expect("should be good")
            .1
            .value
            .expect("shoud be good");
        assert!(res.0 as i64 == -13456 && res.1 == IntFmt::Neg);

        // negative
        let res: ParserError = parser
            .parse_integer("abc")
            .expect_err("should not parse")
            .try_into()
            .expect("should be good");
        assert!(res == ParserError::NotAnInteger);

        let negative_samples = ["-abc", "-0xabc", "0xzsd", "-98az", "987az"];
        for ns in negative_samples {
            let res: ParserError = parser
                .parse_integer(ns)
                .expect_err("should not parse")
                .try_into()
                .expect("should be good");
            assert!(res == ParserError::IntegerParseError);
        }
    }

    #[test]
    fn test_parse_identifier() {
        let parser = Parser::new(None);

        // positive
        let positive_samples = [
            "resource",
            "struct",
            "union",
            "type",
            "flags",
            "t_e_s_t",
            "_t_e_s_t",
            "_t_e_s_t_90_70_10_00",
        ];
        for ps in positive_samples {
            let res = parser.parse_ident(ps).expect("should be good").1.name;
            assert!(res == ps)
        }

        // negative
        let negative_samples = ["9090", "0xasd", "-1980", "&!@$%^asasdf-=_=+"];
        for ns in negative_samples {
            let res: ParserError = parser
                .parse_ident(ns)
                .expect_err("should not parse")
                .try_into()
                .expect("should be good");
            assert!(res == ParserError::NotAnIdent);
        }
    }

    #[test]
    fn test_parse_char() {
        let parser = Parser::new(None);

        // positive
        let positive_samples = ["\'a\'", "\'z\'", "\'0\'", "\'9\'", "\'\'\'"];
        for ps in positive_samples {
            let (value, fmt) = parser
                .parse_char(ps)
                .expect("should be good")
                .1
                .value
                .expect("should be good");
            assert!(
                value == (ps.chars().nth(1).unwrap() as u64)
                    && fmt == IntFmt::Char
            );
        }

        // negative
        let negative_samples = ["\'a", "\"a\"", "123", "asdf"];
        for ns in negative_samples {
            let res: ParserError = parser
                .parse_char(ns)
                .expect_err("should not parse")
                .try_into()
                .expect("should be good");
            assert!(res == ParserError::NotAChar);
        }

        let negative_samples = ["\'\n\'", "\'\r\'"];
        for ns in negative_samples {
            let res: ParserError = parser
                .parse_char(ns)
                .expect_err("should not parse")
                .try_into()
                .expect("should be good");
            assert!(res == ParserError::CharParseError);
        }
    }

    #[test]
    fn test_parse_string() {
        let parser = Parser::new(None);

        // positive
        let positive_samples =
            ["\"asdfasdf1234_+67`\n\r\"", "`1234567890abcdef`"];
        for ps in positive_samples {
            let is_hex = ps.starts_with('`');
            let res = parser.parse_string(ps).expect("should be good").1;
            if is_hex {
                assert!(
                    Parser::decode_hex_str(&ps[1..(ps.len() - 1)])
                        .iter()
                        .zip(res.value.iter())
                        .filter(|&(a, b)| a != b)
                        .count()
                        == 0
                );
                assert!(res.fmt == StrFmt::Hex);
            } else {
                assert!(
                    ps[1..(ps.len() - 1)]
                        .as_bytes()
                        .iter()
                        .zip(res.value.iter())
                        .filter(|&(a, b)| a != b)
                        .count()
                        == 0
                );
                assert!(res.fmt == StrFmt::Raw);
            }
        }

        // negative
        let negative_samples = [
            "\"asdfasdfasdf\r\n76878_+`",
            "`uiioppjjpojpo`",
            "`123`",
            "`aba7689978",
        ];
        for ns in negative_samples {
            let res: ParserError = parser
                .parse_string(ns)
                .expect_err("should not parse")
                .try_into()
                .expect("should be good");
            assert!(res == ParserError::StringParseError);
        }

        let negative_samples =
            ["1234", "absd", "_asdf", "\'aaaa\'", "# asdfasdf\n\r"];
        for ns in negative_samples {
            let res: ParserError = parser
                .parse_string(ns)
                .expect_err("should not parse")
                .try_into()
                .expect("should be good");
            assert!(res == ParserError::NotAString);
        }
    }

    #[test]
    fn test_parse_comment() {
        let parser = Parser::new(None);

        let positive_samples = ["#this is comment`\"\'\t\r123_90\n"];
        for ps in positive_samples {
            parser.parse_comment(ps).expect("should be good");
        }

        let negative_samples =
            ["# this is a comment with no line break", "this is nothing"];
        for ns in negative_samples {
            parser.parse_comment(ns).expect_err("should not parse");
        }
    }

    #[test]
    fn test_parse_flags() {
        let parser = Parser::new(None);

        let positive_samples = [
            "test_flags=1,2,   3,       \'a\',\t\r0x12, -90   \n",
            "test_flags  \t = -910, 1234, 0xaaa, \'\"\' \n",
            "test_flags = \"hjhljk6709_+6765\", `abcdef001122`\n",
        ];
        for ps in positive_samples {
            parser.parse_flags(ps).expect("should be good").1;
        }

        let negative_samples = [
            "test_flags = 1, 2, 3, \"a\"\n",
            "test_flags = \"absdasdf\", 1, 2, 3\n",
        ];
        for ns in negative_samples {
            let res: ParserError = parser
                .parse_flags(ns)
                .expect_err("should not parse")
                .try_into()
                .expect("should be good");
            assert!(res == ParserError::FlagsParseError);
        }

        let negative_samples =
            ["test_flags := 1, 2, 3, \"a\"\n", "test_flags {}"];
        for ns in negative_samples {
            let res: ParserError = parser
                .parse_flags(ns)
                .expect_err("should not parse")
                .try_into()
                .expect("should be good");
            assert!(res == ParserError::NotAFlags);
        }
    }

    #[test]
    fn test_parse_call(){
        let parser = Parser::new(None);
        let positive_samples = [
            "test_call1(fd:fd[sock], count:int32[0:2]) fd\n",
            "test_call2(count:int32[0:100,2])"
        ];
        for ps in positive_samples {
            parser.parse_call(ps).expect("should be good").1;
        }    
    }
}
