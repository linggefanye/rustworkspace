use nom::{
    bytes::complete::{tag, take_while1},
    character::complete::{char, multispace0, digit1, newline},
    combinator::{map, opt, recognize},
    multi::{separated_list0,  many0},
    sequence::{delimited, separated_pair, terminated, tuple, preceded},
    branch::alt,
    IResult,
};

#[derive(Debug)]
struct TypeAlias {
    alias: String,
    underlying_type: BaseType,
    type_options: Option<Vec<String>>,
}

#[derive(Debug)]
enum BaseType {
    Int8,
    Int16,
    Int32,
    Int64,
    IntPtr,
    Custom(String),
}

fn parse_identifier(input: &str) -> IResult<&str, String> {
    map(
        take_while1(|c: char| c.is_alphanumeric() || c == '_'),
        String::from,
    )(input)
}

fn parse_base_type(input: &str) -> IResult<&str, (BaseType, Option<Vec<String>>)> {
    let (input, base_type) = parse_identifier(input)?;
    let (input, type_options) = opt(parse_type_options)(input)?;

    let base_type = match base_type.as_str() {
        "int8" => BaseType::Int8,
        "int16" => BaseType::Int16,
        "int32" => BaseType::Int32,
        "int64" => BaseType::Int64,
        "intptr" => BaseType::IntPtr,
        _ => BaseType::Custom(base_type),
    };

    Ok((input, (base_type, type_options)))
}

fn parse_type_options(input: &str) -> IResult<&str, Vec<String>> {
    let (input, options) = delimited(
        tag("["),
        separated_list0(
            tag(","),
            alt((
                parse_nested_option,
                map(
                    take_while1(|c: char| {
                        c.is_alphanumeric() || c == '_' || c == ':' || c == '-' || c == '.'
                    }),
                    |s: &str| s.trim().to_string(),
                ),
            )),
        ),
        tag("]"),
    )(input)?;

    Ok((input, options))
}

fn parse_nested_option(input: &str) -> IResult<&str, String> {
    let mut nesting_level = 0;
    let mut output = String::new();

    let mut chars = input.char_indices().peekable();

    while let Some((idx, c)) = chars.next() {
        match c {
            '[' => nesting_level += 1,
            ']' => {
                if nesting_level == 0 {
                    return Ok((&input[idx..], output));
                }
                nesting_level -= 1;
            }
            ',' => {
                if nesting_level == 0 {
                    return Ok((&input[idx..], output));
                }
            }
            _ => {}
        }
        output.push(c);
    }

    Err(nom::Err::Error(nom::error::make_error(input, nom::error::ErrorKind::Eof)))
}

fn parse_type_alias(input: &str) -> IResult<&str, TypeAlias> {
    let (input, _) = tag("type")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, alias) = parse_identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, (underlying_type, type_options)) = parse_base_type(input)?;
    let (input, _) = multispace0(input)?;
    Ok((
        input,
        TypeAlias {
            alias,
            underlying_type,
            type_options,
        },
    ))
}

fn parse_type_aliases(input: &str) -> IResult<&str, Vec<TypeAlias>> {
    let (input, type_aliases) = many0(
        terminated(
            parse_type_alias,
            opt(delimited(multispace0, newline, multispace0)),
        ),
    )(input)?;
    Ok((input, type_aliases))
}

fn main() {
    let type_aliases_input = "type signalno int32[0:65]
    type net_port proc[20000, 4, int16be]";
    let (_, type_aliases) = parse_type_aliases(type_aliases_input).expect("Failed to parse the type aliases");
    println!("Type aliases: {:#?}", type_aliases);
}
