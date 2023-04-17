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
struct UnionField {
    name: String,
    field_type: BaseType,
    type_options: Option<Vec<String>>,
}

#[derive(Debug)]
struct UnionDef {
    name: String,
    fields: Vec<UnionField>,
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

fn parse_union_field(input: &str) -> IResult<&str, UnionField> {
    let (input, _) = multispace0(input)?;
    let (input, name) = parse_identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, (field_type, type_options)) = parse_base_type(input)?;
    let (input, _) = newline(input)?;

    Ok((
        input,
        UnionField {
            name,
            field_type,
            type_options,
        },
    ))
}

fn parse_union(input: &str) -> IResult<&str, UnionDef> {
    let (input, _) = tag("union")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, name) = parse_identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("[")(input)?;
    let (input, _) = newline(input)?;
    let (input, fields) = many0(parse_union_field)(input)?;
    let (input, _) = tag("]")(input)?;

    Ok((
        input,
        UnionDef {
            name,
            fields,
        },
    ))
}

fn main() {
    let union_input = "union example [
	value1 int32[1:100]
	value2 int64
	value3 intptr
]";
    let (_, parsed_union) = parse_union(union_input).expect("Failed to parse the union");
    println!("Parsed union: {:#?}", parsed_union);
}

