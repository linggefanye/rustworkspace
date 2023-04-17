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
struct Syscall {
    name: String,
    result_type: String,
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

#[derive(Debug)]
struct Resource {
    name: String,
    base_type: BaseType,
    consts: Vec<u64>,
    type_options: Option<Vec<String>>,
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


fn parse_u64(input: &str) -> IResult<&str, u64> {
    let (input, _) = multispace0(input)?;
    let (input, hex_prefix) = opt(alt((tag("0x"), tag("0X"))))(input)?;
    let (input, num_str) = if hex_prefix.is_some() {
        nom::character::complete::hex_digit1(input)?
    } else {
        digit1(input)?
    };
    let (input, _) = multispace0(input)?;

    let num = if hex_prefix.is_some() {
        u64::from_str_radix(num_str, 16).unwrap()
    } else {
        num_str.parse::<u64>().unwrap()
    };

    Ok((input, num))
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

fn parse_resource(input: &str) -> IResult<&str, Resource> {
    let (input, _) = tag("resource")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, name) = parse_identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("[")(input)?;
    let (input, (base_type, type_options)) = parse_base_type(input)?;
    let (input, _) = tag("]")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, consts) = opt(preceded(
        tag(":"),
        separated_list0(
            delimited(multispace0, tag(","), multispace0),
            parse_u64,
        ),
    ))(input)?;


    Ok((
        input,
        Resource {
            name,
            base_type,
            consts: consts.unwrap_or_else(|| vec![]),
            type_options,
        },
    ))
}

fn parse_resources(input: &str) -> IResult<&str, Vec<Resource>> {
    let (input, resources) = many0(
        terminated(
            parse_resource,
            opt(delimited(multispace0, newline, multispace0)),
        ),
    )(input)?;
    Ok((input, resources))
}

fn main() {
    let resources_input = "resource fd[int32]: 0xffffffffffffffff, 1000000\nresource sock[fd]\nresource sock_unix[sock]";
    let (_, resources) = parse_resources(resources_input).expect("Failed to parse the resources");
    println!("Resources: {:#?}", resources);
}
