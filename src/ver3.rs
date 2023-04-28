use nom::{
    branch::alt,
    multi::{separated_list0, fold_many0},
    bytes::complete::{tag, take_while1, take_until},
    character::complete::{alphanumeric1, multispace0, multispace1, char},
    combinator::{map, opt},
    sequence::{delimited, separated_pair, terminated, tuple, preceded},
    error::context,
    IResult,
};

#[derive(Debug, Clone)]
struct Syscall {
    name: String,
    args: Vec<Field>,
    pub ret: Option<BaseType>,
    /* attributes: Vec<String>, */
}

#[derive(Debug, Clone)]
pub struct Field {
    name: String,
    typename: BaseType,
    type_options: Option<Vec<String>>,
}

#[derive(Debug)]
struct Resource {
    name: String,
    base_type: BaseType,
    consts: Vec<u64>,
    type_options: Option<Vec<String>>,
}


#[derive(Debug, Clone)]
enum BaseType {
    Int8,
    Int16,
    Int32,
    Int64,
    IntPtr,
    /* ResourceType(Resource), */   
    Custom(String),
}
/* #[derive(Debug, PartialEq)]
struct Template {
    name: String,
    fields: Vec<String>,
} */

/* #[derive(Debug, PartialEq)]
struct Attribute {
    name: String,
    value: String,
} */

/* #[derive(Debug, PartialEq)]
struct Constraint {
    name: String,
    range: (String, String),
} */

/* #[derive(Debug)]
struct Resource {
    name: String,
    base_type: String,
} */

fn parse_identifier(input: &str) -> IResult<&str, String> {						//提取一个字符串，只包含字符数字和下划线
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


fn parse_field(input: &str) -> IResult<&str, Field> {					
    let (input, name) = parse_identifier(input)?;							//提取参数名
    let (input, _) = tag(":")(input)?;								//提取":"
    let (input, (typename, type_options)) = parse_base_type(input)?;						//提取参数类型
				                                                                        //提取类型选项
    let (input, _) = multispace0(input)?;


    Ok((
        input,
        Field {
            name,
            typename,
            type_options: Some(type_options.unwrap_or_else(|| vec![])),
        },
    ))
}

fn parse_attributes(input: &str) -> IResult<&str, Vec<String>> {
    separated_list0(tag(","), parse_identifier)(input)
}

fn parse_syscall(input: &str) -> IResult<&str, Syscall> {
    let (input, name) = parse_identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("(")(input)?;

    let mut nesting_level_square = 0;
    let mut nesting_level_round = 0;
    let mut arg_start = 0;
    let mut args = Vec::new();
    for (idx, c) in input.char_indices() {
        match c {
            '[' => nesting_level_square += 1,
            ']' => nesting_level_square -= 1,
            '(' => nesting_level_round += 1,
            ')' => {
                if nesting_level_round == 0 {
                    let arg_input = &input[arg_start..idx];
                    if !arg_input.trim().is_empty() {
                        let (_, arg) = parse_field(arg_input.trim())?;
                        args.push(arg);
                    }
                    break;
                }
                nesting_level_round -= 1;
            }
            ',' => {
                if nesting_level_square == 0 && nesting_level_round == 0 {
                    let arg_input = &input[arg_start..idx];
                    let (_, arg) = parse_field(arg_input.trim())?;
                    args.push(arg);
                    arg_start = idx + 1;
                }
            }
            _ => {}
        }
    }

    let (_, remaining_input) = tag(")")(input.trim_start_matches(|c| c != ')'))?;
    let (remaining_input, _) = multispace0(input)?;
    let (remaining_input, (ret, _)) = parse_base_type(remaining_input)?;

    Ok((remaining_input, Syscall { name, args , ret: Some(ret)}))
}

fn main() {
    let input = "syz_test_write(fd:fd[sock], buf:ptr[in, array[int8]], count:int32[0:2]) fd";
    let (_, parsed_function_call) = parse_syscall(input).expect("Failed to parse the function call");
    println!("{:#?}", parsed_function_call);
}