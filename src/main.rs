use nom::{
    branch::alt,
    multi::separated_list0,
    bytes::complete::{tag, take_while1},
    character::complete::{alphanumeric1, multispace0, multispace1},
    combinator::{map, opt},
    sequence::{delimited, separated_pair, terminated, tuple, preceded},
    IResult,
};

#[derive(Debug)]
struct FunctionCall {
    name: String,
    args: Vec<Argument>,
}

#[derive(Debug)]
pub struct Argument {
    name: String,
    typename: String,
    type_options: Option<Vec<String>>,
    attributes: Vec<String>,
}

#[derive(Debug, PartialEq)]
struct Template {
    name: String,
    fields: Vec<String>,
}

#[derive(Debug, PartialEq)]
struct Attribute {
    name: String,
    value: String,
}

#[derive(Debug, PartialEq)]
struct Constraint {
    name: String,
    range: (String, String),
}

#[derive(Debug)]
struct Resource {
    name: String,
    base_type: String,
}

fn parse_identifier(input: &str) -> IResult<&str, String> {						//提取一个字符串，只包含字符数字和下划线
    map(
        take_while1(|c: char| c.is_alphanumeric() || c == '_'),	
        String::from,
    )(input)
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


fn argument(input: &str) -> IResult<&str, Argument> {					
    let (input, name) = parse_identifier(input)?;							//提取参数名
    let (input, _) = tag(":")(input)?;								//提取":"
    let (input, typename) = parse_identifier(input)?;						//提取参数类型
    let (input, type_options) = opt(parse_type_options)(input)?;					//提取类型选项
    let (input, _) = multispace0(input)?;

    let (input, attributes) = opt(preceded(tag("@"), parse_attributes))(input)?;			//提取可能存在的属性

    Ok((
        input,
        Argument {
            name,
            typename,
            type_options: Some(type_options.unwrap_or_else(|| vec![])),
            attributes: attributes.unwrap_or_else(|| vec![]),
        },
    ))
}

fn parse_attributes(input: &str) -> IResult<&str, Vec<String>> {
    separated_list0(tag(","), parse_identifier)(input)
}

fn fn_function_call(input: &str) -> IResult<&str, FunctionCall> {
    let (input, name) = parse_identifier(input)?;							//解析函数名
    let (input, _) = multispace0(input)?;								//除去空格
    let (input, _) = tag("(")(input)?;								//读取到左括号
    let (input, args) = separated_list0(terminated(tag(","), multispace0), argument)(input)?;	//以","为标识符分割函数参数
    let (input, _) = tag(")")(input)?;								//读取到右括号
    Ok((input, FunctionCall { name, args }))
}

fn main() {
    let input = "syz_test_write(fd:fd[sock], buf:array[array[int8]], count:int32[0:2])";
    let (_, parsed_function_call) = fn_function_call(input).expect("Failed to parse the function call");
    println!("{:#?}", parsed_function_call);
}