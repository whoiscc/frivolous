use std::str::FromStr;

use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, is_not, tag},
    character::complete::{alpha1, alphanumeric0, multispace0},
    combinator::{all_consuming, cond, opt, recognize, value, verify},
    multi::separated_list0,
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    Finish, IResult, Parser,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Node {
    LiteralBool(bool),
    LiteralInt(i32),
    LiteralString(String),

    Id(String),

    Operator2(String, Box<Node>, Box<Node>),
    Call(Box<Node>, Vec<Node>),
    IfElse(Box<Node>, Box<Node>, Option<Box<Node>>),
    While(Box<Node>, Box<Node>),
    Sequence(Vec<Node>),
    Function(Vec<String>, Box<Node>),

    Let(String, Option<Box<Node>>),
    Set(String, Box<Node>),

    Break,
    Continue,
    Return(Box<Node>),
}

#[derive(Debug)]
pub struct Source(Vec<Node>);

impl FromStr for Source {
    type Err = nom::error::Error<String>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match all_consuming(delimited(multispace0, sequence_inner, multispace0))(s).finish() {
            Ok((_, items)) => Ok(Self(items)),
            Err(err) => Err(nom::error::Error {
                input: err.input.into(),
                code: err.code,
            }),
        }
    }
}

impl Source {
    pub fn new(s: &str) -> anyhow::Result<Self> {
        Ok(s.parse()?)
    }
}

fn expr(input: &str) -> IResult<&str, Node> {
    expr1(input)
}

fn expr0(input: &str) -> IResult<&str, Node> {
    alt((
        literal_bool,
        literal_int,
        literal_string,
        if_else,
        expr_while,
        function,
        id.map(|id| Node::Id(id.into())),
        sequence::<false>,
        // TODO parentheses
    ))(input)
}

fn expr1(input: &str) -> IResult<&str, Node> {
    alt((call, operator2, expr0))(input)
}

fn expr2(input: &str) -> IResult<&str, Node> {
    alt((sequence::<true>, expr1))(input)
}

fn literal_bool(input: &str) -> IResult<&str, Node> {
    alt((
        value(Node::LiteralBool(true), tag("true")),
        value(Node::LiteralBool(false), tag("false")),
    ))(input)
}

fn literal_int(input: &str) -> IResult<&str, Node> {
    let (remaining, int) = nom::character::complete::i32(input)?;
    Ok((remaining, Node::LiteralInt(int)))
}

fn literal_string(input: &str) -> IResult<&str, Node> {
    let quoted = escaped_transform(
        is_not("\\\""),
        '\\',
        alt((
            // TODO
            value("\n", tag("n")),
        )),
    );
    let (remaining, string) = delimited(tag("\""), quoted, tag("\""))(input)?;
    Ok((remaining, Node::LiteralString(string)))
}

fn id(input: &str) -> IResult<&str, &str> {
    let parser = recognize(pair(alpha1, alphanumeric0));
    verify(parser, |id| {
        ![
            "function", "begin", "end", "if", "else", "while", "let", "set",
        ]
        .contains(id)
    })(input)
}

fn item_let(input: &str) -> IResult<&str, Node> {
    let (input, id) = preceded(pair(tag("let"), multispace0), id)(input)?;
    let (remaining, expr) =
        opt(preceded(tuple((multispace0, tag("="), multispace0)), expr))(input)?;
    Ok((remaining, Node::Let(id.into(), expr.map(Into::into))))
}

fn item_set(input: &str) -> IResult<&str, Node> {
    let parser = separated_pair(id, tuple((multispace0, tag("="), multispace0)), expr);
    let (remaining, (id, expr)) = preceded(pair(tag("set"), multispace0), parser)(input)?;
    Ok((remaining, Node::Set(id.into(), expr.into())))
}

fn operator2(input: &str) -> IResult<&str, Node> {
    nom::combinator::fail(input) // TODO
}

fn call(input: &str) -> IResult<&str, Node> {
    let arguments = delimited(
        pair(tag("("), multispace0),
        separated_list0(pair(tag(","), multispace0), expr),
        tuple((opt(pair(multispace0, tag(","))), multispace0, tag(")"))),
    );
    let (remaining, (callee, arguments)) = separated_pair(expr0, multispace0, arguments)(input)?;
    Ok((remaining, Node::Call(callee.into(), arguments)))
}

fn sequence<const OMIT_BEGIN: bool>(input: &str) -> IResult<&str, Node> {
    let (remaining, items) = delimited(
        tuple((cond(!OMIT_BEGIN, tag("begin")), multispace0)),
        sequence_inner,
        pair(multispace0, tag("end")),
    )(input)?;
    Ok((remaining, Node::Sequence(items)))
}

fn if_else(input: &str) -> IResult<&str, Node> {
    // thinking of denying sequence expression in test expression all together
    let test = preceded(pair(tag("if"), multispace0), expr);
    let negative = preceded(pair(tag("else"), multispace0), expr2);
    let (remaining, (test, (positive, negative))) = separated_pair(
        test,
        multispace0,
        separated_pair(expr2, multispace0, opt(negative)),
    )(input)?;
    Ok((
        remaining,
        Node::IfElse(test.into(), positive.into(), negative.map(Into::into)),
    ))
}

fn expr_while(input: &str) -> IResult<&str, Node> {
    // omit begin of test expression for do-while style looping
    // while
    //   something...
    //   something more...
    //   loop condition?
    // end
    //   continue
    let test = preceded(pair(tag("while"), multispace0), expr2);
    let (remaining, (test, expr)) = separated_pair(test, multispace0, expr2)(input)?;
    Ok((remaining, Node::While(test.into(), expr.into())))
}

fn function(input: &str) -> IResult<&str, Node> {
    let parameters = preceded(
        pair(tag("function"), multispace0),
        delimited(
            tag("("),
            separated_list0(pair(tag(","), multispace0), id),
            tuple((opt(pair(multispace0, tag(","))), multispace0, tag(")"))),
        ),
    );
    let (remaining, (parameters, expr)) = separated_pair(parameters, multispace0, expr2)(input)?;
    Ok((
        remaining,
        Node::Function(
            parameters.into_iter().map(Into::into).collect(),
            expr.into(),
        ),
    ))
}

fn sequence_inner(input: &str) -> IResult<&str, Vec<Node>> {
    separated_list0(multispace0, alt((item_let, item_set, expr)))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ok_eoi(node: Node) -> IResult<&'static str, Node> {
        Ok(("", node))
    }

    #[test]
    fn literal_int() {
        assert_eq!(expr("42"), ok_eoi(Node::LiteralInt(42)))
    }

    #[test]
    fn literal_bool() {
        assert_eq!(expr("true"), ok_eoi(Node::LiteralBool(true)));
        assert_eq!(expr("false"), ok_eoi(Node::LiteralBool(false)))
    }

    #[test]
    fn literal_string() {
        // assert_eq!(expr("\"\""), ok_eoi(Node::LiteralString("".into())));
        assert_eq!(
            expr(r#""Hello, world!""#),
            ok_eoi(Node::LiteralString("Hello, world!".into()))
        );
        assert_eq!(expr("\"\\n\""), ok_eoi(Node::LiteralString("\n".into())))
    }

    #[test]
    fn function() {
        assert_eq!(
            expr("function () 42"),
            ok_eoi(Node::Function(
                Default::default(),
                Box::new(Node::LiteralInt(42))
            ))
        );
        assert_eq!(
            expr("function () 42 end"),
            ok_eoi(Node::Function(
                Default::default(),
                Box::new(Node::Sequence(vec![Node::LiteralInt(42)]))
            ))
        );
        assert_eq!(
            expr("function () begin 42 end"),
            ok_eoi(Node::Function(
                Default::default(),
                Box::new(Node::Sequence(vec![Node::LiteralInt(42)]))
            ))
        )
    }

    #[test]
    fn call() {
        assert_eq!(
            expr("fib(10)"),
            ok_eoi(Node::Call(
                Box::new(Node::Id("fib".into())),
                vec![Node::LiteralInt(10)]
            ))
        );
        assert_eq!(
            expr("max(10, 42)"),
            ok_eoi(Node::Call(
                Box::new(Node::Id("max".into())),
                vec![Node::LiteralInt(10), Node::LiteralInt(42)]
            ))
        );
        assert_eq!(
            expr("max(10, 42,)"),
            ok_eoi(Node::Call(
                Box::new(Node::Id("max".into())),
                vec![Node::LiteralInt(10), Node::LiteralInt(42)]
            ))
        )
    }
}
