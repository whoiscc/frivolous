use std::str::FromStr;

use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, is_not, tag},
    character::complete::{alpha1, alphanumeric0, char, multispace1, not_line_ending},
    combinator::{all_consuming, fail, opt, recognize, success, value, verify},
    error::context,
    multi::{many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Finish, IResult, Parser,
};

use crate::machine::NumericalOperator2;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Node {
    LiteralBool(bool),
    LiteralInt(i32),
    LiteralString(String),

    Id(String),

    Infix(NumericalOperator2, Box<Node>, Box<Node>),
    Call(Box<Node>, Vec<Node>),
    IfElse(Box<Node>, Vec<Node>, Vec<Node>),
    While(Box<Node>, Vec<Node>),
    Function(Vec<String>, Vec<Node>),
    Sequence(Vec<Node>),

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
        match all_consuming(delimited(token_break, sequence, token_break))(s).finish() {
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
    expr2(input)
}

fn expr0(input: &str) -> IResult<&str, Node> {
    alt((
        id.map(|id| Node::Id(id.into())),
        literal_bool,
        literal_int,
        literal_string,
        if_else,
        expr_while,
        function,
        item_set,
        value(Node::Break, tag("break")),
        value(Node::Continue, tag("continue")),
        preceded(pair(tag("return"), token_break), expr).map(|expr| Node::Return(expr.into())),
        parentheses,
        expr_sequence,
    ))(input)
}

fn parentheses(input: &str) -> IResult<&str, Node> {
    delimited(
        pair(char('('), token_break),
        expr,
        pair(token_break, char(')')),
    )(input)
}

fn expr1(input: &str) -> IResult<&str, Node> {
    alt((call, expr0))(input)
}

fn expr2(input: &str) -> IResult<&str, Node> {
    alt((infix, expr1))(input)
}

fn literal_bool(input: &str) -> IResult<&str, Node> {
    alt((
        value(Node::LiteralBool(true), tag("true")),
        value(Node::LiteralBool(false), tag("false")),
    ))(input)
}

fn literal_int(input: &str) -> IResult<&str, Node> {
    nom::character::complete::i32
        .map(Node::LiteralInt)
        .parse(input)
}

fn literal_string(input: &str) -> IResult<&str, Node> {
    let quoted = escaped_transform(
        is_not("\\\""),
        '\\',
        alt((
            // TODO
            value("\n", char('n')),
        )),
    );
    let (remaining, string) = delimited(char('"'), quoted, char('"'))(input)?;
    Ok((remaining, Node::LiteralString(string)))
}

fn id(input: &str) -> IResult<&str, &str> {
    let parser = recognize(pair(alpha1, alphanumeric0));
    verify(parser, |id| {
        ![
            "function", "begin", "end", "if", "then", "else", "while", "let", "set", "true",
            "false", "break", "continue", "return",
        ]
        .contains(id)
    })(input)
}

fn item_let(input: &str) -> IResult<&str, Node> {
    let (input, id) = preceded(pair(tag("let"), token_break), id)(input)?;
    let (remaining, expr) = opt(preceded(token_break, expr))(input)?;
    Ok((remaining, Node::Let(id.into(), expr.map(Into::into))))
}

fn item_set(input: &str) -> IResult<&str, Node> {
    let (remaining, (id, expr)) = preceded(
        pair(tag("set"), token_break),
        separated_pair(id, token_break, expr),
    )(input)?;
    Ok((remaining, Node::Set(id.into(), expr.into())))
}

fn infix(input: &str) -> IResult<&str, Node> {
    use NumericalOperator2::*;
    // TODO precedence
    let op = alt((
        value(Equal, tag("==")),
        value(NotEqual, tag("!=")),
        value(LessEqual, tag("<=")),
        value(GreaterEqual, tag(">=")),
        value(Add, char('+')),
        value(Sub, char('-')),
        value(Less, char('<')),
        value(Greater, char('>')),
        context("unrecognized infix operator", fail),
    ));
    let (remaining, (one_expr, _, op, _, another_expr)) =
        tuple((expr1, token_break, op, token_break, expr2))(input)?;
    Ok((
        remaining,
        Node::Infix(op, one_expr.into(), another_expr.into()),
    ))
}

fn call(input: &str) -> IResult<&str, Node> {
    let arguments = delimited(
        pair(char('('), token_break),
        separated_list0(pair(char(','), token_break), expr),
        tuple((opt(pair(token_break, char(','))), token_break, char(')'))),
    );
    let (remaining, (callee, arguments)) = separated_pair(expr0, token_break, arguments)(input)?;
    Ok((remaining, Node::Call(callee.into(), arguments)))
}

fn if_else(input: &str) -> IResult<&str, Node> {
    let test = preceded(pair(tag("if"), token_break), expr);
    let positive_tag = || pair(tag("then"), token_break);
    let positive = preceded(positive_tag(), sequence);
    let negative_tag = || pair(tag("else"), token_break);
    let negative = opt(preceded(negative_tag(), sequence)).map(Option::unwrap_or_default);
    let positive_expr = preceded(positive_tag(), expr).map(|expr| vec![expr]);
    let negative_expr = preceded(negative_tag(), expr).map(|expr| vec![expr]);
    let (remaining, (test, (positive, negative))) = separated_pair(
        test,
        token_break,
        alt((
            terminated(pair(positive, negative), pair(token_break, tag("end"))),
            pair(positive_expr, negative_expr),
        )),
    )(input)?;
    Ok((remaining, Node::IfElse(test.into(), positive, negative)))
}

fn expr_while(input: &str) -> IResult<&str, Node> {
    let test = preceded(pair(tag("while"), token_break), expr);
    let (remaining, (test, sequence)) = separated_pair(
        test,
        tuple((token_break, tag("while"), token_break)),
        alt((
            terminated(sequence, pair(token_break, tag("end"))),
            expr.map(|expr| vec![expr]),
        )),
    )(input)?;
    Ok((remaining, Node::While(test.into(), sequence)))
}

fn function(input: &str) -> IResult<&str, Node> {
    let parameters = preceded(
        pair(tag("function"), token_break),
        delimited(
            char('('),
            separated_list0(pair(char(','), token_break), id.map(Into::into)),
            tuple((opt(pair(token_break, char(','))), token_break, char(')'))),
        ),
    );
    let (remaining, (parameters, sequence)) = separated_pair(
        parameters,
        token_break,
        alt((
            terminated(sequence, pair(token_break, tag("end"))),
            expr.map(|expr| vec![expr]),
        )),
    )(input)?;
    Ok((remaining, Node::Function(parameters, sequence)))
}

fn sequence(input: &str) -> IResult<&str, Vec<Node>> {
    separated_list0(token_break, alt((item_let, expr)))(input)
}

fn expr_sequence(input: &str) -> IResult<&str, Node> {
    delimited(
        pair(tag("begin"), token_break),
        sequence,
        pair(token_break, tag("end")),
    )
    .map(Node::Sequence)
    .parse(input)
}

fn comment(input: &str) -> IResult<&str, &str> {
    recognize(pair(char(';'), not_line_ending))(input)
}

fn token_break(input: &str) -> IResult<&str, &str> {
    alt((recognize(many1(alt((comment, multispace1)))), success("")))(input)
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
        let expected = || {
            ok_eoi(Node::Function(
                Default::default(),
                vec![Node::LiteralInt(42)],
            ))
        };
        assert_eq!(expr("function () 42"), expected());
        assert_eq!(all_consuming(expr)("function () 42 end"), expected());
        assert_eq!(
            expr("function () begin 42 end"),
            ok_eoi(Node::Function(
                Default::default(),
                vec![Node::Sequence(vec![Node::LiteralInt(42)])]
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
