use num_traits::Pow;
use std::collections::HashMap;
use std::fmt::Formatter;

#[derive(Debug)]
pub enum Error {
    ParseError(String),
    UnexpectedType(String),
}

impl Error {
    fn default_parse_error() -> Self {
        Error::ParseError("Unspecified parse error".to_string())
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

impl Operator {
    pub fn default_map() -> HashMap<String, Operator> {
        let mut map = HashMap::new();
        map.insert("+".to_string(), Operator::Add);
        map.insert("-".to_string(), Operator::Sub);
        map.insert("*".to_string(), Operator::Mul);
        map.insert("/".to_string(), Operator::Div);
        map.insert("^".to_string(), Operator::Pow);

        map
    }

    pub fn apply(&self, t1: Term, t2: Term) -> Result<Term, Error> {
        if !(t1.is_literal() && t2.is_literal()) {
            return Ok(Term::BinaryOperator(*self, Box::from(t1), Box::from(t2)));
        }

        if let Operator::Pow = self {
            if t1.is_int() && t2.is_int() {
                let t2_int = t2.extract_int()?;
                if t2_int >= 0 && t2_int <= u32::MAX as i64 {
                    return Ok(Term::LiteralInt(t1.extract_int()?.pow(t2_int as u32)));
                }
            }
            return Ok(Term::LiteralFloat(t1.extract_f64()?.pow(t2.extract_f64()?)));
        }

        Ok(if t1.is_float() || t2.is_float() {
            Term::LiteralFloat(self.apply_literals(
                t1.extract_f64().expect("Unexpected non-float value"),
                t2.extract_f64().expect("Unexpected non-float value"),
            )?)
        } else {
            Term::LiteralInt(self.apply_literals(
                t1.extract_int().expect("Unexpected non-int value"),
                t2.extract_int().expect("Unexpected non-int value"),
            )?)
        })
    }

    pub fn apply_literals<T>(&self, t1: T, t2: T) -> Result<T, Error>
    where
        T: std::ops::Add<T, Output = T>
            + std::ops::Sub<T, Output = T>
            + std::ops::Mul<T, Output = T>
            + std::ops::Div<T, Output = T>,
    {
        Ok(match self {
            Operator::Add => t1 + t2,
            Operator::Sub => t1 - t2,
            Operator::Mul => t1 * t2,
            Operator::Div => t1 / t2,
            Operator::Pow => {
                return Err(Error::UnexpectedType(
                    "Operator pow should have been caught earlier".to_string(),
                ))
            }
        })
    }
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Add => "+",
                Self::Sub => "-",
                Self::Mul => "*",
                Self::Div => "/",
                Self::Pow => "^",
            }
        )
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Term {
    BinaryOperator(Operator, Box<Term>, Box<Term>),
    Variable(String),
    LiteralInt(i64),
    LiteralFloat(f64),
}

impl Term {
    pub fn from_string(str: &str) -> Result<Term, Error> {
        Self::from_string_custom_operators(
            str,
            &Operator::default_map(),
            &("(".to_string(), ")".to_string()),
        )
    }

    pub fn from_string_custom_operators(
        str: &str,
        operators: &HashMap<String, Operator>,
        brackets: &(String, String),
    ) -> Result<Term, Error> {
        let elements = extract_elements(str, operators, brackets);
        let yt = shunting_yard(elements);
        infix_to_term(yt)
    }

    pub fn unify_literals(t1: Term, t2: Term) -> (Term, Term) {
        match t1 {
            Term::LiteralInt(i) => match t2 {
                Term::LiteralInt(_) => (t1, t2),
                Term::LiteralFloat(_) => (Term::LiteralFloat(i as f64), t2),
                _ => panic!(),
            },
            Term::LiteralFloat(_) => match t2 {
                Term::LiteralInt(i) => (t1, Term::LiteralFloat(i as f64)),
                Term::LiteralFloat(_) => (t1, t2),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Self::LiteralInt(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::LiteralFloat(_))
    }

    pub fn extract_int(&self) -> Result<i64, Error> {
        match self {
            Self::LiteralInt(i) => Ok(*i),
            _ => Err(Error::UnexpectedType(format!(
                "expected int but got {:?}",
                self
            ))),
        }
    }

    pub fn extract_f64(&self) -> Result<f64, Error> {
        match self {
            Self::LiteralInt(i) => Ok(*i as f64),
            Self::LiteralFloat(f) => Ok(*f),
            _ => Err(Error::UnexpectedType(format!(
                "expected float or int but got {:?}",
                self
            ))),
        }
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::BinaryOperator(o, t1, t2) => {
                write!(f, "{}({},{})", o, t1, t2)
            }
            Term::Variable(s) => write!(f, "{}", s),
            Term::LiteralInt(i) => write!(f, "{}", i),
            Term::LiteralFloat(v) => write!(f, "{}", v),
        }
    }
}

impl Term {
    pub fn apply(&self, values: &HashMap<String, Term>) -> Result<Term, Error> {
        match self {
            Self::BinaryOperator(op, t1, t2) => op.apply(t1.apply(values)?, t2.apply(values)?),
            Self::LiteralFloat(f) => Ok(Self::LiteralFloat(*f)),
            Self::LiteralInt(i) => Ok(Self::LiteralInt(*i)),
            Self::Variable(v) => {
                if let Some(t) = values.get(v) {
                    Ok(t.clone())
                } else {
                    Ok(self.clone())
                }
            }
        }
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Self::LiteralFloat(_) | Self::LiteralInt(_))
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq)]
enum YardTerm {
    Term(Term),
    Operator(Operator),
    BracketStart,
    BracketEnd,
}

impl std::fmt::Display for YardTerm {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Term(t) => write!(f, "{}", t),
            Self::Operator(o) => write!(f, "{}", o),
            Self::BracketStart => write!(f, "("),
            Self::BracketEnd => write!(f, ")"),
        }
    }
}

impl YardTerm {
    #[allow(dead_code)]
    pub fn extract_term(&self) -> Option<Term> {
        if let YardTerm::Term(t) = self {
            Some(t.clone())
        } else {
            None
        }
    }
}

#[allow(dead_code)]
fn print_yard_terms(terms: &[YardTerm]) {
    let result = terms
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");
    println!("{}", result);
}

fn infix_to_term(mut terms: Vec<YardTerm>) -> Result<Term, Error> {
    if terms.len() == 1 {
        return if let YardTerm::Term(t) = terms.pop().unwrap() {
            Ok(t)
        } else {
            Err(Error::default_parse_error())
        };
    }
    let mut stack = vec![];
    let mut second = None;
    for (i, t) in terms.iter().enumerate() {
        match t {
            YardTerm::Operator(o) => {
                if let Some(first) = stack.pop() {
                    let new_term = Term::BinaryOperator(
                        *o,
                        Box::from(first),
                        Box::from(second.take().unwrap()),
                    );
                    if let Some(YardTerm::Operator(_)) = terms.get(i + 1) {
                        second = Some(new_term);
                    } else {
                        stack.push(new_term);
                    }
                }
            }
            YardTerm::Term(t) => {
                if stack.is_empty() {
                    stack.push(t.clone());
                } else {
                    if let Some(previous) = second.take() {
                        stack.push(previous);
                    }
                    second = Some(t.clone());
                }
            }
            _ => {
                todo!();
            }
        }
    }
    stack.pop().ok_or(Error::default_parse_error())
}

fn shunting_yard(terms: Vec<YardTerm>) -> Vec<YardTerm> {
    let mut output_queue = vec![];
    let mut operator_stack = vec![];
    for t in terms {
        match t {
            YardTerm::Term(_) => {
                output_queue.push(t);
            }
            YardTerm::Operator(op) => {
                pop_stack(&mut operator_stack, &mut output_queue, Some(op));
                operator_stack.push(t);
            }
            YardTerm::BracketStart => {
                operator_stack.push(t);
            }
            YardTerm::BracketEnd => {
                pop_stack(&mut operator_stack, &mut output_queue, None);
            }
        }
    }
    pop_stack(&mut operator_stack, &mut output_queue, None);
    output_queue
}

fn pop_stack(stack: &mut Vec<YardTerm>, output: &mut Vec<YardTerm>, new_op: Option<Operator>) {
    while let Some(YardTerm::Operator(op)) = stack.last() {
        if let Some(nop) = new_op {
            if *op < nop {
                break;
            }
        }
        output.push(stack.pop().unwrap());
    }
    if let Some(YardTerm::BracketStart) = stack.last() {
        if new_op.is_none() {
            stack.pop();
        }
    }
}

fn extract_elements(
    str: &str,
    operators: &HashMap<String, Operator>,
    brackets: &(String, String),
) -> Vec<YardTerm> {
    let operators_split = operators.keys().map(|k| k.as_str()).collect::<Vec<_>>();
    let mut operators_combined = operators_split.clone();
    operators_combined.push(brackets.0.as_str());
    operators_combined.push(brackets.1.as_str());
    let pat = operators_combined
        .iter()
        .map(|o| regex::escape(o))
        .collect::<Vec<_>>()
        .join("|");
    let regex = regex::Regex::new(&pat).unwrap(); // TODO: Unwrap
    let with_whitespace =
        regex.replace_all(str, |caps: &regex::Captures| format!(" {} ", &caps[0]));
    let split_by_whitespace = with_whitespace.split_whitespace();
    let mut result = vec![];
    for s in split_by_whitespace {
        if operators_split.contains(&s) {
            if let Some(o) = operators.get(s) {
                result.push(YardTerm::Operator(*o));
            }
        } else if s == brackets.0.as_str() {
            result.push(YardTerm::BracketStart);
        } else if s == brackets.1.as_str() {
            result.push(YardTerm::BracketEnd);
        } else {
            result.push(YardTerm::Term(parse_literal(s)))
        }
    }
    result
}

fn parse_literal(str: &str) -> Term {
    if let Ok(i) = str.parse::<i64>() {
        return Term::LiteralInt(i);
    }
    if let Ok(f) = str.parse::<f64>() {
        return Term::LiteralFloat(f);
    }
    Term::Variable(str.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_elements() {
        let term = "a + 5.0 * (c -1)";
        let r = extract_elements(
            term,
            &Operator::default_map(),
            &("(".to_string(), ")".to_string()),
        );
        let expected = vec![
            YardTerm::Term(Term::Variable("a".to_string())),
            YardTerm::Operator(Operator::Add),
            YardTerm::Term(Term::LiteralFloat(5.0)),
            YardTerm::Operator(Operator::Mul),
            YardTerm::BracketStart,
            YardTerm::Term(Term::Variable("c".to_string())),
            YardTerm::Operator(Operator::Sub),
            YardTerm::Term(Term::LiteralInt(1)),
            YardTerm::BracketEnd,
        ];
        assert_eq!(r, expected);
    }

    #[test]
    fn test_shunting_yard() {
        let term = "a + 5.0 * (c -1)";
        let elements = extract_elements(
            term,
            &Operator::default_map(),
            &("(".to_string(), ")".to_string()),
        );
        let r = shunting_yard(elements);
        let expected = vec![
            YardTerm::Term(Term::Variable("a".to_string())),
            YardTerm::Term(Term::LiteralFloat(5.0)),
            YardTerm::Term(Term::Variable("c".to_string())),
            YardTerm::Term(Term::LiteralInt(1)),
            YardTerm::Operator(Operator::Sub),
            YardTerm::Operator(Operator::Mul),
            YardTerm::Operator(Operator::Add),
        ];
        assert_eq!(r, expected);
        let term = "a * 5.0 * (c * (1+4))-3";
        let elements = extract_elements(
            term,
            &Operator::default_map(),
            &("(".to_string(), ")".to_string()),
        );
        let r = shunting_yard(elements);
        let expected = vec![
            YardTerm::Term(Term::Variable("a".to_string())),
            YardTerm::Term(Term::LiteralFloat(5.0)),
            YardTerm::Operator(Operator::Mul),
            YardTerm::Term(Term::Variable("c".to_string())),
            YardTerm::Term(Term::LiteralInt(1)),
            YardTerm::Term(Term::LiteralInt(4)),
            YardTerm::Operator(Operator::Add),
            YardTerm::Operator(Operator::Mul),
            YardTerm::Operator(Operator::Mul),
            YardTerm::Term(Term::LiteralInt(3)),
            YardTerm::Operator(Operator::Sub),
        ];
        assert_eq!(r, expected);
    }

    #[test]
    fn test_infix_to_term() {
        let term = "a * 5.0 + (c * (1+4))-3";
        let elements = extract_elements(
            term,
            &Operator::default_map(),
            &("(".to_string(), ")".to_string()),
        );
        let r = shunting_yard(elements);
        let t = infix_to_term(r);
        assert!(t.is_ok());
        let t = t.unwrap();
        assert_eq!(
            t,
            Term::BinaryOperator(
                Operator::Add,
                Box::from(Term::BinaryOperator(
                    Operator::Mul,
                    Box::from(Term::Variable("a".to_string())),
                    Box::from(Term::LiteralFloat(5.0))
                )),
                Box::from(Term::BinaryOperator(
                    Operator::Sub,
                    Box::from(Term::BinaryOperator(
                        Operator::Mul,
                        Box::from(Term::Variable("c".to_string())),
                        Box::from(Term::BinaryOperator(
                            Operator::Add,
                            Box::from(Term::LiteralInt(1)),
                            Box::from(Term::LiteralInt(4))
                        ))
                    )),
                    Box::from(Term::LiteralInt(3))
                ))
            )
        )
    }

    #[test]
    fn test_apply() {
        let term = "a * 5.0 + (c * (1+4))-3";
        let elements = extract_elements(
            term,
            &Operator::default_map(),
            &("(".to_string(), ")".to_string()),
        );
        let r = shunting_yard(elements);
        let t = infix_to_term(r).unwrap();
        let result = t.apply(&HashMap::from([
            ("a".to_string(), Term::LiteralFloat(0.)),
            ("c".to_string(), Term::LiteralInt(2)),
        ]));
        assert!(result.is_ok());
        assert_eq!(result.unwrap().extract_f64().unwrap(), 7.);
        let result = t.apply(&HashMap::from([
            ("a".to_string(), Term::LiteralFloat(1.3)),
            ("c".to_string(), Term::LiteralInt(24)),
        ]));
        assert_eq!(result.unwrap().extract_f64().unwrap(), 123.5);
    }
}
