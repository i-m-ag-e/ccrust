use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Function { params: Vec<Type> },
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Function { params } => write!(
                f,
                "fn({})",
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}
