// ns_project_root/ns_core/src/ast.rs
use std::rc::Rc;

// Using Box for heap allocation of recursive enum variants or struct fields.

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Number(f64),
    String(Rc<String>),
    Boolean(bool),
    NoneLiteral, // Represents the 'none' keyword parsed as a literal
    Symbol(Rc<String>),
    Quote(Box<QuotedData>),
    Call(Box<CallNode>), // Boxed to keep Expression size known
    If(Box<IfNode>),
    Let(Box<LetNode>),
    Lambda(Box<LambdaNode>),
    Get(Box<GetNode>),
    Set(Box<SetNode>),
    While(Box<WhileNode>),
    Begin(Box<BeginNode>),
}

/// Represents the literal data structure inside a quote.
/// This matches your Python parser which stores SymbolNode, lists, or primitives.
#[derive(Debug, Clone, PartialEq)]
pub enum QuotedData {
    Symbol(Rc<String>), // e.g., 'foo results in QuotedData::Symbol("foo")
    Number(f64),
    String(Rc<String>),
    Boolean(bool),
    None,                  // e.g., 'none results in QuotedData::None
    List(Vec<QuotedData>), // e.g., '(a 1) results in QuotedData::List([Symbol("a"), Number(1)])
    // No NestedQuote here, the parser should handle ''foo as Quote(Quote(Symbol("foo")))
    // which means the outer QuoteNode in Expression contains a QuotedData::Symbol("quote")
    // if we represent ' 'foo as (quote (quote foo)). Or, if your parser makes
    // QuoteNode(expression=QuoteNode(expression=SymbolNode("foo"))), then this QuotedData
    // doesn't need to handle nested quotes itself, the Expression::Quote will.
    // Let's assume the parser will resolve ' 'foo into Expression::Quote(QuotedData::List([Symbol("quote"), QuotedData::Symbol("foo")])) if we want ' 'foo to be a list.
    // Or more directly, Expression::Quote(Box::new(QuotedData::Quote(Box::new(QuotedData::Symbol("foo")))))
    // For simplicity now, we'll assume the parser creates nested Expression::Quote if needed.
    // The Python parser for '(a 'b) results in (SymbolNode a, QuoteNode(SymbolNode b)) inside the list.
    // So a QuotedData::List can contain other QuotedData including one representing a nested quote.
    // Let's add QuotedData::Quote for this.
    Quote(Box<QuotedData>), // Represents a nested quote like in '(a 'b) -> List([Symbol(a), Quote(Symbol(b))])
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallNode {
    pub callable_expr: Expression, // Changed from Box<Expression> as Expression itself can be small (e.g. Symbol)
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfNode {
    pub condition: Expression,
    pub then_branch: Expression,
    pub else_branch: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetBinding {
    pub name: Rc<String>, // Symbol name
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetNode {
    pub bindings: Vec<LetBinding>,
    pub body: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaNode {
    pub params: Vec<Rc<String>>, // Parameter names
    pub body: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GetNode {
    pub instance: Expression,
    pub field_name: Rc<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SetNode {
    pub instance: Expression,
    pub field_name: Rc<String>,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileNode {
    pub condition: Expression,
    pub body: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BeginNode {
    pub expressions: Vec<Expression>,
}

// --- Top-Level Forms ---
#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelForm {
    StaticDef(StaticNode),
    FnDef(FnNode),
    StructDef(StructDefNode),
    Use(UseNode),
    Expression(Expression), // A top-level expression to be evaluated
}

#[derive(Debug, Clone, PartialEq)]
pub struct StaticNode {
    pub name: Rc<String>,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnNode {
    pub name: Rc<String>,
    pub params: Vec<Rc<String>>,
    pub body: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefNode {
    pub name: Rc<String>,        // Struct type name
    pub fields: Vec<Rc<String>>, // Field names
}

#[derive(Debug, Clone, PartialEq)]
pub enum UseItems {
    All,                       // Represents *
    Specific(Vec<Rc<String>>), // List of specific symbol names to import
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseNode {
    pub module_name: Rc<String>,
    pub items: UseItems,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProgramNode {
    pub forms: Vec<TopLevelForm>,
}
