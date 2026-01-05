use crate::ast::ASTNode;

pub fn generate_code(ast: &[ASTNode]) -> String {
    let mut output = String::new();

    for node in ast {
        match node {
            ASTNode::Print { name } => {
                println!("Print value: {}", name);
            }
            ASTNode::Loop(body) => {
                output.push_str("loop {\n");
                output.push_str(&generate_code(body));
                output.push_str("}\n");
            }
            ASTNode::Condition(condition, body) => {
                output.push_str(&format!("if {} {{\n", condition));
                output.push_str(&generate_code(body));
                output.push_str("}\n");
            }
            ASTNode::VarAssign(name, value) => {
                output.push_str(&format!("let {} = \"{}\".to_string();\n", name, value));
            }
            ASTNode::Add(name, value) => {
                output.push_str(&format!("{} += {};\n", name, value));
            }
            ASTNode::Subtract(name, value) => {
                output.push_str(&format!("{} -= {};\n", name, value));
            }
            ASTNode::Error => {
                output.push_str("// Error: Invalid node encountered.\n");
            }
        }
    }

    output
}
