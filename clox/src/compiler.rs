use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::{array, mem, ops::Add, u16};

use crate::{
    Function, FunctionType, FunctionUpvalue, Gc, GcRef, LoxError, OpCode, Scanner, Token,
    TokenType, Value,
};

pub fn compile(source_code: &str, gc: &mut Gc) -> Result<GcRef<Function>, LoxError> {
    let parser = Parser::new(source_code, gc);
    parser.compile()
}

const MAX_LOCAL_SIZE: usize = u8::MAX as usize + 1;
const UNINITIALIZED_LOCAL_DEPTH: isize = -1;

struct Compiler<'a> {
    enclosing: Option<Box<Compiler<'a>>>,
    function: Function,
    function_type: FunctionType,
    locals: [Local<'a>; MAX_LOCAL_SIZE],
    local_count: usize,
    scope_depth: isize,
}

impl<'a> Compiler<'a> {
    fn new(function_type: FunctionType, name: GcRef<String>) -> Box<Self> {
        let compiler = Self {
            locals: array::from_fn(|_| Local::default()),
            local_count: 1,
            scope_depth: 0,
            function: Function::new(name),
            function_type,
            enclosing: None,
        };

        Box::new(compiler)
    }

    fn resolve_local(&mut self, name: Token<'a>) -> Option<(u8, isize)> {
        (0..self.local_count)
            .rev()
            .find(|&i| self.locals[i].name.lexeme.eq(name.lexeme))
            .map(|i| (i as u8, self.locals[i].depth))
    }

    fn resolve_upvalue(&mut self, name: Token<'a>) -> Result<Option<(u8, isize)>, &'static str> {
        if let Some(enclosing) = self.enclosing.as_mut() {
            if let Some((index, depth)) = enclosing.resolve_local(name) {
                enclosing.locals[index as usize].is_captured = true;
                let index = self.add_upvalue(index, true)?;
                return Ok(Some((index as u8, depth)));
            }

            if let Some((index, depth)) = enclosing.resolve_upvalue(name)? {
                let index = self.add_upvalue(index, false)?;
                return Ok(Some((index as u8, depth)));
            }
        }
        Ok(None)
    }

    fn add_upvalue(&mut self, index: u8, is_local: bool) -> Result<usize, &'static str> {
        if let Some(i) = self
            .function
            .upvalues
            .iter()
            .position(|u| u.index == index && u.is_local == is_local)
        {
            return Ok(i);
        }

        if self.function.upvalues.len() == MAX_LOCAL_SIZE {
            return Err("Too many closure variables in function.");
        }

        self.function
            .upvalues
            .push(FunctionUpvalue { index, is_local });
        Ok(self.function.upvalues.len() - 1)
    }
}

#[derive(Clone, Default)]
struct Local<'a> {
    pub name: Token<'a>,
    pub depth: isize,
    pub is_captured: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Add<u8> for Precedence {
    type Output = Self;

    fn add(self, rhs: u8) -> Self::Output {
        Self::try_from(self as u8 + rhs).unwrap()
    }
}

struct Parser<'a> {
    previous: Token<'a>,
    current: Token<'a>,
    scanner: Scanner<'a>,
    had_error: bool,
    panic_mode: bool,
    compiler: Box<Compiler<'a>>,
    gc: &'a mut Gc,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str, gc: &'a mut Gc) -> Self {
        let function_name = gc.intern("script".to_owned());
        Self {
            gc,
            scanner: Scanner::new(source),
            previous: Token::default(),
            current: Token::default(),
            had_error: false,
            panic_mode: false,
            compiler: Compiler::new(FunctionType::Script, function_name),
        }
    }

    fn advance(&mut self) {
        self.previous = self.current;

        while let Some(token) = self.scanner.next() {
            self.current = token;

            if self.current.typ == TokenType::Error {
                self.error_at_current(self.current.lexeme);
            } else {
                break;
            }
        }
    }

    fn compile(mut self) -> Result<GcRef<Function>, LoxError> {
        self.had_error = false;
        self.panic_mode = false;

        self.advance();

        while !self._match(TokenType::Eof) {
            self.declaration();
        }

        self.emit_return();

        if self.had_error {
            return Err(LoxError::CompileError);
        }

        Ok(self.gc.alloc(self.compiler.function))
    }

    fn consume(&mut self, typ: TokenType, msg: &str) {
        if self.current.typ == typ {
            self.advance();
        } else {
            self.error_at_current(msg);
        }
    }

    fn error_at_current(&mut self, msg: &str) {
        self.error_at(self.current, msg);
    }

    fn error(&mut self, msg: &str) {
        self.error_at(self.previous, msg);
    }

    fn error_at(&mut self, token: Token<'a>, msg: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprint!("[line {}] Error", token.line);
        if token.typ == TokenType::Eof {
            eprint!(" at end");
        } else if token.typ == TokenType::Error {
            // do nothing
        } else {
            eprint!(" at '{}'", token.lexeme);
        }
        eprintln!(": {msg}");
        self.had_error = true;
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn emit_byte(&mut self, byte: OpCode) {
        self.compiler
            .function
            .chunk
            .write_chunk(byte, self.previous.line);
    }

    fn emit_return(&mut self) {
        #[cfg(feature = "debug_trace_execution")]
        {
            if !self.had_error {
                let name = self.gc.deref(self.compiler.function.name);
                self.compiler.function.chunk.disassemble(name, &self.gc);
            }
        }

        self.emit_byte(OpCode::Nil);
        self.emit_byte(OpCode::Return);
    }

    fn emit_two_bytes(&mut self, byte1: OpCode, byte2: OpCode) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_constant(&mut self, val: Value) {
        let constant = self.make_constant(val);
        if constant < u8::MAX as usize {
            // Use OP_CONSTANT for indices 0-255
            self.emit_byte(OpCode::Constant(constant as u8));
        } else if constant < (1 << 24) {
            // Use OP_CONSTANT_LONG for indices 256-16,777,215
            self.emit_byte(OpCode::ConstantLong(constant as u32));
        } else {
            self.error("Too many constants in one chunk.");
        }
    }

    fn make_constant(&mut self, val: Value) -> usize {
        self.compiler.function.chunk.add_constant(val)
    }

    fn number(&mut self, _can_assign: bool) {
        self.emit_constant(self.previous.lexeme.parse::<f64>().unwrap().into());
    }

    fn and(&mut self, _can_assign: bool) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse(0));
        self.emit_byte(OpCode::Pop);
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    fn or(&mut self, _can_assign: bool) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse(0));
        let end_jump = self.emit_jump(OpCode::Jump(0));

        self.patch_jump(else_jump);
        self.emit_byte(OpCode::Pop);

        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn call(&mut self, _can_assign: bool) {
        let arg_count = self.argument_list();
        self.emit_byte(OpCode::Call(arg_count));
    }

    fn argument_list(&mut self) -> u8 {
        let mut arg_count = 0;
        if !self.check(TokenType::RightParen) {
            loop {
                self.expression();
                if arg_count == u8::MAX {
                    self.error("Can't have more than 255 arguments.");
                }

                arg_count += 1;
                if !self._match(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        arg_count
    }

    fn grouping(&mut self, _can_assign: bool) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self, _can_assign: bool) {
        let operator_type = self.previous.typ;

        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Bang => {
                self.emit_byte(OpCode::Not);
            }
            TokenType::Minus => {
                self.emit_byte(OpCode::Negate);
            }
            _ => {}
        }
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.previous, can_assign)
    }

    fn binary(&mut self, _can_assign: bool) {
        let operator_type = self.previous.typ;
        let rule = ParseRule::get_rule(operator_type);
        self.parse_precedence(rule.precedence + 1);

        match operator_type {
            TokenType::BangEqual => {
                self.emit_two_bytes(OpCode::Equal, OpCode::Not);
            }
            TokenType::EqualEqual => {
                self.emit_byte(OpCode::Equal);
            }
            TokenType::Greater => {
                self.emit_byte(OpCode::Greater);
            }
            TokenType::GreaterEqual => {
                self.emit_two_bytes(OpCode::Less, OpCode::Not);
            }
            TokenType::Less => {
                self.emit_byte(OpCode::Less);
            }
            TokenType::LessEqual => {
                self.emit_two_bytes(OpCode::Greater, OpCode::Not);
            }
            TokenType::Plus => {
                self.emit_byte(OpCode::Add);
            }
            TokenType::Minus => {
                self.emit_byte(OpCode::Subtract);
            }
            TokenType::Star => {
                self.emit_byte(OpCode::Multiply);
            }
            TokenType::Slash => {
                self.emit_byte(OpCode::Divide);
            }
            _ => {}
        }
    }

    fn string(&mut self, _can_assign: bool) {
        let value = &self.previous.lexeme[1..self.previous.lexeme.len() - 1];
        let s = self.gc.intern(value.to_owned());
        self.emit_constant(Value::String(s));
    }

    fn literal(&mut self, _can_assign: bool) {
        match self.previous.typ {
            TokenType::False => {
                self.emit_byte(OpCode::False);
            }
            TokenType::Nil => {
                self.emit_byte(OpCode::Nil);
            }
            TokenType::True => {
                self.emit_byte(OpCode::True);
            }
            _ => {}
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let prefix_rule = match ParseRule::get_rule(self.previous.typ).prefix {
            Some(prefix_rule) => prefix_rule,
            None => {
                self.error("Expect expression.");
                return;
            }
        };

        let can_assign = precedence <= Precedence::Assignment;
        prefix_rule(self, can_assign);

        while precedence <= ParseRule::get_rule(self.current.typ).precedence {
            self.advance();

            let infix_rule = ParseRule::get_rule(self.previous.typ).infix.unwrap();

            infix_rule(self, can_assign);
        }

        if can_assign && self._match(TokenType::Equal) {
            self.error("Invalid assignment target.");
        }
    }

    fn _match(&mut self, typ: TokenType) -> bool {
        if !self.check(typ) {
            return false;
        }
        self.advance();
        return true;
    }

    fn declaration(&mut self) {
        if self._match(TokenType::Fun) {
            self.fun_declaration();
        } else if self._match(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        if self._match(TokenType::Print) {
            self.print_statement();
        } else if self._match(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if self._match(TokenType::If) {
            self.if_statement();
        } else if self._match(TokenType::Return) {
            self.return_statement();
        } else if self._match(TokenType::While) {
            self.while_statement();
        } else if self._match(TokenType::For) {
            self.for_statement();
        } else {
            self.expression_statement();
        }
    }

    fn return_statement(&mut self) {
        if self.compiler.function_type == FunctionType::Script {
            self.error("Can't return from top-level code.");
        }

        if self._match(TokenType::Semicolon) {
            self.emit_return();
        } else {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value.");
            self.emit_byte(OpCode::Return);
        }
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");

        if self._match(TokenType::Semicolon) {
            // No initializer.
        } else if self._match(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.compiler.function.chunk.code.len();

        let mut exit_jump = None;
        if !self._match(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

            // Jump out of the loop if the condition is false.
            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse(0)));
            self.emit_byte(OpCode::Pop);
        }

        if !self._match(TokenType::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump(0));
            let increment_start = self.compiler.function.chunk.code.len();
            self.expression();
            self.emit_byte(OpCode::Pop);

            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit_byte(OpCode::Pop);
        }

        self.end_scope();
    }

    fn while_statement(&mut self) {
        let loop_start = self.compiler.function.chunk.code.len();
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse(0));
        self.emit_byte(OpCode::Pop);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_byte(OpCode::Pop);
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse(0));
        self.emit_byte(OpCode::Pop);
        self.statement();

        let else_jump = self.emit_jump(OpCode::Jump(0));

        self.patch_jump(then_jump);
        self.emit_byte(OpCode::Pop);

        if self._match(TokenType::Else) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.compiler.function.chunk.code.len() - offset;
        if jump > u16::MAX as usize {
            self.error("Too much code to jump over.");
        }

        self.compiler.function.chunk.code[offset - 1].patch_jump(jump as u16);
    }

    fn emit_jump(&mut self, instruction: OpCode) -> usize {
        self.emit_byte(instruction);
        self.compiler.function.chunk.code.len()
    }

    fn emit_loop(&mut self, loop_start: usize) {
        let jump = self.compiler.function.chunk.code.len() - loop_start + 1;
        if jump > u16::MAX as usize {
            self.error("Loop body too large.");
        }

        self.emit_byte(OpCode::Loop(jump as u16));
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_byte(OpCode::Print);
    }

    fn check(&self, typ: TokenType) -> bool {
        self.current.typ == typ
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_byte(OpCode::Pop);
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;
        while self.current.typ != TokenType::Eof {
            if self.previous.typ == TokenType::Semicolon {
                return;
            }

            match self.current.typ {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => {
                    return;
                }
                _ => {}
            }

            self.advance();
        }
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function name.");
        self.mark_initialized();
        self.function(FunctionType::Function);
        self.define_variable(global);
    }

    fn function(&mut self, function_type: FunctionType) {
        self.push_compiler(function_type);
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        if !self.check(TokenType::RightParen) {
            loop {
                match self.compiler.function.arity.checked_add(1) {
                    Some(v) => self.compiler.function.arity = v,
                    None => self.error_at_current("Can't have more than 255 parameters."),
                }
                let constant = self.parse_variable("Expect parameter name.");
                self.define_variable(constant);

                if !self._match(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        self.consume(TokenType::LeftBrace, "Expect '{' before function body.");
        self.block();

        let compiler = self.pop_compiler();
        let function = self.gc.alloc(compiler.function);
        let constant = self.make_constant(Value::Function(function));
        self.emit_byte(OpCode::Closure(constant as u8));
    }

    fn push_compiler(&mut self, function_type: FunctionType) {
        let function_name = self.gc.intern(self.previous.lexeme.to_owned());
        let new_compiler = Compiler::new(function_type, function_name);
        let old_compiler = mem::replace(&mut self.compiler, new_compiler);
        self.compiler.enclosing = Some(old_compiler);
    }

    fn pop_compiler(&mut self) -> Box<Compiler<'a>> {
        self.emit_return();
        match self.compiler.enclosing.take() {
            Some(enclosing) => mem::replace(&mut self.compiler, enclosing),
            None => panic!("pop_compiler: No enclosing compiler"),
        }
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self._match(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_byte(OpCode::Nil);
        }

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    fn parse_variable(&mut self, error_msg: &str) -> usize {
        self.consume(TokenType::Identifier, error_msg);

        self.declare_variable();
        if self.compiler.scope_depth > 0 {
            return 0;
        }

        self.identifier_constant(self.previous.lexeme)
    }

    fn declare_variable(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }

        let name = self.previous;

        for i in (0..self.compiler.local_count).rev() {
            let local = &self.compiler.locals[i];
            if local.depth != UNINITIALIZED_LOCAL_DEPTH && local.depth < self.compiler.scope_depth {
                break;
            }

            if name.lexeme.eq(local.name.lexeme) {
                self.error("Already a variable with this name in this scope.");
            }
        }

        self.add_local(name);
    }

    fn define_variable(&mut self, global: usize) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        self.emit_byte(OpCode::DefineGlobal(global as u8));
    }

    fn identifier_constant(&mut self, identifier: &str) -> usize {
        let v = self.gc.intern(identifier.to_owned());
        self.make_constant(Value::String(v))
    }

    fn named_variable(&mut self, name: Token<'a>, can_assign: bool) {
        let (get_op, set_op) = if let Some((pos, depth)) = self.compiler.resolve_local(name) {
            if depth == UNINITIALIZED_LOCAL_DEPTH {
                self.error("Can't read local variable in its own initializer.");
                return;
            }
            (OpCode::GetLocal(pos as u8), OpCode::SetLocal(pos as u8))
        } else if let Some((pos, _)) = self
            .compiler
            .resolve_upvalue(name)
            .inspect_err(|err| self.error(err))
            .ok()
            .flatten()
        {
            (OpCode::GetUpvalue(pos), OpCode::SetUpvalue(pos))
        } else {
            let arg = self.identifier_constant(name.lexeme);
            (OpCode::GetGlobal(arg as u8), OpCode::SetGlobal(arg as u8))
        };

        if can_assign && self._match(TokenType::Equal) {
            self.expression();
            self.emit_byte(set_op);
        } else {
            self.emit_byte(get_op);
        }
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;

        while self.compiler.local_count > 0
            && self.compiler.locals[self.compiler.local_count - 1].depth > self.compiler.scope_depth
        {
            if self.compiler.locals[self.compiler.local_count - 1].is_captured {
                self.emit_byte(OpCode::CloseUpvalue);
            } else {
                self.emit_byte(OpCode::Pop);
            }
            self.compiler.local_count -= 1;
        }
    }

    fn add_local(&mut self, name: Token<'a>) {
        if self.compiler.local_count == MAX_LOCAL_SIZE {
            self.error("Too many local variables in function.");
            return;
        }

        let local = &mut self.compiler.locals[self.compiler.local_count];
        self.compiler.local_count += 1;
        local.name = name;
        local.depth = UNINITIALIZED_LOCAL_DEPTH;
        local.is_captured = false;
    }

    fn mark_initialized(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }
        self.compiler.locals[self.compiler.local_count - 1].depth = self.compiler.scope_depth;
    }
}

type ParseFn<'a> = fn(&mut Parser<'a>, bool /* can_assign */);

struct ParseRule<'a> {
    prefix: Option<ParseFn<'a>>,
    infix: Option<ParseFn<'a>>,
    precedence: Precedence,
}

impl<'a> ParseRule<'a> {
    fn new(
        prefix: Option<ParseFn<'a>>,
        infix: Option<ParseFn<'a>>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }

    fn get_rule(typ: TokenType) -> Self {
        match typ {
            TokenType::LeftParen => {
                Self::new(Some(Parser::grouping), Some(Parser::call), Precedence::Call)
            }
            TokenType::RightParen => Self::new(None, None, Precedence::None),
            TokenType::LeftBrace => Self::new(None, None, Precedence::None),
            TokenType::RightBrace => Self::new(None, None, Precedence::None),
            TokenType::Comma => Self::new(None, None, Precedence::None),
            TokenType::Dot => Self::new(None, None, Precedence::None),
            TokenType::Minus => {
                Self::new(Some(Parser::unary), Some(Parser::binary), Precedence::Term)
            }
            TokenType::Plus => Self::new(None, Some(Parser::binary), Precedence::Term),
            TokenType::Semicolon => Self::new(None, None, Precedence::None),
            TokenType::Slash => Self::new(None, Some(Parser::binary), Precedence::Factor),
            TokenType::Star => Self::new(None, Some(Parser::binary), Precedence::Factor),
            TokenType::Bang => Self::new(Some(Parser::unary), None, Precedence::None),
            TokenType::BangEqual => Self::new(None, Some(Parser::binary), Precedence::Equality),
            TokenType::Equal => Self::new(None, None, Precedence::None),
            TokenType::EqualEqual => Self::new(None, Some(Parser::binary), Precedence::Equality),
            TokenType::Greater => Self::new(None, Some(Parser::binary), Precedence::Comparison),
            TokenType::GreaterEqual => {
                Self::new(None, Some(Parser::binary), Precedence::Comparison)
            }
            TokenType::Less => Self::new(None, Some(Parser::binary), Precedence::Comparison),
            TokenType::LessEqual => Self::new(None, Some(Parser::binary), Precedence::Comparison),
            TokenType::Identifier => Self::new(Some(Parser::variable), None, Precedence::None),
            TokenType::String => Self::new(Some(Parser::string), None, Precedence::None),
            TokenType::Number => Self::new(Some(Parser::number), None, Precedence::None),
            TokenType::And => Self::new(None, Some(Parser::and), Precedence::None),
            TokenType::Class => Self::new(None, None, Precedence::None),
            TokenType::Else => Self::new(None, None, Precedence::None),
            TokenType::False => Self::new(Some(Parser::literal), None, Precedence::None),
            TokenType::Fun => Self::new(None, None, Precedence::None),
            TokenType::For => Self::new(None, None, Precedence::None),
            TokenType::If => Self::new(None, None, Precedence::None),
            TokenType::Nil => Self::new(Some(Parser::literal), None, Precedence::None),
            TokenType::Or => Self::new(None, Some(Parser::or), Precedence::None),
            TokenType::Print => Self::new(None, None, Precedence::None),
            TokenType::Return => Self::new(None, None, Precedence::None),
            TokenType::Super => Self::new(None, None, Precedence::None),
            TokenType::This => Self::new(None, None, Precedence::None),
            TokenType::True => Self::new(Some(Parser::literal), None, Precedence::None),
            TokenType::Var => Self::new(None, None, Precedence::None),
            TokenType::While => Self::new(None, None, Precedence::None),
            TokenType::Break => Self::new(None, None, Precedence::None),
            TokenType::Eof => Self::new(None, None, Precedence::None),
            TokenType::Error => Self::new(None, None, Precedence::None),
        }
    }
}
