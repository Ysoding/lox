use std::{cell::RefCell, collections::HashMap, rc::Rc};

use anyhow::Result;

use crate::{Chunk, Closure, LoxError, NativeFunction, OpCode, Upvalue, Value, clock, compile};

macro_rules! binary_op {
    ($self:ident, $op:tt) => {{
        let b = $self.pop().as_number().map_err(|_| $self.runtime_error("Operands must be numbers."))?;
        let a = $self.pop().as_number().map_err(|_| $self.runtime_error("Operands must be numbers."))?;
        $self.push((a $op b).into());
    }};
}

type Table = HashMap<String, Value>;

const FRAME_MAX_SIZE: usize = 64;
const STACK_MAX_SIZE: usize = FRAME_MAX_SIZE * (u8::MAX as usize + 1);

pub struct VirtualMachine {
    stack: Vec<Value>,
    globals: Table,
    frames: Vec<CallFrame>,
    open_upvalues: Option<Rc<RefCell<Upvalue>>>,
}

impl VirtualMachine {
    pub fn new() -> Self {
        let mut vm = Self {
            stack: Vec::with_capacity(STACK_MAX_SIZE),
            globals: Table::new(),
            frames: Vec::with_capacity(FRAME_MAX_SIZE),
            open_upvalues: None,
        };
        vm.define_native("clock", clock);
        vm
    }

    pub fn reset(&mut self) {
        self.frames.clear();
        self.reset_stack();
    }

    fn current_closure(&self) -> &Closure {
        &self.current_frame().closure
    }

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), LoxError> {
        let function = compile(source)?;
        self.push(Value::Function(function.clone()));
        let closure = Closure::new(function);
        self.frames.push(CallFrame::new(closure, 0));
        self.run()
    }

    fn run(&mut self) -> Result<(), LoxError> {
        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                print!("          ");
                for ele in &self.stack {
                    print!("[ {} ]", *ele);
                }
                println!();
                self.current_chunk()
                    .disassemble_instruction(self.current_frame().ip);
            }

            let instruction = self.read_instruction(self.current_frame().ip);

            self.current_frame_mut().ip += 1;

            match instruction {
                OpCode::Return => {
                    let result = self.pop();
                    let frame = self.frames.pop().unwrap();

                    if self.frames.is_empty() {
                        self.pop();
                        return Ok(());
                    } else {
                        self.stack.truncate(frame.slot_start);
                        self.push(result);
                    }
                }
                OpCode::Constant(c_idx) => {
                    let constant = self.read_constant(c_idx as usize);
                    self.stack.push(constant.clone());
                }
                OpCode::ConstantLong(c_idx) => {
                    let constant = self.read_constant(c_idx as usize);
                    println!("{}", constant);
                }
                OpCode::Unknown => {
                    panic!("Unknown instruction")
                }
                OpCode::Negate => {
                    let v = self
                        .pop()
                        .as_number()
                        .map_err(|_| self.runtime_error("Operand must be a number."))?;

                    self.push((-v).into());
                }
                OpCode::Add => {
                    let v1 = self.peek(0);
                    let v2 = self.peek(1);
                    if v1.is_string() && v2.is_string() {
                        self.concatenate();
                    } else if v1.is_number() && v2.is_number() {
                        binary_op!(self, +);
                    } else {
                        self.runtime_error("Operands must be two numbers or two strings.");
                        return Err(LoxError::RuntimeError);
                    }
                }
                OpCode::Subtract => {
                    binary_op!(self, -);
                }
                OpCode::Multiply => {
                    binary_op!(self, *);
                }
                OpCode::Divide => {
                    binary_op!(self, /);
                }
                OpCode::Nil => {
                    self.push(Value::Nil);
                }
                OpCode::True => {
                    self.push(Value::Bool(true));
                }
                OpCode::False => {
                    self.push(Value::Bool(false));
                }
                OpCode::Not => {
                    let v = self.pop().is_falsy();
                    self.push(v.into());
                }
                OpCode::Equal => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push((a.equal(&b)).into());
                }
                OpCode::Greater => {
                    binary_op!(self, >);
                }
                OpCode::Less => {
                    binary_op!(self, <);
                }
                OpCode::Print => {
                    println!("{}", self.pop());
                }
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::DefineGlobal(pos) => {
                    let name = self
                        .read_constant(pos as usize)
                        .clone()
                        .as_string()
                        .map_err(|msg| self.runtime_error(&msg))?;
                    self.globals.insert(name, self.peek(0).clone());
                    self.pop();
                }
                OpCode::GetGlobal(pos) => {
                    let name = self
                        .read_constant(pos as usize)
                        .clone()
                        .as_string()
                        .map_err(|msg| self.runtime_error(&msg))?;
                    if let Some(v) = self.globals.get(&name) {
                        self.push(v.clone());
                    } else {
                        self.runtime_error(&format!("Undefined variable '{}'.", name));
                        return Err(LoxError::RuntimeError);
                    }
                }
                OpCode::SetGlobal(pos) => {
                    let name = self
                        .read_constant(pos as usize)
                        .clone()
                        .as_string()
                        .map_err(|msg| self.runtime_error(&msg))?;
                    if !self.globals.contains_key(&name) {
                        self.runtime_error(&format!("Undefined variable '{}'.", name));
                        return Err(LoxError::RuntimeError);
                    }

                    self.globals.insert(name, self.peek(0).clone());
                }
                OpCode::GetLocal(slot) => {
                    let slot = slot as usize + self.current_frame().slot_start;
                    self.push(self.stack[slot].clone());
                }
                OpCode::SetLocal(slot) => {
                    let slot = slot as usize + self.current_frame().slot_start;
                    self.stack[slot] = self.peek(0).clone();
                }
                OpCode::JumpIfFalse(offset) => {
                    if self.peek(0).is_falsy() {
                        self.current_frame_mut().ip += offset as usize;
                    }
                }
                OpCode::Jump(offset) => {
                    self.current_frame_mut().ip += offset as usize;
                }
                OpCode::Loop(offset) => {
                    self.current_frame_mut().ip -= offset as usize;
                }
                OpCode::Call(arg_count) => {
                    self.call_value(arg_count)?;
                }
                OpCode::Closure(constant) => {
                    let function = self
                        .read_constant(constant as usize)
                        .clone()
                        .as_function()
                        .unwrap();
                    let upvalue_count = function.upvalues.len();
                    let mut closure = Closure::new(function.clone());

                    for i in 0..upvalue_count {
                        let upvalue = function.upvalues[i].clone();
                        let obj_upvalue = if upvalue.is_local {
                            let location = self.current_frame().slot_start + upvalue.index as usize;
                            self.capture_upvalue(location)
                        } else {
                            self.current_closure().upvalues[upvalue.index as usize].clone()
                        };
                        closure.upvalues.push(obj_upvalue)
                    }

                    self.push(Value::Closure(closure));
                }
                OpCode::GetUpvalue(slot) => {
                    self.push(
                        self.stack[self.current_closure().upvalues[slot as usize].location].clone(),
                    );
                }
                OpCode::SetUpvalue(slot) => {
                    let idx = self.current_closure().upvalues[slot as usize].location;
                    self.stack[idx] = self.peek(0).clone();
                }
                OpCode::CloseUpvalue => {}
            }
        }
    }

    fn read_instruction(&self, instruction: usize) -> OpCode {
        *self.current_chunk().code.get(instruction).unwrap()
    }

    fn read_constant(&self, constant_idx: usize) -> &Value {
        self.current_chunk().constants.get(constant_idx).unwrap()
    }

    fn current_chunk(&self) -> &Chunk {
        &self.current_frame().closure.function.chunk
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - distance]
    }

    fn reset_stack(&mut self) {
        self.stack.clear();
    }

    fn runtime_error(&mut self, msg: &str) -> LoxError {
        let mut err_msg = String::from(msg);

        (0..self.frames.len()).rev().for_each(|i| {
            let frame = &self.frames[i];
            let function = &frame.closure.function;
            err_msg.push_str(&format!(
                "\n[line {}] in ",
                function.chunk.line(frame.ip - 1)
            ));
            let name = if function.name.is_empty() {
                "script"
            } else {
                &format!("{}()", function.name)
            };
            err_msg.push_str(name);
            // err_msg.push('\n');
        });

        println!("{}", err_msg);

        self.reset_stack();

        LoxError::RuntimeError
    }

    fn concatenate(&mut self) {
        let b = self
            .pop()
            .as_string()
            .unwrap_or_else(|msg| panic!("{}", msg));
        let a = self
            .pop()
            .as_string()
            .unwrap_or_else(|msg| panic!("{}", msg));

        self.push(Value::String(format!("{}{}", a, b)));
    }

    fn call_value(&mut self, arg_count: u8) -> Result<(), LoxError> {
        let callee = self.peek(arg_count as usize).clone();
        match callee {
            Value::Function(f) => {}
            Value::NativeFunction(f) => {
                let left = self.stack.len() - arg_count as usize;
                let result = f(self.stack[left..].to_vec());
                // Stack should be restored after native function called
                self.stack.truncate(left - 1);
                self.push(result);
            }
            Value::Closure(c) => {
                self.call(c, arg_count)?;
            }
            _ => {
                return Err(self.runtime_error("Can only call functions and classes.".into()));
            }
        }
        Ok(())
    }

    fn define_native(&mut self, name: &str, native: NativeFunction) {
        self.globals
            .insert(name.to_string(), Value::NativeFunction(native));
    }

    fn call(&mut self, closure: Closure, arg_count: u8) -> Result<(), LoxError> {
        if arg_count != closure.function.arity {
            return Err(self.runtime_error(&format!(
                "Expected {} arguments but got {}.",
                closure.function.arity, arg_count
            )));
        }
        if self.frames.len() == FRAME_MAX_SIZE {
            return Err(self.runtime_error("Stack overflow."));
        }

        let call_frame = CallFrame::new(closure, self.stack.len() - arg_count as usize - 1);
        self.frames.push(call_frame);
        Ok(())
    }

    fn capture_upvalue(&mut self, location: usize) -> Upvalue {
        let created_upvalue = Upvalue::new(location);

        created_upvalue
    }
}

struct CallFrame {
    closure: Closure,
    ip: usize,
    slot_start: usize,
}

impl CallFrame {
    fn new(closure: Closure, slot_start: usize) -> Self {
        Self {
            ip: 0,
            closure,
            slot_start,
        }
    }
}
