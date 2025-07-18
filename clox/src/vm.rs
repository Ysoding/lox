use std::fmt::Debug;

use anyhow::Result;

use crate::{
    BoundMethod, Chunk, Class, Closure, Gc, GcRef, GcTrace, GcTraceFormatter, Instance, LoxError,
    NativeFunction, OpCode, Table, Upvalue, Value, clock, compile,
};

macro_rules! binary_op {
    ($self:ident, $op:tt) => {{
        let b = $self.pop().as_number().map_err(|_| $self.runtime_error("Operands must be numbers."))?;
        let a = $self.pop().as_number().map_err(|_| $self.runtime_error("Operands must be numbers."))?;
        $self.push((a $op b).into());
    }};
}

const FRAME_MAX_SIZE: usize = 64;
const STACK_MAX_SIZE: usize = FRAME_MAX_SIZE * (u8::MAX as usize + 1);

pub struct VirtualMachine {
    stack: Vec<Value>,
    globals: Table,
    frames: Vec<CallFrame>,
    open_upvalues: Vec<GcRef<Upvalue>>,
    gc: Gc,
    init_string: GcRef<String>,
}

impl VirtualMachine {
    pub fn new() -> Self {
        let mut gc = Gc::new();
        let init_string = gc.intern("init".to_owned());

        let mut vm = Self {
            stack: Vec::with_capacity(STACK_MAX_SIZE),
            globals: Table::new(),
            frames: Vec::with_capacity(FRAME_MAX_SIZE),
            open_upvalues: Vec::new(),
            gc,
            init_string,
        };
        vm.define_native("clock", clock);
        vm
    }

    pub fn reset(&mut self) {
        self.frames.clear();
        self.reset_stack();
    }

    fn current_closure(&self) -> &Closure {
        let closure = self.current_frame().closure;
        self.gc.deref(closure)
    }

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), LoxError> {
        let function = compile(source, &mut self.gc)?;
        self.push(Value::Function(function));
        let closure = self.alloc(Closure::new(function));
        self.frames.push(CallFrame::new(closure, 0));
        self.run()
    }

    fn alloc<T: GcTrace + 'static + Debug>(&mut self, object: T) -> GcRef<T> {
        self.mark_and_sweep();
        self.gc.alloc(object)
    }

    fn intern(&mut self, name: String) -> GcRef<String> {
        self.mark_and_sweep();
        self.gc.intern(name)
    }

    fn mark_and_sweep(&mut self) {
        if self.gc.should_gc() {
            #[cfg(feature = "debug_log_gc")]
            println!("-- gc begin");

            self.mark_roots();
            self.gc.collect_garbage();

            #[cfg(feature = "debug_log_gc")]
            println!("-- gc end");
        }
    }

    fn mark_roots(&mut self) {
        for &value in &self.stack {
            self.gc.mark_value(value);
        }

        for frame in &self.frames {
            self.gc.mark_object(frame.closure)
        }

        for &upvalue in &self.open_upvalues {
            self.gc.mark_object(upvalue);
        }

        self.gc.mark_table(&self.globals);
    }

    fn run(&mut self) -> Result<(), LoxError> {
        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                print!("          ");
                for &ele in &self.stack {
                    print!("[ {} ]", GcTraceFormatter::new(ele, &self.gc));
                }
                println!();
                self.current_chunk()
                    .disassemble_instruction(self.current_frame().ip, &self.gc);
            }

            let instruction = self.read_instruction(self.current_frame().ip);

            self.current_frame_mut().ip += 1;

            match instruction {
                OpCode::Return => {
                    let result = self.pop();
                    let frame = self.frames.pop().unwrap();
                    self.close_upvalues(frame.slot_start);

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
                    self.stack.push(constant);
                }
                OpCode::ConstantLong(c_idx) => {
                    let constant = self.read_constant(c_idx as usize);
                    println!("{}", GcTraceFormatter::new(constant, &self.gc));
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
                    self.push((a == b).into());
                }
                OpCode::Greater => {
                    binary_op!(self, >);
                }
                OpCode::Less => {
                    binary_op!(self, <);
                }
                OpCode::Print => {
                    println!("{}", GcTraceFormatter::new(self.pop(), &self.gc));
                }
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::DefineGlobal(pos) => {
                    let name = self
                        .read_constant(pos as usize)
                        .as_string()
                        .map_err(|msg| self.runtime_error(&msg))?;
                    self.globals.insert(name, self.peek(0));
                    self.pop();
                }
                OpCode::GetGlobal(constant) => {
                    let name = self
                        .read_constant(constant as usize)
                        .as_string()
                        .map_err(|msg| self.runtime_error(&msg))?;
                    if let Some(&v) = self.globals.get(&name) {
                        self.push(v);
                    } else {
                        self.runtime_error(&format!(
                            "Undefined variable '{}'.",
                            self.gc.deref(name)
                        ));
                        return Err(LoxError::RuntimeError);
                    }
                }
                OpCode::SetGlobal(pos) => {
                    let name = self
                        .read_constant(pos as usize)
                        .as_string()
                        .map_err(|msg| self.runtime_error(&msg))?;
                    if !self.globals.contains_key(&name) {
                        self.runtime_error(&format!(
                            "Undefined variable '{}'.",
                            self.gc.deref(name)
                        ));
                        return Err(LoxError::RuntimeError);
                    }

                    self.globals.insert(name, self.peek(0));
                }
                OpCode::GetLocal(slot) => {
                    let slot = slot as usize + self.current_frame().slot_start;
                    self.push(self.stack[slot]);
                }
                OpCode::SetLocal(slot) => {
                    let slot = slot as usize + self.current_frame().slot_start;
                    self.stack[slot] = self.peek(0);
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
                    let function = self.read_constant(constant as usize).as_function().unwrap();
                    let upvalue_count = self.gc.deref(function).upvalues.len();
                    let mut closure = Closure::new(function);

                    for i in 0..upvalue_count {
                        let upvalue = self.gc.deref(function).upvalues[i];
                        let obj_upvalue = if upvalue.is_local {
                            let location = self.current_frame().slot_start + upvalue.index as usize;
                            self.capture_upvalue(location)
                        } else {
                            self.current_closure().upvalues[upvalue.index as usize]
                        };
                        closure.upvalues.push(obj_upvalue)
                    }
                    let closure = self.alloc(closure);
                    self.push(Value::Closure(closure));
                }
                OpCode::GetUpvalue(slot) => {
                    let value = {
                        let upvalue = self.current_closure().upvalues[slot as usize];
                        let upvalue = self.gc.deref(upvalue);
                        if let Some(value) = upvalue.closed {
                            value
                        } else {
                            self.stack[upvalue.location]
                        }
                    };
                    self.push(value);
                }
                OpCode::SetUpvalue(slot) => {
                    let upvalue = self.current_closure().upvalues[slot as usize];
                    let value = self.peek(0);
                    let upvalue = self.gc.deref_mut(upvalue);
                    if upvalue.closed.is_none() {
                        self.stack[upvalue.location] = value;
                    } else {
                        upvalue.closed = Some(value);
                    }
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalues(self.stack.len() - 1);
                    self.pop();
                }
                OpCode::Class(c) => {
                    let name = self.read_constant(c as usize).as_string().unwrap();
                    let class = self.alloc(Class::new(name));
                    self.push(Value::Class(class));
                }
                OpCode::SetProperty(c) => {
                    let instance = self
                        .peek(1)
                        .as_instance()
                        .map_err(|_msg| self.runtime_error("Only instances have fields."))?;

                    let name = self.read_constant(c as usize).as_string().unwrap();

                    let value = self.pop();
                    let instance = self.gc.deref_mut(instance);
                    instance.fields.insert(name, value);
                    self.pop();
                    self.push(value);
                }
                OpCode::GetProperty(c) => {
                    let instance = self
                        .peek(0)
                        .as_instance()
                        .map_err(|_msg| self.runtime_error("Only instances have properties."))?;
                    let instance = self.gc.deref(instance);

                    let name = self.read_constant(c as usize).as_string().unwrap();
                    match instance.fields.get(&name) {
                        Some(&v) => {
                            self.pop();
                            self.push(v);
                        }
                        None => {
                            self.bind_method(instance.class, name)?;
                        }
                    }
                }
                OpCode::Method(c) => {
                    let name = self.read_constant(c as usize).as_string().unwrap();
                    self.define_method(name);
                }
                OpCode::Invoke((c, arg_count)) => {
                    let name = self.read_constant(c as usize).as_string().unwrap();
                    self.invoke(name, arg_count)?;
                }
                OpCode::Inherit => {
                    let superclass = self
                        .peek(1)
                        .as_class()
                        .map_err(|_| self.runtime_error("Superclass must be a class."))?;

                    let subclass = self
                        .peek(0)
                        .as_class()
                        .map_err(|_| self.runtime_error("Subclass must be a class."))?;

                    let methods = self.gc.deref(superclass).methods.clone();

                    let subclass = self.gc.deref_mut(subclass);
                    subclass.methods = methods;
                    self.pop();
                }
                OpCode::GetSuper(c) => {
                    let name = self.read_constant(c as usize).as_string().unwrap();
                    let superclass = self.pop().as_class().unwrap();

                    self.bind_method(superclass, name)?;
                }
                OpCode::SuperInvoke((c, arg_count)) => {
                    let method_name = self.read_constant(c as usize).as_string().unwrap();

                    let class = self.pop().as_class().unwrap();
                    self.invoke_from_class(class, method_name, arg_count)?;
                }
            }
        }
    }

    fn define_method(&mut self, name: GcRef<String>) {
        let method = self.peek(0);
        let class = self.peek(1).as_class().unwrap();

        let class = self.gc.deref_mut(class);
        class.methods.insert(name, method);
        self.pop();
    }

    fn read_instruction(&self, instruction: usize) -> OpCode {
        self.current_chunk().code[instruction]
    }

    fn read_constant(&self, constant_idx: usize) -> Value {
        self.current_chunk().constants[constant_idx]
    }

    fn current_chunk(&self) -> &Chunk {
        let closure = self.current_closure();
        let function = self.gc.deref(closure.function);
        &function.chunk
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn peek(&self, distance: usize) -> Value {
        self.stack[self.stack.len() - 1 - distance]
    }

    fn reset_stack(&mut self) {
        self.stack.clear();
    }

    fn runtime_error(&mut self, msg: &str) -> LoxError {
        let mut err_msg = String::from(msg);

        (0..self.frames.len()).rev().for_each(|i| {
            let frame = &self.frames[i];
            let closure = self.gc.deref(frame.closure);
            let function = self.gc.deref(closure.function);
            err_msg.push_str(&format!(
                "\n[line {}] in ",
                function.chunk.line(frame.ip - 1)
            ));
            let function_name = self.gc.deref(function.name);
            err_msg.push_str(&function_name);
        });

        eprintln!("{}", err_msg);

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

        let b = self.gc.deref(b);
        let a = self.gc.deref(a);
        let res = self.intern(format!("{}{}", a, b));

        self.push(Value::String(res));
    }

    fn call_value(&mut self, arg_count: u8) -> Result<(), LoxError> {
        let callee = self.peek(arg_count as usize);
        match callee {
            Value::BoundMethod(bound) => {
                let bound = self.gc.deref(bound);
                let method = bound.method;
                let receiver = bound.receiver;

                let pos = self.stack.len() - 1 - arg_count as usize;
                self.stack[pos] = receiver;

                self.call(method, arg_count)?;
            }
            Value::Function(_f) => {}
            Value::Class(class) => {
                let instance = Instance::new(class);
                let instance = self.alloc(instance);
                let pos = self.stack.len() - 1 - arg_count as usize;
                self.stack[pos] = Value::Instance(instance);

                let class = self.gc.deref(class);
                if let Some(&initializer) = class.methods.get(&self.init_string) {
                    let initializer = initializer
                        .as_closure()
                        .map_err(|_msg| self.runtime_error("Initializer is not closure"))?;

                    self.call(initializer, arg_count)?;
                } else if arg_count != 0 {
                    return Err(
                        self.runtime_error(&format!("Expected 0 arguments but got {}.", arg_count))
                    );
                }
            }
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
        let name = self.gc.intern(name.to_owned());
        self.globals.insert(name, Value::NativeFunction(native));
    }

    fn call(&mut self, closure_ref: GcRef<Closure>, arg_count: u8) -> Result<(), LoxError> {
        let closure = self.gc.deref(closure_ref);
        let function = self.gc.deref(closure.function);

        if arg_count != function.arity {
            return Err(self.runtime_error(&format!(
                "Expected {} arguments but got {}.",
                function.arity, arg_count
            )));
        }
        if self.frames.len() == FRAME_MAX_SIZE {
            return Err(self.runtime_error("Stack overflow."));
        }

        let call_frame = CallFrame::new(closure_ref, self.stack.len() - arg_count as usize - 1);
        self.frames.push(call_frame);
        Ok(())
    }

    fn capture_upvalue(&mut self, location: usize) -> GcRef<Upvalue> {
        for &ele in &self.open_upvalues {
            let upvalue = self.gc.deref(ele);
            if upvalue.location == location {
                return ele;
            }
        }

        let created_upvalue = Upvalue::new(location);
        let created_upvalue = self.alloc(created_upvalue);
        self.open_upvalues.push(created_upvalue);
        created_upvalue
    }

    fn close_upvalues(&mut self, last: usize) {
        let mut i = 0;

        while i != self.open_upvalues.len() {
            let upvalue = self.open_upvalues[i];
            let upvalue = self.gc.deref_mut(upvalue);
            if upvalue.location >= last {
                self.open_upvalues.remove(i);
                let localtion = upvalue.location;
                upvalue.closed = Some(self.stack[localtion]);
            } else {
                i += 1;
            }
        }
    }

    fn bind_method(&mut self, class: GcRef<Class>, name: GcRef<String>) -> Result<(), LoxError> {
        let class = self.gc.deref(class);
        match class.methods.get(&name) {
            Some(method) => {
                let receiver = self.peek(0);
                let method = method.as_closure().unwrap();

                let bound = self.alloc(BoundMethod::new(receiver, method));
                self.pop();
                self.push(Value::BoundMethod(bound));

                Ok(())
            }
            None => {
                let name = self.gc.deref(name);
                let msg = format!("Undefined property '{}'.", name);
                Err(self.runtime_error(&msg))
            }
        }
    }

    fn invoke(&mut self, name: GcRef<String>, arg_count: u8) -> Result<(), LoxError> {
        let receiver = self.peek(arg_count as usize);

        let instance = receiver
            .as_instance()
            .map_err(|_msg| self.runtime_error("Only instances have methods."))?;

        let instance = self.gc.deref(instance);

        match instance.fields.get(&name) {
            Some(&field) => {
                let pos = self.stack.len() - 1 - arg_count as usize;
                self.stack[pos] = field;

                self.call_value(arg_count)
            }
            None => {
                let class = instance.class;
                self.invoke_from_class(class, name, arg_count)
            }
        }
    }

    fn invoke_from_class(
        &mut self,
        class: GcRef<Class>,
        name: GcRef<String>,
        arg_count: u8,
    ) -> Result<(), LoxError> {
        let class = self.gc.deref(class);
        match class.methods.get(&name) {
            Some(method) => {
                let closure = method.as_closure().unwrap();
                self.call(closure, arg_count)
            }
            None => {
                let name = self.gc.deref(name);
                let msg = format!("Undefined property '{}'.", name);
                Err(self.runtime_error(&msg))
            }
        }
    }
}
struct CallFrame {
    closure: GcRef<Closure>,
    ip: usize,
    slot_start: usize,
}

impl CallFrame {
    fn new(closure: GcRef<Closure>, slot_start: usize) -> Self {
        Self {
            ip: 0,
            closure,
            slot_start,
        }
    }
}
