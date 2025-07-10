use std::{collections::HashMap, mem};

use crate::{Chunk, GcRef, GcTrace, OpCode, Value};

pub type NativeFunction = fn(Vec<Value>) -> Value;

pub type Table = HashMap<GcRef<String>, Value>;

#[derive(Debug)]
pub struct BoundMethod {
    pub receiver: Value,
    pub method: GcRef<Closure>,
}

impl BoundMethod {
    pub fn new(receiver: Value, method: GcRef<Closure>) -> Self {
        Self { receiver, method }
    }
}

impl GcTrace for BoundMethod {
    fn format(&self, f: &mut std::fmt::Formatter, gc: &crate::Gc) -> std::fmt::Result {
        let method = gc.deref(self.method);
        method.format(f, gc)
    }

    fn size(&self) -> usize {
        mem::size_of::<BoundMethod>()
    }

    fn trace(&self, gc: &mut crate::Gc) {
        gc.mark_value(self.receiver);
        gc.mark_object(self.method);
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

#[derive(Debug)]
pub struct Instance {
    pub class: GcRef<Class>,
    pub fields: Table,
}

impl Instance {
    pub fn new(class: GcRef<Class>) -> Self {
        Self {
            class,
            fields: Table::new(),
        }
    }
}

impl GcTrace for Instance {
    fn format(&self, f: &mut std::fmt::Formatter, gc: &crate::Gc) -> std::fmt::Result {
        let class = gc.deref(self.class);
        let name = gc.deref(class.name);
        write!(f, "{} instance", name)
    }

    fn size(&self) -> usize {
        mem::size_of::<Instance>()
            + self.fields.capacity() * (mem::size_of::<GcRef<String>>() + mem::size_of::<Value>())
    }

    fn trace(&self, gc: &mut crate::Gc) {
        gc.mark_object(self.class);
        gc.mark_table(&self.fields);
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

#[derive(Debug)]
pub struct Class {
    pub name: GcRef<String>,
    pub methods: Table,
}

impl Class {
    pub fn new(name: GcRef<String>) -> Self {
        Self {
            name,
            methods: Table::new(),
        }
    }
}

impl GcTrace for Class {
    fn format(&self, f: &mut std::fmt::Formatter, gc: &crate::Gc) -> std::fmt::Result {
        let name = gc.deref(self.name);
        write!(f, "{}", name)
    }

    fn size(&self) -> usize {
        mem::size_of::<Class>()
    }

    fn trace(&self, gc: &mut crate::Gc) {
        gc.mark_object(self.name);
        gc.mark_table(&self.methods);
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FunctionType {
    Function,
    Script,
    Method,
    Initializer,
}

#[derive(Copy, Clone, Debug)]
pub struct FunctionUpvalue {
    pub index: u8,
    pub is_local: bool,
}

#[derive(Debug)]
pub struct Function {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: GcRef<String>,
    pub upvalues: Vec<FunctionUpvalue>,
}

impl Function {
    pub fn new(name: GcRef<String>) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name,
            upvalues: Vec::new(),
        }
    }
}

impl GcTrace for Function {
    fn format(&self, f: &mut std::fmt::Formatter, gc: &crate::Gc) -> std::fmt::Result {
        let name = gc.deref(self.name);
        if name.is_empty() {
            write!(f, "<script>")
        } else {
            write!(f, "<fn {}>", name)
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<Function>()
            + self.upvalues.capacity() * mem::size_of::<FunctionUpvalue>()
            + self.chunk.code.capacity() * mem::size_of::<OpCode>()
            + self.chunk.constants.capacity() * mem::size_of::<Value>()
            + self.chunk.line_runs.capacity() * mem::size_of::<(u32, u32)>()
    }

    fn trace(&self, gc: &mut crate::Gc) {
        gc.mark_object(self.name);
        self.chunk.constants.iter().for_each(|&constant| {
            gc.mark_value(constant);
        });
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

#[derive(Debug)]
pub struct Closure {
    pub function: GcRef<Function>,
    pub upvalues: Vec<GcRef<Upvalue>>,
}

impl Closure {
    pub fn new(function: GcRef<Function>) -> Self {
        Self {
            function,
            upvalues: vec![],
        }
    }
}

impl GcTrace for Closure {
    fn format(&self, f: &mut std::fmt::Formatter, gc: &crate::Gc) -> std::fmt::Result {
        let function = gc.deref(self.function);
        function.format(f, gc)
    }

    fn size(&self) -> usize {
        mem::size_of::<Closure>() + self.upvalues.capacity() * mem::size_of::<GcRef<Upvalue>>()
    }

    fn trace(&self, gc: &mut crate::Gc) {
        gc.mark_object(self.function);
        self.upvalues.iter().for_each(|&upvalue| {
            gc.mark_object(upvalue);
        });
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

#[derive(Debug)]
pub struct Upvalue {
    pub location: usize,
    pub closed: Option<Value>,
}

impl Upvalue {
    pub fn new(location: usize) -> Self {
        Upvalue {
            location,
            closed: None,
        }
    }
}

impl GcTrace for Upvalue {
    fn format(&self, f: &mut std::fmt::Formatter, _gc: &crate::Gc) -> std::fmt::Result {
        write!(f, "upvalue")
    }

    fn size(&self) -> usize {
        mem::size_of::<Upvalue>()
    }

    fn trace(&self, gc: &mut crate::Gc) {
        if let Some(obj) = self.closed {
            gc.mark_value(obj);
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}
