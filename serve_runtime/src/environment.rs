use std::marker::PhantomData;
use std::ops::Deref;
use std::rc::Rc;
use super::types::*;
use typemap::{Key, TypeMap};

struct TypeEnvironment<T> {
    arena: Vec<Rc<T>>
}

impl<T> TypeEnvironment<T> {
    pub fn new() -> Self {
        Self {
            arena: Vec::new(),
        }
    }
}

struct EnvironmentKey<T> {
    phantom: PhantomData<T>,
}

impl<T: 'static> Key for EnvironmentKey<T> {
    type Value = TypeEnvironment<T>;
}

pub struct Environment {
    types: TypeMap,
}

pub struct EnvironmentReference<T> {
    index: usize,
    phantom: PhantomData<T>,
}

impl<T: 'static> EnvironmentReference<T> {
    fn new(index: usize) -> Self {
        Self {
            index: index,
            phantom: PhantomData,
        }
    }

    pub fn lookup(&self, env: &mut Environment) -> Rc<T>
    {
        env.types.get::<EnvironmentKey<T>>().unwrap().arena.get(self.index).unwrap().clone()
    }
}

impl Environment {

    pub fn new() -> Self {
        Self {
            types: TypeMap::new(),
        }
    }

    pub fn allocate<T, R>(&mut self, value: R) -> EnvironmentReference<T>
        where T: 'static + ServeType + Constructible,
              R: Into<T::Inner>
    {
        let type_env = self.types.entry::<EnvironmentKey<T>>()
            .or_insert_with(|| TypeEnvironment::new());
        let env_ref = EnvironmentReference::new(type_env.arena.len());
        type_env.arena.push(Rc::new(T::new(value)));
        env_ref
    }

}

#[test]
fn test_allocate() {
    let mut env = Environment::new();
    let sref = env.allocate::<ServeString>("test".to_string());
    assert_eq!(sref.lookup(&mut env).value(), "test");
}
