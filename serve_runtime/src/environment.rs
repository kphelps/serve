use std::marker::PhantomData;
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

pub struct Env {
    types: TypeMap,
}

#[derive(Debug)]
pub struct EnvRef<T> {
    index: usize,
    phantom: PhantomData<T>,
}

impl<T: 'static> EnvRef<T> {
    fn new(index: usize) -> Self {
        Self {
            index: index,
            phantom: PhantomData,
        }
    }

    pub fn lookup(&self, env: &mut Env) -> Rc<T>
    {
        env.types.get::<EnvironmentKey<T>>().unwrap().arena.get(self.index).unwrap().clone()
    }
}

impl Env {

    pub fn new() -> Self {
        Self {
            types: TypeMap::new(),
        }
    }

    pub fn allocate<T, R>(&mut self, value: R) -> EnvRef<T>
        where T: 'static + ServeType + Constructible,
              R: Into<T::Inner>
    {
        let type_env = self.types.entry::<EnvironmentKey<T>>()
            .or_insert_with(|| TypeEnvironment::new());
        let env_ref = EnvRef::new(type_env.arena.len());
        type_env.arena.push(Rc::new(T::new(value)));
        env_ref
    }

}

#[test]
fn test_allocate() {
    let mut env = Env::new();
    let sref = env.allocate::<ServeString>("test".to_string());
    assert_eq!(sref.lookup(&mut env).value(), "test");
}
