use std::collections::HashMap;

#[derive(Clone)]
pub struct ActionDefinition {
    pub method: String,
    pub path: String,
}

struct EnvironmentFrame {
    actions: HashMap<String, ActionDefinition>,
}

pub struct StaticEnvironment {
    frames: Vec<EnvironmentFrame>,
}

impl StaticEnvironment {
    pub fn new() -> Self {
        Self {
            frames: vec![EnvironmentFrame::new()],
        }
    }

    pub fn register_action(&mut self, name: &str, method: &str, path: &str) {
        self.current_frame().actions
            .insert(name.to_string(), ActionDefinition::new(method, path));
    }

    pub fn lookup_action(&mut self, name: &str) -> Result<ActionDefinition, String> {
        self.search_frames(|frame| frame.actions.get(name).map(Clone::clone))
            .ok_or(format!("Action '{:?}' not found", name))
    }

    pub fn current_frame(&mut self) -> &mut EnvironmentFrame {
        self.frames.last_mut().unwrap()
    }

    pub fn push(&mut self) {
        self.frames.push(EnvironmentFrame::new());
    }

    pub fn pop(&mut self) {
        self.frames.pop();
        assert!(!self.frames.is_empty())
    }

    fn search_frames<F, T>(&self, f: F) -> Option<T>
        where F: Fn(&EnvironmentFrame) -> Option<T>
    {
        for frame in self.frames.iter().rev() {
            let value = f(frame);
            if value.is_some() {
                return value
            }
        }
        None
    }
}

impl EnvironmentFrame {
    fn new() -> Self {
        Self {
            actions: HashMap::new(),
        }
    }
}

impl ActionDefinition {
    fn new(method: &str, path: &str) -> Self {
        Self {
            method: method.to_string(),
            path: path.to_string(),
        }
    }
}

#[test]
fn test_get_action_not_found() {
    let mut env = StaticEnvironment::new();
    env.push();
    env.push();
    env.push();
    env.push();
    assert!(env.lookup_action("test").is_err());
}

#[test]
fn test_get_action_found_first_frame() {
    let mut env = StaticEnvironment::new();
    env.push();
    env.push();
    env.push();
    env.push();
    env.register_action("test", "GET", "/resource/:id");
    let action = env.lookup_action("test").unwrap();
    assert_eq!(action.method, "GET");
    assert_eq!(action.path, "/resource/:id");
}

#[test]
fn test_get_action_found_other_frame() {
    let mut env = StaticEnvironment::new();
    env.push();
    env.register_action("test", "GET", "/resource/:id");
    env.push();
    env.push();
    env.push();
    let action = env.lookup_action("test").unwrap();
    assert_eq!(action.method, "GET");
    assert_eq!(action.path, "/resource/:id");
}

#[test]
fn test_get_action_shadowing() {
    let mut env = StaticEnvironment::new();
    env.register_action("test", "GET", "/resource/:id");
    env.push();
    env.push();
    env.register_action("test", "POST", "/resource/:id/thing");
    env.push();
    env.register_action("test", "PATCH", "/:id");
    env.push();
    let action = env.lookup_action("test").unwrap();
    assert_eq!(action.method, "PATCH");
    assert_eq!(action.path, "/:id");
}
