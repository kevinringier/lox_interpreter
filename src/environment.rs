use std::{cell::RefCell, rc::Rc};

#[derive(Clone, Debug)]
pub struct Environment<T> {
    enclosing: Option<Rc<RefCell<Environment<T>>>>,
    values: std::collections::HashMap<String, T>,
}

#[derive(Debug)]
pub struct EnvironmentError;

impl<T: Clone> Environment<T> {
    pub fn new() -> Self {
        Self {
            enclosing: None,
            values: std::collections::HashMap::<String, T>::new(),
        }
    }

    pub fn set_enclosing_env(&mut self, enclosing: Rc<RefCell<Environment<T>>>) {
        self.enclosing = Some(enclosing);
    }

    pub fn assign_at(&mut self, distance: usize, key: &String, value: T) {
        if distance == 0 {
            self.values.insert(key.clone(), value);
        } else {
            let env = self.ancestor(distance);
            env.borrow_mut().assign_at(0, key, value);
        }
    }

    pub fn assign(&mut self, key: String, value: T) -> Result<(), EnvironmentError> {
        if self.values.contains_key(&key) {
            self.values.insert(key, value);
            Ok(())
        } else if let Some(env) = self.enclosing.as_mut() {
            env.as_ref().borrow_mut().assign(key, value)
        } else {
            Err(EnvironmentError)
        }
    }

    pub fn define(&mut self, key: String, value: T) {
        self.values.insert(key, value);
    }

    fn ancestor(&self, distance: usize) -> Rc<RefCell<Environment<T>>> {
        match &self.enclosing {
            Some(enclosing) => {
                let enclosing_clone = enclosing.clone();

                if distance == 1 {
                    enclosing_clone
                } else {
                    enclosing_clone.borrow().ancestor(distance - 1)
                }
            }
            _ => panic!("we assumed enclosing should exist based on resolver pass"),
        }
    }

    pub fn get_at(&self, distance: usize, name: &String) -> T {
        if distance == 0 {
            self.values
                .get(name)
                .expect("get_at assumes values are available after resolver pass")
                .clone()
        } else {
            let env = self.ancestor(distance);
            env.borrow().get_at(0, name)
        }
    }

    pub fn get(&self, key: &String) -> Option<T> {
        match self.values.get(key) {
            Some(v) => Some(v.to_owned()),
            None => self
                .enclosing
                .as_ref()
                .map(|env| env.borrow().get(key))
                .flatten(),
        }
    }
}
