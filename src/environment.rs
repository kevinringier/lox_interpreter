#[derive(Clone, Debug)]
pub struct Environment<T> {
    enclosing: Option<Box<Environment<T>>>,
    values: std::collections::HashMap<String, T>,
}

pub struct EnvironmentError;

impl<T: Clone> Environment<T> {
    pub fn new() -> Self {
        Self {
            enclosing: None,
            values: std::collections::HashMap::<String, T>::new(),
        }
    }

    pub fn set_enclosing_env(&mut self, enclosing: Environment<T>) {
        self.enclosing = Some(Box::new(enclosing));
    }

    pub fn get_enclosing_env(&self) -> Environment<T> {
        match self.enclosing.as_ref() {
            Some(env) => env.as_ref().clone(),
            None => panic!("Should not call when enclosing is not set."),
        }
    }

    pub fn assign(&mut self, key: String, value: T) -> Result<(), EnvironmentError> {
        if self.values.contains_key(&key) {
            self.values.insert(key, value);
            Ok(())
        } else if let Some(env) = self.enclosing.as_mut() {
            env.assign(key, value)
        } else {
            Err(EnvironmentError)
        }
    }

    pub fn define(&mut self, key: String, value: T) {
        self.values.insert(key, value);
    }

    pub fn get(&self, key: &String) -> Option<T> {
        match self.values.get(key) {
            Some(v) => Some(v.to_owned()),
            None => self.enclosing.as_ref().map(|env| env.get(key)).flatten(),
        }
    }
}
