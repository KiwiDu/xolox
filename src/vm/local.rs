use std::borrow::Borrow;

#[derive(Debug)]
pub struct Local<T: Borrow<str> + Clone> {
    pub name: T,
    pub depth: u32,
    pub captured: bool,
}

impl<T: Borrow<str> + Clone> Local<T> {
    pub fn new(name: T, depth: u32) -> Self {
        Self {
            name,
            depth,
            captured: false,
        }
    }

    pub fn capture(&mut self) {
        self.captured = true;
    }
}

impl<T: Borrow<str> + Clone> Clone for Local<T> {
    fn clone(&self) -> Self {
        Self {
            name: T::clone(&self.name),
            depth: self.depth,
            captured: self.captured,
        }
    }
}
