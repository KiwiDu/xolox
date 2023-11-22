use std::{
    borrow::Borrow,
    hash::{Hash, Hasher},
    marker::PhantomData,
    rc::Rc,
};

#[derive(Debug, Eq)]
pub struct RcStr<H: Hasher> {
    s: Rc<str>,
    hash: u64,
    _h: PhantomData<H>,
}

impl<H: Hasher> Borrow<str> for RcStr<H> {
    fn borrow(&self) -> &str {
        &self.s
    }
}

impl<H: Hasher> PartialEq for RcStr<H> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.s, &other.s) || self.s == other.s
    }
}

impl<H: Hasher> PartialEq<str> for RcStr<H> {
    fn eq(&self, other: &str) -> bool {
        self.s.as_ref() == other
    }
}

impl<H: Hasher> PartialEq<RcStr<H>> for str {
    fn eq(&self, other: &RcStr<H>) -> bool {
        other == self
    }
}

impl<H: Hasher + Default> RcStr<H> {
    fn new(s: &str) -> Self {
        let mut state = H::default();
        s.hash(&mut state);

        Self {
            s: s.into(),
            hash: state.finish(),
            _h: PhantomData::default(),
        }
    }
}
impl<H: Hasher + Default> From<&str> for RcStr<H> {
    fn from(value: &str) -> Self {
        Self::new(value)
    }
}

impl<H: Hasher + Default> From<String> for RcStr<H> {
    fn from(value: String) -> Self {
        Self::new(&value)
    }
}

impl<H: Hasher + Default> Clone for RcStr<H> {
    fn clone(&self) -> Self {
        Self {
            s: Rc::clone(&self.s),
            hash: self.hash,
            _h: PhantomData::default(),
        }
    }
}

impl<H: Hasher> PartialOrd for RcStr<H> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.s.partial_cmp(&other.s)
    }
}

impl<H: Hasher> Hash for RcStr<H> {
    fn hash<H_: Hasher>(&self, state: &mut H_) {
        self.hash.hash(state);
    }
}
