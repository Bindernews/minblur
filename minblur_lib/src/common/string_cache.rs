use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{hash_map::DefaultHasher, HashMap},
    fmt,
    hash::{Hash, Hasher},
    ops::Deref,
    rc::Rc,
};

const DEFAULT_BUF_SIZE: usize = 4096;
type CacheDataRef = Rc<RefCell<StringCacheData>>;

#[derive(Clone)]
pub struct CaString {
    source: Rc<String>,
    /// Start index and length of sub-string
    span: (u32, u32),
}
impl CaString {
    pub fn as_str(&self) -> &str {
        &self.source[self.span.0 as usize..self.span.1 as usize]
    }
}
impl PartialEq<Self> for CaString {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}
impl PartialEq<&str> for CaString {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}
impl Eq for CaString {}
impl PartialOrd for CaString {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}
impl Ord for CaString {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}
impl Hash for CaString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}
impl fmt::Debug for CaString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("CaString").field(&self.as_str()).finish()
    }
}
impl fmt::Display for CaString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}
impl AsRef<str> for CaString {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}
impl Deref for CaString {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}
impl From<&str> for CaString {
    fn from(s: &str) -> Self {
        Self {
            source: Rc::new(String::from(s)),
            span: (0, s.len() as u32),
        }
    }
}
impl From<CaString> for String {
    fn from(v: CaString) -> Self {
        v.as_str().into()
    }
}

#[derive(Debug, Clone)]
pub struct StringCache {
    source: CacheDataRef,
}
impl StringCache {
    pub fn new() -> Self {
        Self {
            source: Rc::new(RefCell::new(StringCacheData::new())),
        }
    }

    pub fn get(&self, s: &str) -> CaString {
        let pos = (*self.source).borrow_mut().find_or_insert(s);
        CaString {
            source: (*self.source).borrow().bufs[pos.index].clone(),
            span: (pos.span.0 as u32, pos.span.1 as u32),
        }
    }
}
impl Default for StringCache {
    fn default() -> Self {
        Self::new()
    }
}
impl PartialEq for StringCache {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.source, &other.source)
    }
}
impl Eq for StringCache {}

#[derive(Debug, Clone)]
struct StringCacheData {
    bufs: Vec<Rc<String>>,
    /// Map string hashes to arrays of cache positions.
    /// This handles the case where multiple strings may have the same hash.
    lookup_map: HashMap<u64, Vec<CachePos>>,
}
impl StringCacheData {
    pub fn new() -> Self {
        Self {
            bufs: vec![Rc::new(String::with_capacity(DEFAULT_BUF_SIZE))],
            lookup_map: HashMap::new(),
        }
    }

    pub fn find_or_insert(&mut self, s: &str) -> CachePos {
        let hash = self.ensure_lookup(s);
        // Grab the list and try to find an existing position for this string
        let pos_list = self.lookup_map.get(&hash).unwrap();
        for pos in pos_list.iter() {
            if self.slice_from_pos(pos) == s {
                return pos.clone();
            }
        }
        // No existing position found, append a new one
        self.alloc_string(s)
    }

    /// Ensure that a lookup_map entry exists for this string, returns the string's hash.
    fn ensure_lookup(&mut self, s: &str) -> u64 {
        let hash = Self::hash_it(s);
        self.lookup_map.entry(hash).or_insert_with(Vec::new);
        hash
    }

    /// Ensure that the last buffer on the stack has at minimum `required`
    /// free space to store a new string in.
    fn ensure_buf_space(&mut self, required: usize) {
        let buf1 = self.bufs.last().unwrap();
        let req_size = required + 2;
        if buf1.len() + req_size >= buf1.capacity() {
            let default_size = DEFAULT_BUF_SIZE * (self.bufs.len() + 1);
            self.bufs
                .push(Rc::new(String::with_capacity(req_size.max(default_size))));
        }
    }

    fn alloc_string(&mut self, s: &str) -> CachePos {
        // Make sure there's enough space for the new string
        self.ensure_buf_space(s.len());
        // Add the new string
        let index = self.bufs.len() - 1;
        let buf1 = self.bufs.last_mut().unwrap();
        let old_len = buf1.len();
        // Since we're NEVER re-allocating the memory for an allocated
        // string, the memory never moves and it's safe to mess with it.
        unsafe {
            // TODO when this is stabilized use get_mut_unchecked
            //Rc::get_mut_unchecked(buf1).push_str(s);
            (Rc::as_ptr(buf1) as *mut String)
                .as_mut()
                .unwrap()
                .push_str(s);
        }
        let span = (old_len, buf1.len());
        let c_pos = CachePos { index, span };
        // Add the string to the lookup map
        let hash = self.ensure_lookup(s);
        self.lookup_map.get_mut(&hash).unwrap().push(c_pos.clone());
        c_pos
    }

    pub fn slice_from_pos(&self, pos: &CachePos) -> &str {
        &self.bufs[pos.index].as_str()[pos.span.0..pos.span.1]
    }

    pub fn hash_it<H: Hash>(h: H) -> u64 {
        let mut state = DefaultHasher::new();
        h.hash(&mut state);
        state.finish()
    }
}

/// "Position" of a cached string within a given StringCacheData
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct CachePos {
    /// bufs index
    index: usize,
    /// span within the buf containing this string
    span: (usize, usize),
}

#[cfg(test)]
pub mod test {
    use super::*;

    #[test]
    fn test_ca_string_equal() {
        let string_cache = StringCache::new();
        let a = CaString::from("abc");
        let b = string_cache.get("abc");
        assert_eq!(a, b);
    }
}
