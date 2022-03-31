use std::{fmt, rc::Rc, sync::Arc};

/// The source file and position of a given token
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Source {
    pub name: Rc<String>,
    pub line: u32,
    pub column: u32,
    pub parent: Option<Rc<Source>>,
}
impl Source {
    pub fn new(name: Rc<String>, line: u32, column: u32) -> Self {
        Self {
            name,
            line,
            column,
            parent: None,
        }
    }

    /// Returns a new `Source` with a new `Rc<String>` name
    pub fn new_unique(name: &str, line: u32, column: u32) -> Self {
        Self::new(Rc::new(name.into()), line, column)
    }

    pub fn with_tree_parent(&self, tree_parent: &Rc<Source>) -> Self {
        let mut nself = self.clone();
        if let Some(parent) = &self.parent {
            nself.parent = Some(Rc::new(parent.with_tree_parent(tree_parent)));
        } else {
            nself.parent = Some(tree_parent.clone());
        }
        nself
    }

    /// Modifies self to have the new parent
    pub fn with_parent(mut self, parent: Option<Rc<Source>>) -> Self {
        self.parent = parent;
        self
    }

    /// Modifies self with the new line and column
    pub fn with_position(mut self, line: u32, column: u32) -> Self {
        self.line = line;
        self.column = column;
        self
    }

    /// Modifies self by changing the line and column by the given amounts
    pub fn offset_position(mut self, line: i32, column: i32) -> Self {
        self.line = (self.line as i32 + line).max(0) as u32;
        self.column = (self.column as i32 + column).max(0) as u32;
        self
    }

    pub fn iter_source(&self) -> SourceIter {
        SourceIter { source: Some(self) }
    }
}
impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl From<ArcSource> for Source {
    fn from(source: ArcSource) -> Self {
        Self {
            name: Rc::new(source.name.as_ref().clone()),
            line: source.line,
            column: source.column,
            parent: source.parent.map(|v| Rc::new(Self::from((*v).clone()))),
        }
    }
}

pub struct SourceIter<'a> {
    source: Option<&'a Source>,
}
impl<'a> Iterator for SourceIter<'a> {
    type Item = &'a Source;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.source;
        self.source = self
            .source
            .and_then(|s1| s1.parent.as_ref().map(|s2| s2.as_ref()));
        result
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArcSource {
    pub name: Arc<String>,
    pub line: u32,
    pub column: u32,
    pub parent: Option<Arc<ArcSource>>,
}
impl ArcSource {
    pub fn new(name: Arc<String>, line: u32, column: u32) -> Self {
        Self {
            name,
            line,
            column,
            parent: None,
        }
    }

    pub fn new_unique(name: &str, line: u32, column: u32) -> Self {
        Self::new(Arc::new(name.into()), line, column)
    }

    /// Recursively prints out this source and its parents
    pub fn display(&self, indent: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:indent$}in {}", "", self.name, indent = indent)?;
        // Online write "at line:col" if they're not (0, 0)
        if self.line != 0 || self.column != 0 {
            write!(f, " at {}:{}", self.line, self.column)?;
        }
        writeln!(f)?;
        if let Some(parent) = &self.parent {
            parent.display(indent + 2, f)?;
        }
        Ok(())
    }
}
impl From<Source> for ArcSource {
    fn from(source: Source) -> Self {
        Self {
            name: Arc::new(source.name.as_ref().clone()),
            line: source.line,
            column: source.column,
            parent: source.parent.map(|v| Arc::new(Self::from((*v).clone()))),
        }
    }
}
impl fmt::Display for ArcSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(0, f)
    }
}
#[cfg(feature = "serialize")]
impl serde::Serialize for ArcSource {
    fn serialize<S: serde::Serializer>(&self, se: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeStruct;
        let parent: Option<&ArcSource> = self.parent.as_ref().map(|v| v.as_ref());
        let mut m = se.serialize_struct("ArcSource", 4)?;
        m.serialize_field("name", self.name.as_str())?;
        m.serialize_field("line", &self.line)?;
        m.serialize_field("column", &self.column)?;
        m.serialize_field("parent", &parent)?;
        m.end()
    }
}
