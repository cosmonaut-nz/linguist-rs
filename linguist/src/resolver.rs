use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::fmt::Display;
use std::io::{BufRead, BufReader};
use std::path::Path;

#[cfg(feature = "matcher")]
use regex::Regex;

use crate::container::Container;
use crate::error::LinguistError;
use crate::utils::{determine_multiline_exec_from_str, has_shebang, is_binary};

/// A `Language` exposes the properties of a language definition.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Language {
    pub parent: Option<String>,
    pub name: String,
    pub aliases: Vec<String>,
    pub scope: Scope,
    pub extensions: Vec<OsString>,
    pub filenames: Vec<OsString>,
    pub interpreters: Vec<String>,
    pub color: Option<String>,
}

impl Display for Language {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// A `Scope` represents the type of a [`Language`].
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Scope {
    Programming,
    Markup,
    Data,
    Prose,
    Unknown,
}

impl From<String> for Scope {
    fn from(value: String) -> Self {
        match value.to_lowercase().as_str() {
            "programming" => Scope::Programming,
            "markup" => Scope::Markup,
            "data" => Scope::Data,
            "prose" => Scope::Prose,
            _ => Scope::Unknown,
        }
    }
}

impl From<&str> for Scope {
    fn from(value: &str) -> Self {
        match value.to_lowercase().as_str() {
            "programming" => Scope::Programming,
            "markup" => Scope::Markup,
            "data" => Scope::Data,
            "prose" => Scope::Prose,
            _ => Scope::Unknown,
        }
    }
}

impl std::fmt::Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Scope::Programming => write!(f, "Programming"),
            Scope::Markup => write!(f, "Markup"),
            Scope::Data => write!(f, "Data"),
            Scope::Prose => write!(f, "Prose"),
            Scope::Unknown => write!(f, "Unknown"),
        }
    }
}

/// A `HeuristicRule` represents a check for a [`Language`] based on the content of a file.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "matcher", derive(serde::Serialize, serde::Deserialize))]
pub struct HeuristicRule {
    /// The reference to the [`Language`] that is matched by this rule.
    pub language: String,
    /// A list of extensions that are used to check whether this rule applies.
    pub extensions: Vec<OsString>,
    /// A list of patterns that are used to check whether this rule applies.
    pub patterns: Vec<String>,
}

/// Used to resolve all possible [`Language`]s by the given file.
pub fn resolve_languages_by_filename(
    file: impl AsRef<Path>,
    container: &impl Container,
) -> Result<Vec<&Language>, LinguistError> {
    match container.get_languages_by_filename(file) {
        Some(langs) => Ok(langs),
        _ => Err(LinguistError::LanguageNotFound),
    }
}
/// Used to resolve all possible [`Language`]s by the given filename.
pub fn resolve_languages_by_filename_str(
    filename: impl AsRef<OsString>,
    container: &impl Container,
) -> Result<Vec<&Language>, LinguistError> {
    match container.get_languages_by_filename_str(filename.as_ref()) {
        Some(langs) => Ok(langs),
        _ => Err(LinguistError::LanguageNotFound),
    }
}

/// Used to resolve all possible [`Language`]s by the given extension.
pub fn resolve_languages_by_extension(
    file: impl AsRef<Path>,
    container: &impl Container,
) -> Result<Vec<&Language>, LinguistError> {
    match container.get_languages_by_extension(file) {
        Some(langs) => Ok(langs),
        _ => Err(LinguistError::LanguageNotFound),
    }
}
/// Used to resolve all possible [`Language`]s by the given extension str.
pub fn resolve_languages_by_extension_str(
    ext: impl AsRef<OsString>,
    container: &impl Container,
) -> Result<Vec<&Language>, LinguistError> {
    match container.get_languages_by_extension_str(ext.as_ref()) {
        Some(langs) => Ok(langs),
        _ => Err(LinguistError::LanguageNotFound),
    }
}

/// Used to resolve all possible [`Language`]s by the file contents.
#[cfg(feature = "matcher")]
pub fn resolve_language_by_content(
    file: impl AsRef<Path>,
    container: &impl Container,
) -> Result<Option<&Language>, LinguistError> {
    let ext = match file.as_ref().extension() {
        Some(ext) => ext,
        _ => match file.as_ref().file_name() {
            Some(name) => name,
            _ => return Ok(None),
        },
    };
    let content = match std::fs::read_to_string(file.as_ref()) {
        Ok(content) => content,
        _ => return Err(LinguistError::FileNotFound),
    };

    resolve_language_by_content_str(content, ext, container)
}
/// Used to resolve all possible [`Language`]s by the file contents.
#[cfg(feature = "matcher")]
pub fn resolve_language_by_content_str(
    content: impl AsRef<OsStr>,
    ext: impl AsRef<OsStr>,
    container: &impl Container,
) -> Result<Option<&Language>, LinguistError> {
    let content_str = content
        .as_ref()
        .to_str()
        .ok_or(LinguistError::InvalidData)?;
    if let Some(rules) = container.get_heuristics_by_extension_str(ext.as_ref()) {
        for rule in rules {
            let matcher = Regex::new(&rule.patterns.join("|"))?;

            for line in content_str.lines() {
                if matcher.is_match(line) {
                    return Ok(container.get_language_by_name(&rule.language));
                }
            }
        }
    }

    Err(LinguistError::LanguageNotFound)
}

/// Used to resolve all possible [`Language`]s by the file contents.
pub fn resolve_languages_by_shebang(
    file: impl AsRef<Path>,
    container: &impl Container,
) -> Result<Option<Vec<&Language>>, LinguistError> {
    // load first line of file
    let file = match std::fs::File::open(&file) {
        Ok(file) => file,
        Err(err) => return Err(LinguistError::IOError(err)),
    };

    let mut buf = BufReader::new(file);
    let mut file_content = String::new();

    // Read the first few lines of the file
    for _ in 0..5 {
        let mut line = String::new();
        let bytes_read = buf.read_line(&mut line).map_err(LinguistError::IOError)?;
        if bytes_read == 0 {
            break;
        }
        file_content.push_str(&line);
    }

    // Call the main logic function with the first line
    resolve_languages_by_shebang_line_str(&file_content, container)
}
/// Resolves the language according to the first line of a file contents
fn resolve_languages_by_shebang_line_str(
    file_contents: impl AsRef<OsStr>,
    container: &impl Container,
) -> Result<Option<Vec<&Language>>, LinguistError> {
    let line = file_contents
        .as_ref()
        .to_str()
        .ok_or(LinguistError::InvalidData)?;

    // check whether the first line of the file is a shebang
    if !has_shebang(line.as_bytes()) {
        return Ok(None);
    }

    let line = line[2..].trim();
    let mut fields = line.split_whitespace().collect::<Vec<&str>>();
    if fields.is_empty() {
        return Ok(None);
    }

    let mut interpreter = Path::new(fields[0])
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .to_owned();

    if interpreter == "env" {
        if fields.len() == 1 {
            return Ok(None);
        }

        let env_opt_args = Regex::new(r"^-[a-zA-Z]+$").unwrap();
        let env_var_args = Regex::new(r"^\$[a-zA-Z_]+$").unwrap();

        let _i = 1;
        while fields.len() > 2 {
            if env_opt_args.is_match(fields[1]) || env_var_args.is_match(fields[1]) {
                fields.remove(1);
                continue;
            }
            break;
        }
        interpreter = Path::new(fields[1])
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .to_owned();
    }

    if interpreter == "sh" {
        interpreter = determine_multiline_exec_from_str(file_contents.as_ref()).unwrap();
    }

    let python_version = Regex::new(r"^python[0-9]*\.[0-9]*").unwrap();
    if python_version.is_match(&interpreter) {
        interpreter = interpreter.split('.').next().unwrap().to_owned();
    }
    // If osascript is called with argument -l it could be different language so do not rely on it
    // To match linguist behavior, see ref https://github.com/github/linguist/blob/d95bae794576ab0ef2fcb41a39eb61ea5302c5b5/lib/linguist/shebang.rb#L63
    if interpreter == "osascript" && line.contains("-l") {
        interpreter = "".to_string();
    }

    let results = container.get_languages_by_interpreter(&interpreter);
    if results.is_some() {
        Ok(results)
    } else {
        Ok(None)
    }
}

/// Resolve the [`Language`] of the given file. It will try to resolve the language by the filename,
/// extension, shebang and content. The most likely language will be returned.
pub fn resolve_language(
    file: impl AsRef<Path>,
    container: &impl Container,
) -> Result<Option<&Language>, LinguistError> {
    if is_binary(&file)? {
        return Ok(None);
    }

    let mut probabilities: HashMap<String, usize> = HashMap::new();

    if let Ok(candidates) = resolve_languages_by_filename(&file, container) {
        for candidate in candidates {
            *probabilities
                .entry(candidate.name.clone().to_lowercase())
                .or_insert(1) += 1;
        }
    }

    if let Ok(Some(candidate)) = resolve_languages_by_shebang(&file, container) {
        for lang in candidate {
            *probabilities
                .entry(lang.name.clone().to_lowercase())
                .or_insert(1) += 10;
        }
    }

    if let Ok(candidates) = resolve_languages_by_extension(&file, container) {
        for candidate in candidates {
            *probabilities
                .entry(candidate.name.clone().to_lowercase())
                .or_insert(1) += 20;
        }
    }

    if let Ok(Some(candidate)) = resolve_language_by_content(&file, container) {
        *probabilities
            .entry(candidate.name.clone().to_lowercase())
            .or_insert(1) += 50;
    }

    let mut ordered: Vec<(&String, &usize)> = probabilities.iter().collect();
    ordered.sort_by(|a, b| {
        match b.1.cmp(a.1) {
            std::cmp::Ordering::Equal => a.0.cmp(b.0), // If weights are equal, then sort by name
            other => other,                            // Otherwise, sort by weight
        }
    });

    if !ordered.is_empty() {
        return Ok(Some(
            container
                .get_language_by_name(ordered.get(0).unwrap().0)
                .unwrap(),
        ));
    }
    Err(LinguistError::LanguageNotFound)
}
