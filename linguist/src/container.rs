use std::{
    collections::HashMap,
    ffi::{OsStr, OsString},
    path::Path,
};

use crate::resolver::{HeuristicRule, Language};

/// A `Container` can be used to implement a storage that holds [`Language`] and [`HeuristicRule`] definitions.
///
/// ## Features
/// When the `matcher` feature is enabled, the `Container` trait will also expose methods to retrieve [`HeuristicRule`] definitions.
pub trait Container {
    /// Returns a list of all [`Language`] definitions identified by its name.
    fn get_language_by_name(&self, name: &str) -> Option<&Language>;
    /// Returns a list of all [`Language`] definitions identified by the extension string provided.
    fn get_languages_by_extension_str(&self, ext: &OsStr) -> Option<Vec<&Language>>;
    /// Returns a list of all [`Language`] definitions identified by the extension of the given file.
    fn get_languages_by_extension(&self, file: impl AsRef<Path>) -> Option<Vec<&Language>>;
    /// Returns a list of all [`Language`] definitions identified by the name of the given file.
    fn get_languages_by_filename_str(&self, filename: &OsStr) -> Option<Vec<&Language>>;
    /// Returns a list of all [`Language`] definitions identified by the name of the given file.
    fn get_languages_by_filename(&self, file: impl AsRef<Path>) -> Option<Vec<&Language>>;
    /// Returns a list of all [`Language`] definitions identified by its interpreter.
    fn get_languages_by_interpreter(&self, interpreter: &str) -> Option<Vec<&Language>>;
    /// Returns a list of all [`HeuristicRule`] definitions identified by the extension of the given file.
    #[cfg(feature = "matcher")]
    fn get_heuristics_by_extension(&self, file: impl AsRef<Path>) -> Option<&Vec<HeuristicRule>>;
    #[cfg(feature = "matcher")]
    fn get_heuristics_by_extension_str(&self, extension: &OsStr) -> Option<&Vec<HeuristicRule>>;
}

#[derive(Debug, Default)]
pub struct InMemoryLanguageContainer {
    languages: Vec<Language>,
    heuristics: HashMap<OsString, Vec<HeuristicRule>>,
}

impl InMemoryLanguageContainer {
    pub fn register_language(&mut self, lang: impl Into<Language>) {
        self.languages.push(lang.into());
    }

    #[cfg(feature = "matcher")]
    pub fn register_heuristic_rule(&mut self, rule: impl Into<HeuristicRule>) {
        let rule = rule.into();

        for ext in &rule.extensions {
            if let Some(heuristic) = self.heuristics.get_mut(ext) {
                if !heuristic.contains(&rule) {
                    heuristic.push(rule.clone());
                }
            } else {
                self.heuristics
                    .insert(ext.to_os_string(), vec![rule.clone()]);
            }
        }
    }
}

impl Container for InMemoryLanguageContainer {
    fn get_language_by_name(&self, name: &str) -> Option<&Language> {
        self.languages
            .iter()
            .find(|lang| lang.name.to_lowercase() == *name.to_lowercase())
    }

    fn get_languages_by_extension(&self, file: impl AsRef<Path>) -> Option<Vec<&Language>> {
        let ext = match file.as_ref().extension() {
            Some(ext) => ext,
            _ => match file.as_ref().file_name() {
                Some(name) => name,
                _ => return None,
            },
        };

        self.get_languages_by_extension_str(ext)
    }
    fn get_languages_by_extension_str(&self, ext: &OsStr) -> Option<Vec<&Language>> {
        let candidates: Vec<&Language> = self
            .languages
            .iter()
            .filter(|lang| lang.extensions.contains(&OsString::from(ext)))
            .collect();

        if !candidates.is_empty() {
            Some(candidates)
        } else {
            None
        }
    }
    fn get_languages_by_filename(&self, file: impl AsRef<Path>) -> Option<Vec<&Language>> {
        if let Some(file_name) = file.as_ref().file_name() {
            self.get_languages_by_extension_str(file_name)
        } else {
            None
        }
    }
    fn get_languages_by_filename_str(&self, filename: &OsStr) -> Option<Vec<&Language>> {
        let candidates: Vec<&Language> = self
            .languages
            .iter()
            .filter(|lang| lang.filenames.contains(&filename.to_os_string()))
            .collect();

        if !candidates.is_empty() {
            Some(candidates)
        } else {
            None
        }
    }

    #[cfg(feature = "matcher")]
    fn get_heuristics_by_extension(&self, file: impl AsRef<Path>) -> Option<&Vec<HeuristicRule>> {
        let ext = match file.as_ref().extension() {
            Some(val) => val,
            _ => return None,
        };

        self.get_heuristics_by_extension(ext)
    }
    #[cfg(feature = "matcher")]
    fn get_heuristics_by_extension_str(&self, ext: &OsStr) -> Option<&Vec<HeuristicRule>> {
        let heuristics = self.heuristics.get(&ext.to_os_string());
        heuristics
    }

    fn get_languages_by_interpreter(&self, interpreter: &str) -> Option<Vec<&Language>> {
        let interpreters: Vec<&Language> = self
            .languages
            .iter()
            .filter(|lang| lang.interpreters.contains(&interpreter.to_string()))
            .collect();

        if !interpreters.is_empty() {
            Some(interpreters)
        } else {
            None
        }
    }
}
