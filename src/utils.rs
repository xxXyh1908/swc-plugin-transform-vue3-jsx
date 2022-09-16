use std::{env, path};

use globset::GlobBuilder;
use lazy_static::lazy_static;
use regex::{Captures, Regex, RegexBuilder};

//------------------------------------ matcher ----------------------------------------------
fn to_base_regex(pattern_str: &str) -> Option<Regex> {
    if pattern_str.starts_with('/') {
        let m = pattern_str[1..].rsplit_once('/');
        if let Some(tuple) = m {

            return Some(
                RegexBuilder::new(tuple.0)
                    .multi_line(tuple.1.contains('m'))
                    .dot_matches_new_line(tuple.1.contains('s'))
                    .case_insensitive(tuple.1.contains('i'))
                    .build()
                    .expect("Invalid Regex"),
            );
        }
    }

    return None;
}

pub(crate) fn to_path_regex(pattern_str: &str, root: &path::PathBuf) -> Option<Regex> {
    match to_base_regex(pattern_str) {
        Some(result) => Some(result),
        _ => {
            let glob_str = get_matcher_string(pattern_str, root);

            match glob_str {
                Some(glob_str) => Some(
                    Regex::new(
                        &GlobBuilder::new(&glob_str.replace('\\', "/"))
                            .backslash_escape(true)
                            .case_insensitive(true)
                            .build()
                            .expect("Invalid Glob")
                            .regex()
                            .replace("(?-u)", "(?u)"),
                    )
                    .expect("Invalid Regex"),
                ),
                _ => None,
            }
        }
    }
}

pub(crate) fn to_regex(pattern_str: &str) -> Option<Regex> {
    match to_base_regex(pattern_str) {
        Some(result) => Some(result),
        _ => {
            let escaped_str = format!("^{}$", escape_regex(pattern_str));
            Some(Regex::new(&escaped_str).unwrap())
        }
    }
}

fn escape_regex(source: &str) -> String {
    let mut str = String::new();
    for ch in source.chars() {
        match ch {
            '\\' | '.' | '+' | '*' | '?' | '(' | ')' | '|' | '[' | ']' | '{' | '}' | '^' | '$'
            | '#' | '&' | '~' => str.push('\\'),
            '-' => {
                str.push_str(r"\x2d");
                continue;
            }
            _ => {}
        }

        str.push(ch)
    }

    str
}

fn to_root(cwd: Option<&str>) -> path::PathBuf {
    return match cwd {
        None => env::current_dir().unwrap(),
        Some(cwd) => env::current_dir().unwrap().join(cwd),
    };
}

fn get_matcher_string(id: &str, root: &path::PathBuf) -> Option<String> {
    let path = path::PathBuf::from(id);

    if IS_MATCHER_REGEX.is_match(id) || path.is_absolute() {
        return match path.to_str() {
            None => None,
            Some(str) => Some(String::from(str)),
        };
    }

    return match root.clone().join(path).to_str() {
        None => None,
        Some(str) => Some(String::from(str)),
    };
}

pub trait StringFilter {
    fn do_filter(&self, _p: &str) -> bool {
        false
    }
}

pub(crate) struct CreateFileNameFilter {
    include: Vec<Regex>,
    exclude: Vec<Regex>,
}

impl StringFilter for CreateFileNameFilter {
    fn do_filter(&self, p: &str) -> bool {
        let path = &p.replace('\\', "/");

        for exc in self.exclude.iter() {
            if exc.is_match(path) {
                return false;
            }
        }

        for inc in self.include.iter() {
            if inc.is_match(path) {
                return true;
            }
        }

        false
    }
}

pub(crate) fn create_file_name_filter(
    include: Option<Vec<String>>,
    exclude: Option<Vec<String>>,
    cwd: Option<&str>,
) -> Box<dyn StringFilter> {
    let root = to_root(cwd);
    let mut include_vec: Vec<Regex> = Vec::new();
    let mut exclude_vec: Vec<Regex> = Vec::new();

    if let Some(include) = include {
        for inc in include {
            if let Some(regexp) = to_path_regex(&inc, &root) {
                include_vec.push(regexp);
            }
        }
    }

    if let Some(exclude) = exclude {
        for exc in exclude {
            if let Some(regexp) = to_path_regex(&exc, &root) {
                exclude_vec.push(regexp);
            }
        }
    }

    Box::new(CreateFileNameFilter {
        include: include_vec,
        exclude: exclude_vec,
    })
}

pub(crate) struct CreateFilter {
    include: Vec<Regex>,
}

impl StringFilter for CreateFilter {
    fn do_filter(&self, p: &str) -> bool {
        for inc in self.include.iter() {
            if inc.is_match(p) {
                return true;
            }
        }

        false
    }
}

pub(crate) fn create_filter(include: Option<Vec<String>>) -> Box<dyn StringFilter> {
    let mut include_vec: Vec<Regex> = Vec::new();

    if let Some(include) = include {
        for inc in include {
            if let Some(regexp) = to_regex(&inc) {
                include_vec.push(regexp);
            }
        }
    }

    Box::new(CreateFilter {
        include: include_vec,
    })
}

//------------------------------------ string ----------------------------------------------
lazy_static! {
    static ref CAMELIZE_REGEX: Regex = Regex::new(r"-(\w)").unwrap();
    static ref CAMELIZE_UPPER_FIRST_REGEX: Regex = Regex::new(r"(?:^|-)(\w)").unwrap();
    static ref UPPER_FIRST_REGEX: Regex = Regex::new(r"^([a-z])").unwrap();
    static ref LOWER_FIRST_REGEX: Regex = Regex::new(r"^([A-Z])").unwrap();
    static ref IS_MATCHER_REGEX: Regex = Regex::new(r"^[*{]|^[^\\/]*[*{][^\\/]*$").unwrap();
}

pub(crate) fn upper_first(str: &str) -> String {
    return UPPER_FIRST_REGEX
        .replace(str, |caps: &Captures| (&caps[1]).to_string().to_uppercase())
        .to_string();
}

pub(crate) fn lower_first(str: &str) -> String {
    return LOWER_FIRST_REGEX
        .replace(str, |caps: &Captures| (&caps[1]).to_string().to_lowercase())
        .to_string();
}

pub(crate) fn camelize(str: &str) -> String {
    return CAMELIZE_REGEX
        .replace_all(str, |caps: &Captures| (&caps[1]).to_string().to_uppercase())
        .to_string();
}

pub(crate) fn camelize_upper_first(str: &str) -> String {
    return CAMELIZE_UPPER_FIRST_REGEX
        .replace_all(str, |caps: &Captures| (&caps[1]).to_string().to_uppercase())
        .to_string();
}

//-------------------------------------------macros------------------------------------------
#[macro_export]
macro_rules! hashmap_get_mut_default {
    ($hashmap:expr, $key:expr, $default:expr) => {
        match $hashmap.get_mut($key) {
            None => {
                $hashmap.insert($key.clone(), $default);
                $hashmap.get_mut($key).unwrap()
            }
            Some(value) => value,
        }
    };
}

#[macro_export]
macro_rules! hashmap_str_key_get_mut_default {
    ($hashmap:expr, $key_str:expr, $default:expr) => {
        match $hashmap.get_mut($key_str) {
            None => {
                $hashmap.insert($key_str.to_string(), $default);
                $hashmap.get_mut($key_str).unwrap()
            }
            Some(value) => value,
        }
    };
}
