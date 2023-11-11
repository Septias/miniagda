use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Ctx<T>(Vec<T>);

#[derive(Clone, Debug)]
pub struct Tel<T>(Vec<T>);

#[derive(Clone, Debug)]
pub struct Glo<T>(HashMap<String, T>);