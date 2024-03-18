#![allow(dead_code)]

mod utils;
mod game;

use wasm_bindgen::prelude::*;
use crate::game::play;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet(msg: &str) {
    play();
    alert(&format!("Hello, {msg}!"));
}
