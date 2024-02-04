use std::{ffi::CStr, os::raw::c_char};

pub fn hello(input: &str) {
    println!("Hello, {}!", input);
}

#[no_mangle]
pub unsafe extern "C" fn c_hello(input: *const c_char) {
    hello(CStr::from_ptr(input).to_str().expect("invalid UTF-8 data"));
}
