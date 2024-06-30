use std::{
    ffi::{CStr, CString},
    os::raw::c_char,
    sync::Mutex,
};
use tao::{
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop, EventLoopBuilder, EventLoopProxy},
    window::WindowBuilder,
};
use wry::WebViewBuilder;

static SENDER: Mutex<Option<EventLoopProxy<String>>> = Mutex::new(None);

fn main(callback: extern "C" fn(*const c_char)) -> wry::Result<()> {
    let event_loop: EventLoop<String> = EventLoopBuilder::with_user_event().build();
    let window = WindowBuilder::new().build(&event_loop).unwrap();

    #[cfg(any(
        target_os = "windows",
        target_os = "macos",
        target_os = "ios",
        target_os = "android"
    ))]
    let builder = WebViewBuilder::new(&window);

    #[cfg(not(any(
        target_os = "windows",
        target_os = "macos",
        target_os = "ios",
        target_os = "android"
    )))]
    let builder = {
        use tao::platform::unix::WindowExtUnix;
        use wry::WebViewBuilderExtUnix;
        let vbox = window.default_vbox().unwrap();
        WebViewBuilder::new_gtk(vbox)
    };

    let webview = builder
        .with_html(include_str!("../index.html"))?
        .with_ipc_handler(move |event| {
            let s = CString::new(event).unwrap();
            callback(s.as_ptr());
        })
        .build()?;
    *SENDER.lock().unwrap() = Some(event_loop.create_proxy());

    event_loop.run(move |event, _, control_flow| {
        *control_flow = ControlFlow::Wait;

        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => *control_flow = ControlFlow::Exit,
            Event::UserEvent(ref js) => {
                webview.evaluate_script(js).unwrap();
            }
            _ => (),
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn c_start(callback: extern "C" fn(*const c_char)) {
    main(callback).unwrap();
}

#[no_mangle]
pub unsafe extern "C" fn c_eval(input: *const c_char) {
    let s = CStr::from_ptr(input).to_str().expect("invalid UTF-8 data");

    loop {
        let mut tx_cell = SENDER.lock().unwrap();
        if let Some(tx) = tx_cell.as_mut() {
            tx.send_event(s.to_owned()).unwrap();
            break;
        }
    }
}
