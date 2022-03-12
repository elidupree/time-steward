use std::cell::RefCell;
use std::os::raw::{c_int, c_void};
use std::ptr::null_mut;
use std::thread::sleep;
use std::time::Duration;

pub fn main_loop<Frame: FnMut() -> bool>(mut frame: Frame) {
  if cfg!(target_os = "emscripten") {
    set_main_loop_callback(|| {
      frame();
    });
  } else {
    let mut closed = false;
    while !closed {
      closed = frame();
      sleep(Duration::from_millis(10));
    }
  }
}

thread_local!(static CLICKS: RefCell<Vec<(f64, f64)>> = RefCell::new(Vec::new()));

#[no_mangle]
pub fn canvas_click(x: f64, y: f64) {
  CLICKS.with(|vector| vector.borrow_mut().push((x, y)));
}
pub fn pop_click() -> Option<(f64, f64)> {
  CLICKS.with(|vector| vector.borrow_mut().pop())
}

#[allow(non_camel_case_types)]
type em_callback_func = unsafe extern "C" fn();
extern "C" {
  fn emscripten_set_main_loop(func: em_callback_func, fps: c_int, simulate_infinite_loop: c_int);
}

thread_local!(static MAIN_LOOP_CALLBACK: RefCell<*mut c_void> = RefCell::new(null_mut()));

pub fn set_main_loop_callback<F>(callback: F)
where
  F: FnMut(),
{
  MAIN_LOOP_CALLBACK.with(|log| {
    *log.borrow_mut() = &callback as *const _ as *mut c_void;
  });

  unsafe {
    emscripten_set_main_loop(wrapper::<F>, 0, 1);
  }

  unsafe extern "C" fn wrapper<F>()
  where
    F: FnMut(),
  {
    MAIN_LOOP_CALLBACK.with(|z| {
      let closure = *z.borrow_mut() as *mut F;
      (*closure)();
    });
  }
}
