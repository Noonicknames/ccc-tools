use std::process::ExitCode;
fn main() -> ExitCode {
    match cs_extract::run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(_err) => ExitCode::FAILURE,
    }
}
