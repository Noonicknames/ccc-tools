use std::process::ExitCode;
fn main() -> ExitCode {
    match extract_cs::run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(_err) => ExitCode::FAILURE,
    }
}
