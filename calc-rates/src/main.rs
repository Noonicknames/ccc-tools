use std::process::ExitCode;
fn main() -> ExitCode {
    match calc_rates::run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(_err) => ExitCode::FAILURE,
    }
}