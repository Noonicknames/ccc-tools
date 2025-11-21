use std::process::ExitCode;
fn main() -> ExitCode {
    match prep_plot::run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(_err) => ExitCode::FAILURE,
    }
}
