# Logging System

The logging system is used for giving helpful errors or general information about what the program is doing.
This may become verbose, however it is recommended that at least error logs are enabled.

## Configuring Log Level
The log level can be configured via the `RUST_LOG` environment variable which may take on either `info`, `warn`, `error`, `none` in order of decreasing verbosity.
