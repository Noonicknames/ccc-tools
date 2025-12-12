/// Benchmark the amount of time to run a block of code.
#[macro_export]
macro_rules! bench_time {
    ($name: expr, $block: block) => {{
        let now = std::time::Instant::now();
        let result = $block;

        println!(
            "Time to run '{}': {}ms",
            $name,
            now.elapsed().as_secs_f32() * 1000.0
        );
        result
    }};
}
