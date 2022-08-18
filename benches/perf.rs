use std::{fs::File, os::raw::c_int};

use criterion::profiler::Profiler;
use pprof::ProfilerGuard;

pub struct FlamegraphFrofiler<'a> {
    frequency: c_int,
    active_profiler: Option<ProfilerGuard<'a>>,
}

impl<'a> FlamegraphFrofiler<'a> {
    #[allow(dead_code)]
    pub fn new(frequency: c_int) -> Self {
        FlamegraphFrofiler {
            frequency,
            active_profiler: None,
        }
    }
}

impl<'a> Profiler for FlamegraphFrofiler<'a> {
    fn start_profiling(&mut self, _benchmark_id: &str, _benchmark_dir: &std::path::Path) {
        self.active_profiler = Some(ProfilerGuard::new(self.frequency).unwrap());
    }

    fn stop_profiling(&mut self, _benchmark_id: &str, benchmark_dir: &std::path::Path) {
        std::fs::create_dir_all(benchmark_dir).unwrap();
        let flamegraph_path = benchmark_dir.join("flamegraph.svg");
        let flamegraph_file =
            File::create(&flamegraph_path).expect("Could not create flamegraph.svg");

        if let Some(profiler) = self.active_profiler.take() {
            profiler
                .report()
                .build()
                .unwrap()
                .flamegraph(flamegraph_file)
                .expect("Could not write flamegraph")
        }
    }
}
