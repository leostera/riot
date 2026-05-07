# riot-trace AGENTS

`riot-trace` owns profiler execution and trace artifact inspection/conversion. Binary
selection and builds stay in `riot-run`; CLI parsing and rendering stay in
`riot-cli`.

## Rules

1. Keep profiler backends behind typed requests and events.
2. Preserve application stdout/stderr behavior unless the selected profiler requires
   an explicit forwarding flag.
3. Keep `Path.t` typed until process execution or external tool boundaries.
4. Treat trace format conversion and summaries as best-effort wrappers over native
   tools until Riot owns a native trace format.
5. Do not clobber or append to trace outputs implicitly; require explicit runner
   policy such as overwrite or append.
