# Instructions for Claude

## Committing and pushing

Commit and push on your own once a piece of work is finished and verified; do
not wait to be asked.

- Verify first: `make test` at least; for changes to the compiler (`src/`) or
  the runtime (`runtime/`), also `make test-all`, `make fixpoint` and the ANSI
  suite (`tests/ansi/run.sh`, compare `build/ansi/summary.txt` with
  `tests/ansi/RESULTS.txt`).
- Commit with a descriptive message in the style of `git log`, then `git push`
  to `origin main`.
- Do not commit broken or half-done work.
- Ask before destructive operations (force push, rewriting published history).
