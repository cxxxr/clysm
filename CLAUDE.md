# Instructions for Claude

Work on a branch and open a pull request for each piece of verified work
(`gh pr create`); do not push to main directly. Commit, push the branch and
open the PR without being asked (no force push to main). Before opening it,
run the checks the change needs (`make test`; for compiler or runtime changes
also `make ansi` and `make test-all`, which includes the self-hosting check).

Merge such a pull request yourself once its checks pass (CI and the checks
above), so that later branches build on it.
