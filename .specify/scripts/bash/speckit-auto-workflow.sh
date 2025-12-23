#!/usr/bin/env bash
#
# speckit-auto-workflow.sh
#
# Automates the speckit workflow with intelligent interaction detection.
# Uses a separate Claude process to determine if user interaction is needed.
#
# Usage:
#   ./speckit-auto-workflow.sh "feature description"
#   ./speckit-auto-workflow.sh --continue           # Continue from last session
#   ./speckit-auto-workflow.sh --implement          # Run implement loop only
#

set -euo pipefail

# Resolve the actual script location (following symlinks)
SCRIPT_SOURCE="${BASH_SOURCE[0]}"
while [ -L "$SCRIPT_SOURCE" ]; do
    SCRIPT_DIR="$(cd "$(dirname "$SCRIPT_SOURCE")" && pwd)"
    SCRIPT_SOURCE="$(readlink "$SCRIPT_SOURCE")"
    [[ $SCRIPT_SOURCE != /* ]] && SCRIPT_SOURCE="$SCRIPT_DIR/$SCRIPT_SOURCE"
done
SCRIPT_DIR="$(cd "$(dirname "$SCRIPT_SOURCE")" && pwd)"

source "$SCRIPT_DIR/common.sh"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Configuration
SESSION_FILE="${TMPDIR:-/tmp}/speckit-session-id"
LOG_DIR="${TMPDIR:-/tmp}/speckit-logs"
ALLOWED_TOOLS="Read,Write,Edit,Bash,Glob,Grep,WebFetch,WebSearch"

# Judgment prompt for interaction detection
JUDGE_PROMPT='You are a workflow automation judge. Analyze the following output from a "speckit" workflow step.

Determine if user intervention is required to continue.

## Criteria for "yes" (user intervention needed):
- Explicit questions ending with "?" that require user decision
- Multiple choice options presented (A/B/C, 1/2/3, Option 1/Option 2)
- Phrases like "Which do you prefer?", "Please choose", "Do you want to..."
- [NEEDS CLARIFICATION] markers
- Errors or warnings that require user judgment to proceed
- Ambiguous situations requiring confirmation

## Criteria for "no" (can auto-continue):
- Completion messages: "Done", "Created", "Updated", "Completed"
- File generation/modification reports
- Progress summaries without questions
- "Next step is..." guidance (not a question)
- Successful validation results
- Simple status reports

## Output
Reply with ONLY "yes" or "no" on a single line. No explanation.'

# Initialize log directory
mkdir -p "$LOG_DIR"

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[OK]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_step() {
    echo ""
    echo -e "${CYAN}========================================${NC}"
    echo -e "${CYAN}  $1${NC}"
    echo -e "${CYAN}========================================${NC}"
}

# Check if claude CLI is available
check_claude_cli() {
    if ! command -v claude &> /dev/null; then
        log_error "claude CLI not found. Please install Claude Code."
        exit 1
    fi
}

# Save session ID for continuation
save_session_id() {
    local session_id="$1"
    echo "$session_id" > "$SESSION_FILE"
}

# Load saved session ID
load_session_id() {
    if [[ -f "$SESSION_FILE" ]]; then
        cat "$SESSION_FILE"
    else
        echo ""
    fi
}

# Run Claude and capture output
run_claude() {
    local prompt="$1"
    local session_id="${2:-}"
    local output_file="$LOG_DIR/step-$(date +%s).json"

    local cmd=(claude -p "$prompt" --allowedTools "$ALLOWED_TOOLS" --output-format json)

    if [[ -n "$session_id" ]]; then
        cmd=(claude --resume "$session_id" -p "$prompt" --allowedTools "$ALLOWED_TOOLS" --output-format json)
    fi

    "${cmd[@]}" > "$output_file" 2>&1 || true

    echo "$output_file"
}

# Extract session ID from JSON output
extract_session_id() {
    local json_file="$1"
    jq -r '.session_id // empty' "$json_file" 2>/dev/null || echo ""
}

# Extract result text from JSON output
extract_result() {
    local json_file="$1"
    jq -r '.result // .messages[-1].content // "No output"' "$json_file" 2>/dev/null || cat "$json_file"
}

# Check if interaction is needed using a separate Claude process
check_needs_interaction() {
    local output="$1"
    local result

    # Use a lightweight model for judgment (haiku is faster and cheaper)
    result=$(echo "$output" | claude -p "$JUDGE_PROMPT" --output-format text --max-turns 1 2>/dev/null | tr -d '[:space:]' | tr '[:upper:]' '[:lower:]')

    if [[ "$result" == "yes" ]]; then
        echo "yes"
    else
        echo "no"
    fi
}

# Execute a workflow step with interaction detection
execute_step() {
    local step_name="$1"
    local command="$2"
    local session_id="${3:-}"

    log_step "$step_name"
    log_info "Executing: $command"

    # Run the command
    local output_file
    output_file=$(run_claude "$command" "$session_id")

    # Extract results
    local new_session_id
    new_session_id=$(extract_session_id "$output_file")

    local result
    result=$(extract_result "$output_file")

    # Save session for continuation
    if [[ -n "$new_session_id" ]]; then
        save_session_id "$new_session_id"
    fi

    # Check if interaction is needed
    log_info "Checking if user interaction is needed..."
    local needs_interaction
    needs_interaction=$(check_needs_interaction "$result")

    if [[ "$needs_interaction" == "yes" ]]; then
        log_warn "User interaction required"
        echo ""
        echo -e "${YELLOW}--- Output ---${NC}"
        echo "$result"
        echo -e "${YELLOW}--- End Output ---${NC}"
        echo ""

        read -r -p "Your response (or press Enter to skip, 'q' to quit): " user_response

        if [[ "$user_response" == "q" ]]; then
            log_info "Workflow paused. Resume with: $0 --continue"
            exit 0
        elif [[ -n "$user_response" ]]; then
            # Send user response
            log_info "Sending your response..."
            output_file=$(run_claude "$user_response" "$new_session_id")
            new_session_id=$(extract_session_id "$output_file")
            result=$(extract_result "$output_file")

            if [[ -n "$new_session_id" ]]; then
                save_session_id "$new_session_id"
            fi
        fi
    else
        log_success "No interaction needed, continuing automatically..."
        # Show brief summary
        echo "$result" | head -20
        if [[ $(echo "$result" | wc -l) -gt 20 ]]; then
            echo "... (truncated, see full output in $output_file)"
        fi
    fi

    echo "$new_session_id"
}

# Analyze implementation plan and suggest next feature
analyze_next_feature() {
    log_step "Analyzing Implementation Plan"

    local analysis_prompt="@.specify/memory/implementation-plan.md の仕様と現状の実装を比較し、以下を出力してください：

1. 未実装機能の一覧（依存関係順にソート）
2. 各機能の実装に必要な変更箇所
3. 推奨する実装順序とその理由

最後に、次に実装すべき最優先の機能について、以下の形式で出力してください：

---
NEXT_FEATURE_PROMPT:
[/speckit.specify に渡すための具体的なプロンプト]
---"

    local output_file
    output_file=$(run_claude "$analysis_prompt")

    local session_id
    session_id=$(extract_session_id "$output_file")
    save_session_id "$session_id"

    local result
    result=$(extract_result "$output_file")

    echo "$result"
    echo ""

    # Extract the suggested prompt
    local suggested_prompt
    suggested_prompt=$(echo "$result" | sed -n '/NEXT_FEATURE_PROMPT:/,/---/p' | grep -v 'NEXT_FEATURE_PROMPT:' | grep -v '^---$' | head -10)

    if [[ -n "$suggested_prompt" ]]; then
        echo -e "${GREEN}Suggested feature prompt:${NC}"
        echo "$suggested_prompt"
        echo ""
        read -r -p "Use this prompt? (y/n/custom): " choice

        case "$choice" in
            y|Y|yes)
                echo "$suggested_prompt"
                ;;
            n|N|no)
                log_info "Workflow cancelled."
                exit 0
                ;;
            *)
                read -r -p "Enter your custom feature description: " custom_prompt
                echo "$custom_prompt"
                ;;
        esac
    else
        read -r -p "Enter feature description for /speckit.specify: " manual_prompt
        echo "$manual_prompt"
    fi
}

# Run the full workflow
run_full_workflow() {
    local feature_desc="$1"
    local session_id=""

    # Step 1: specify
    session_id=$(execute_step "Step 1/5: /speckit.specify" "/speckit.specify $feature_desc")

    # Step 2: clarify
    session_id=$(execute_step "Step 2/5: /speckit.clarify" "/speckit.clarify" "$session_id")

    # Step 3: plan
    session_id=$(execute_step "Step 3/5: /speckit.plan" "/speckit.plan" "$session_id")

    # Step 4: tasks
    session_id=$(execute_step "Step 4/5: /speckit.tasks" "/speckit.tasks" "$session_id")

    # Step 5: analyze
    session_id=$(execute_step "Step 5/5: /speckit.analyze" "/speckit.analyze" "$session_id")

    log_step "Workflow Complete!"
    log_success "All planning steps completed."
    echo ""
    echo "Session ID: $session_id"
    echo ""
    echo "Next steps:"
    echo "  1. Review the generated files in specs/<feature>/"
    echo "  2. Run implementation: $0 --implement"
    echo "  3. Or manually: claude --resume $session_id -p '/speckit.implement'"
}

# Run implement loop until all tasks complete
run_implement_loop() {
    local session_id
    session_id=$(load_session_id)

    if [[ -z "$session_id" ]]; then
        log_error "No session found. Run the workflow first or provide a session ID."
        exit 1
    fi

    local max_iterations=50
    local iteration=0

    while [[ $iteration -lt $max_iterations ]]; do
        iteration=$((iteration + 1))

        log_step "Implementation Iteration $iteration"

        # Run implement
        session_id=$(execute_step "Running /speckit.implement" "/speckit.implement" "$session_id")

        # Check if all tasks are complete
        log_info "Checking task completion status..."
        local check_result
        check_result=$(claude --resume "$session_id" -p "tasks.mdを確認し、全タスクが完了しているか確認してください。
完了している場合は 'ALL_TASKS_COMPLETE' と出力してください。
未完了タスクがある場合は 'TASKS_REMAINING: N' (Nは残りタスク数) と出力してください。" \
            --output-format text --max-turns 1 2>/dev/null || echo "TASKS_REMAINING: unknown")

        if echo "$check_result" | grep -q "ALL_TASKS_COMPLETE"; then
            log_success "All tasks completed!"
            break
        else
            local remaining
            remaining=$(echo "$check_result" | grep -o 'TASKS_REMAINING: [0-9]*' | grep -o '[0-9]*' || echo "?")
            log_info "Tasks remaining: $remaining"

            read -r -p "Continue with next iteration? (y/n): " continue_choice
            if [[ "$continue_choice" != "y" && "$continue_choice" != "Y" ]]; then
                log_info "Implementation paused. Resume with: $0 --implement"
                break
            fi
        fi
    done

    if [[ $iteration -ge $max_iterations ]]; then
        log_warn "Reached maximum iterations ($max_iterations). Please check manually."
    fi
}

# Continue from last session
continue_workflow() {
    local session_id
    session_id=$(load_session_id)

    if [[ -z "$session_id" ]]; then
        log_error "No saved session found."
        exit 1
    fi

    log_info "Resuming session: $session_id"

    # Ask what to do next
    echo ""
    echo "What would you like to do?"
    echo "  1. Run /speckit.clarify"
    echo "  2. Run /speckit.plan"
    echo "  3. Run /speckit.tasks"
    echo "  4. Run /speckit.analyze"
    echo "  5. Run /speckit.implement"
    echo "  6. Custom command"
    echo "  7. Open interactive session"
    echo ""
    read -r -p "Choice (1-7): " choice

    case "$choice" in
        1) execute_step "Running /speckit.clarify" "/speckit.clarify" "$session_id" ;;
        2) execute_step "Running /speckit.plan" "/speckit.plan" "$session_id" ;;
        3) execute_step "Running /speckit.tasks" "/speckit.tasks" "$session_id" ;;
        4) execute_step "Running /speckit.analyze" "/speckit.analyze" "$session_id" ;;
        5) run_implement_loop ;;
        6)
            read -r -p "Enter command: " custom_cmd
            execute_step "Running custom command" "$custom_cmd" "$session_id"
            ;;
        7)
            log_info "Opening interactive session..."
            claude --resume "$session_id"
            ;;
        *)
            log_error "Invalid choice"
            exit 1
            ;;
    esac
}

# Show usage
show_usage() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS] [FEATURE_DESCRIPTION]

Automates the speckit workflow with intelligent interaction detection.

OPTIONS:
    -h, --help          Show this help message
    -c, --continue      Continue from last saved session
    -i, --implement     Run implementation loop only
    -a, --analyze       Analyze implementation plan and suggest next feature
    -s, --session ID    Use specific session ID

EXAMPLES:
    # Start new workflow with feature description
    $(basename "$0") "Add user authentication with OAuth2"

    # Analyze and suggest next feature from implementation plan
    $(basename "$0") --analyze

    # Continue from last session
    $(basename "$0") --continue

    # Run implementation loop
    $(basename "$0") --implement

EOF
}

# Main entry point
main() {
    check_claude_cli

    case "${1:-}" in
        -h|--help)
            show_usage
            exit 0
            ;;
        -c|--continue)
            continue_workflow
            ;;
        -i|--implement)
            run_implement_loop
            ;;
        -a|--analyze)
            feature_desc=$(analyze_next_feature)
            if [[ -n "$feature_desc" ]]; then
                read -r -p "Start workflow with this feature? (y/n): " start_choice
                if [[ "$start_choice" == "y" || "$start_choice" == "Y" ]]; then
                    run_full_workflow "$feature_desc"
                fi
            fi
            ;;
        -s|--session)
            if [[ -z "${2:-}" ]]; then
                log_error "Session ID required"
                exit 1
            fi
            save_session_id "$2"
            continue_workflow
            ;;
        "")
            # No arguments - analyze and suggest
            feature_desc=$(analyze_next_feature)
            if [[ -n "$feature_desc" ]]; then
                read -r -p "Start workflow with this feature? (y/n): " start_choice
                if [[ "$start_choice" == "y" || "$start_choice" == "Y" ]]; then
                    run_full_workflow "$feature_desc"
                fi
            fi
            ;;
        *)
            # Feature description provided
            run_full_workflow "$*"
            ;;
    esac
}

main "$@"
