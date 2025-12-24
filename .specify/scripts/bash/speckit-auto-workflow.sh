#!/usr/bin/env bash
#
# speckit-auto-workflow.sh v2
#
# 完全自動化されたspec-kitワークフロー
# - implementation-plan.mdと現在の実装を比較して次の機能を自動決定
# - 推奨オプションを自動選択
# - 品質ゲートで問題があれば自動停止
#
# Usage:
#   ./speckit-auto-workflow.sh              # 自動で次の機能を決定して実行
#   ./speckit-auto-workflow.sh --continue   # 前回のセッションを続行
#   ./speckit-auto-workflow.sh --implement  # 実装ループのみ
#   ./speckit-auto-workflow.sh "機能説明"   # 手動で機能を指定
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

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m'

# Configuration
REPO_ROOT=$(get_repo_root)
SESSION_FILE="${TMPDIR:-/tmp}/speckit-session-id"
STATE_FILE="${TMPDIR:-/tmp}/speckit-state"
LOG_DIR="$REPO_ROOT/.specify/logs"
IMPL_PLAN="$REPO_ROOT/.specify/memory/implementation-plan.md"
SPECS_DIR="$REPO_ROOT/specs"
ALLOWED_TOOLS="Read,Write,Edit,Bash,Glob,Grep,WebFetch,WebSearch"

# States
STATE_START="start"
STATE_ANALYZE="analyze"
STATE_SPECIFY="specify"
STATE_CLARIFY="clarify"
STATE_PLAN="plan"
STATE_TASKS="tasks"
STATE_REVIEW="review"
STATE_IMPLEMENT="implement"
STATE_COMPLETE="complete"
STATE_PAUSED="paused"

# Initialize
mkdir -p "$LOG_DIR"
LOG_FILE="$LOG_DIR/auto-workflow-$(date +%Y-%m-%d).log"

#=============================================================================
# Logging
#=============================================================================

log() {
    local level="$1"
    shift
    local msg="$*"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] [$level] $msg" >> "$LOG_FILE"
}

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1" >&2
    log "INFO" "$1"
}

log_success() {
    echo -e "${GREEN}[OK]${NC} $1" >&2
    log "OK" "$1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1" >&2
    log "WARN" "$1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
    log "ERROR" "$1"
}

log_step() {
    echo "" >&2
    echo -e "${CYAN}════════════════════════════════════════════════════════════${NC}" >&2
    echo -e "${CYAN}  $1${NC}" >&2
    echo -e "${CYAN}════════════════════════════════════════════════════════════${NC}" >&2
    log "STEP" "$1"
}

log_auto_decision() {
    local context="$1"
    local decision="$2"
    local reason="$3"
    echo -e "${MAGENTA}[AUTO]${NC} $context: $decision" >&2
    log "AUTO_DECISION" "$context | $decision | $reason"
}

#=============================================================================
# State Management
#=============================================================================

save_state() {
    echo "$1" > "$STATE_FILE"
}

load_state() {
    if [[ -f "$STATE_FILE" ]]; then
        cat "$STATE_FILE"
    else
        echo "$STATE_START"
    fi
}

save_session() {
    echo "$1" > "$SESSION_FILE"
}

load_session() {
    if [[ -f "$SESSION_FILE" ]]; then
        cat "$SESSION_FILE"
    else
        echo ""
    fi
}

#=============================================================================
# Claude CLI Helpers
#=============================================================================

run_claude() {
    local prompt="$1"
    local session_id="${2:-}"
    local output_file="$LOG_DIR/step-$(date +%s).json"

    # プロンプトを表示
    echo "" >&2
    echo -e "${MAGENTA}┌─── Claude Prompt ───────────────────────────────────────────${NC}" >&2
    echo "$prompt" | head -20 | sed 's/^/│ /' >&2
    if [[ $(echo "$prompt" | wc -l) -gt 20 ]]; then
        echo -e "│ ${YELLOW}... (truncated)${NC}" >&2
    fi
    echo -e "${MAGENTA}└──────────────────────────────────────────────────────────────${NC}" >&2
    if [[ -n "$session_id" ]]; then
        echo -e "${BLUE}Session: ${session_id}${NC}" >&2
    fi
    echo "" >&2

    local cmd
    if [[ -n "$session_id" ]]; then
        cmd=(claude --resume "$session_id" -p "$prompt" --allowedTools "$ALLOWED_TOOLS" --output-format json)
    else
        cmd=(claude -p "$prompt" --allowedTools "$ALLOWED_TOOLS" --output-format json)
    fi

    "${cmd[@]}" > "$output_file" 2>&1 || true
    echo "$output_file"
}

extract_session_id() {
    local json_file="$1"
    jq -r '.session_id // empty' "$json_file" 2>/dev/null || echo ""
}

extract_result() {
    local json_file="$1"
    jq -r '.result // .messages[-1].content // "No output"' "$json_file" 2>/dev/null || cat "$json_file"
}

#=============================================================================
# Auto-Answer Logic
#=============================================================================

# 推奨オプションを自動選択
auto_answer() {
    local output="$1"

    # Recommended パターン
    if echo "$output" | grep -qE '\*\*Recommended:\*\*'; then
        echo "yes"
        return 0
    fi

    # Suggested パターン
    if echo "$output" | grep -qE '\*\*Suggested:\*\*'; then
        echo "yes"
        return 0
    fi

    # Option A/B/C の推奨
    if echo "$output" | grep -qE 'Recommended.*Option [A-E]'; then
        echo "yes"
        return 0
    fi

    return 1
}

# 対話が必要かどうかを判定（コマンド別）
needs_interaction() {
    local command="$1"
    local output="$2"

    case "$command" in
        specify)
            # [NEEDS CLARIFICATION] がある場合
            if echo "$output" | grep -qi "NEEDS CLARIFICATION"; then
                # ただし推奨がある場合は自動回答可能
                if auto_answer "$output" > /dev/null; then
                    return 1
                fi
                return 0
            fi
            return 1
            ;;
        clarify)
            # 質問がある場合
            if echo "$output" | grep -qE '\?$|Option [A-E]|選んでください|どうしますか'; then
                # 推奨がある場合は自動回答可能
                if auto_answer "$output" > /dev/null; then
                    return 1
                fi
                return 0
            fi
            # 曖昧さなしの場合
            if echo "$output" | grep -qi "No critical ambiguities"; then
                return 1
            fi
            return 1
            ;;
        analyze)
            # CRITICAL問題がある場合 - メトリクス表から実際の件数を確認
            local crit_count=0
            if echo "$output" | grep -qE 'Critical Issues Count.*\*\*[0-9]+\*\*'; then
                crit_count=$(echo "$output" | grep -oE 'Critical Issues Count.*\*\*[0-9]+\*\*' | head -1 | grep -oE '\*\*[0-9]+\*\*' | tr -d '*')
            fi
            # フォールバック: テーブル形式でSeverity列にCRITICALがある行
            if [[ "$crit_count" -eq 0 ]]; then
                crit_count=$(echo "$output" | grep -cE '\|\s*\*\*CRITICAL\*\*\s*\|' || echo 0)
            fi
            if [[ "$crit_count" -gt 0 ]]; then
                return 0
            fi
            return 1
            ;;
        implement)
            # チェックリスト未完了の確認
            if echo "$output" | grep -qi "checklists are incomplete"; then
                return 0
            fi
            return 1
            ;;
        *)
            return 1
            ;;
    esac
}

#=============================================================================
# Next Feature Detection
#=============================================================================

# implementation-plan.mdと現在の実装を比較して次の機能を決定
detect_next_feature() {
    log_step "Analyzing Implementation Plan vs Current State"

    local analysis_prompt="あなたはClysm（WebAssembly GC向けCommon Lispコンパイラ）の開発を支援するAIです。

## タスク
以下の情報を分析し、次に実装すべき機能を特定してください。

## 入力情報

### 1. 実装計画
@.specify/memory/implementation-plan.md

### 2. 既存のスペック
$(ls -1 "$SPECS_DIR" 2>/dev/null | sed 's/^/- /')

### 3. ソースコード構造
$(find "$REPO_ROOT/src" -type f -name "*.lisp" 2>/dev/null | head -30 | sed 's/^/- /')

### 4. 最近のコミット
$(cd "$REPO_ROOT" && git log --oneline -10 2>/dev/null | sed 's/^/- /')

## 分析手順

1. **Phase進捗の確認**
   - 各Phaseの検証基準（チェックボックス）を確認
   - 完了済み/未完了を判定

2. **既存specsとの対応**
   - どのspecがどのPhaseに対応するか
   - 実装済み機能の特定

3. **ギャップ分析**
   - implementation-plan.mdで定義されているが未実装の機能
   - 依存関係を考慮した優先順位

4. **次の機能の決定**
   - 依存関係が満たされている
   - 最も優先度が高い
   - 具体的なスコープが明確

## 出力形式

以下のJSON形式で出力してください：

\`\`\`json
{
  \"analysis\": {
    \"completed_phases\": [\"Phase 0の一部\", \"Phase 1の一部\"],
    \"in_progress\": [\"008-character-string\"],
    \"next_phase\": \"Phase 2\",
    \"reasoning\": \"Phase 1の基本機能は実装済み。Phase 2のクロージャ実装が次のステップ。\"
  },
  \"next_feature\": {
    \"phase\": \"Phase 2\",
    \"name\": \"クロージャとTail Call最適化\",
    \"description\": \"Phase 2: クロージャと再帰 (Lisp-1) を実装する。ファーストクラス関数とTail Call最適化を含む。\",
    \"scope\": [
      \"lambda式のコンパイル\",
      \"funcall/applyの実装\",
      \"labels/fletのサポート\",
      \"末尾呼び出し最適化\"
    ],
    \"verification\": [
      \"(funcall (lambda (x) (+ x 1)) 10) => 11\",
      \"(fact 100) がスタックオーバーフローなし\"
    ],
    \"dependencies\": [\"Phase 1の関数定義が完了していること\"]
  },
  \"specify_prompt\": \"Phase 2: クロージャと再帰 (Lisp-1) を実装する。目標はファーストクラス関数とTail Call最適化。lambda, funcall, apply, labels/fletをサポートし、末尾再帰でスタックオーバーフローが発生しないこと。\"
}
\`\`\`

JSONのみを出力してください。"

    local output_file
    output_file=$(run_claude "$analysis_prompt")

    local result
    result=$(extract_result "$output_file")

    # JSONを抽出
    local json
    json=$(echo "$result" | sed -n '/```json/,/```/p' | sed '1d;$d')

    if [[ -z "$json" ]]; then
        # JSONブロックがない場合、全体をJSONとして扱う
        json="$result"
    fi

    # 分析結果を表示（stderrへ）
    echo "" >&2
    echo -e "${CYAN}=== Analysis Result ===${NC}" >&2
    (echo "$json" | jq -r '.analysis.reasoning // "Analysis complete"' 2>/dev/null || echo "$result") >&2
    echo "" >&2

    # specify_promptを抽出
    local specify_prompt
    specify_prompt=$(echo "$json" | jq -r '.specify_prompt // empty' 2>/dev/null)

    if [[ -n "$specify_prompt" ]]; then
        echo -e "${GREEN}=== Next Feature ===${NC}" >&2
        (echo "$json" | jq -r '.next_feature.name // "Unknown"' 2>/dev/null) >&2
        echo "" >&2
        echo -e "${BLUE}Description:${NC}" >&2
        echo "$specify_prompt" >&2
        echo "" >&2

        # セッションIDを保存
        local session_id
        session_id=$(extract_session_id "$output_file")
        if [[ -n "$session_id" ]]; then
            save_session "$session_id"
        fi

        # 戻り値（stdoutへ）
        echo "$specify_prompt"
    else
        log_error "Could not determine next feature"
        echo "" >&2
        echo "Raw output:" >&2
        echo "$result" >&2
        return 1
    fi
}

#=============================================================================
# Workflow Steps
#=============================================================================

# Step: specify
run_specify() {
    local feature_desc="$1"
    local session_id="${2:-}"

    log_step "Step 1/5: /speckit.specify"
    log_info "Feature: $feature_desc"

    local output_file
    output_file=$(run_claude "/speckit.specify $feature_desc" "$session_id")

    local new_session_id
    new_session_id=$(extract_session_id "$output_file")
    save_session "$new_session_id"
    save_state "$STATE_SPECIFY"

    local result
    result=$(extract_result "$output_file")

    # 対話が必要か確認
    if needs_interaction "specify" "$result"; then
        if answer=$(auto_answer "$result"); then
            log_auto_decision "specify" "$answer" "recommended_option"
            output_file=$(run_claude "$answer" "$new_session_id")
            new_session_id=$(extract_session_id "$output_file")
            save_session "$new_session_id"
        else
            log_warn "Manual intervention required"
            echo "$result" >&2
            read -r -p "Your response: " user_response
            output_file=$(run_claude "$user_response" "$new_session_id")
            new_session_id=$(extract_session_id "$output_file")
            save_session "$new_session_id"
        fi
    fi

    log_success "Specify completed"
    echo "$new_session_id"
}

# Step: clarify (loop)
run_clarify() {
    local session_id="$1"
    local max_questions=5
    local question_count=0
    local auto_count=0
    local manual_count=0

    log_step "Step 2/5: /speckit.clarify"

    local output_file
    output_file=$(run_claude "/speckit.clarify" "$session_id")

    local new_session_id
    new_session_id=$(extract_session_id "$output_file")
    save_session "$new_session_id"
    save_state "$STATE_CLARIFY"

    local result
    result=$(extract_result "$output_file")

    # 曖昧さなしの場合
    if echo "$result" | grep -qi "No critical ambiguities"; then
        log_success "No clarification needed"
        echo "$new_session_id"
        return 0
    fi

    # 質問ループ
    while [[ $question_count -lt $max_questions ]]; do
        question_count=$((question_count + 1))

        if needs_interaction "clarify" "$result"; then
            if answer=$(auto_answer "$result"); then
                log_auto_decision "clarify_q$question_count" "$answer" "recommended_option"
                auto_count=$((auto_count + 1))
                output_file=$(run_claude "$answer" "$new_session_id")
            else
                log_warn "Manual intervention required for question $question_count"
                echo "" >&2
                echo "$result" | tail -50 >&2
                echo "" >&2
                read -r -p "Your response (or 'skip'): " user_response
                if [[ "$user_response" == "skip" ]]; then
                    user_response="done"
                fi
                manual_count=$((manual_count + 1))
                output_file=$(run_claude "$user_response" "$new_session_id")
            fi

            new_session_id=$(extract_session_id "$output_file")
            save_session "$new_session_id"
            result=$(extract_result "$output_file")
        else
            # 質問終了
            break
        fi

        # 完了チェック
        if echo "$result" | grep -qiE "clarification complete|no more questions|proceeding"; then
            break
        fi
    done

    log_success "Clarify completed (auto: $auto_count, manual: $manual_count)"
    echo "$new_session_id"
}

# Step: plan
run_plan() {
    local session_id="$1"

    log_step "Step 3/5: /speckit.plan"

    local output_file
    output_file=$(run_claude "/speckit.plan" "$session_id")

    local new_session_id
    new_session_id=$(extract_session_id "$output_file")
    save_session "$new_session_id"
    save_state "$STATE_PLAN"

    local result
    result=$(extract_result "$output_file")

    # 進捗表示（stderrへ）
    (echo "$result" | grep -E "Created|Generated|Updated" | head -10) >&2

    log_success "Plan completed"
    echo "$new_session_id"
}

# Step: tasks
run_tasks() {
    local session_id="$1"

    log_step "Step 4/5: /speckit.tasks"

    local output_file
    output_file=$(run_claude "/speckit.tasks" "$session_id")

    local new_session_id
    new_session_id=$(extract_session_id "$output_file")
    save_session "$new_session_id"
    save_state "$STATE_TASKS"

    local result
    result=$(extract_result "$output_file")

    # タスク数を表示（stderrへ）
    (echo "$result" | grep -E "Total|tasks|Tasks" | head -5) >&2

    log_success "Tasks generated"
    echo "$new_session_id"
}

# Step: analyze
run_analyze() {
    local session_id="$1"

    log_step "Step 5/5: /speckit.analyze"

    local output_file
    output_file=$(run_claude "/speckit.analyze" "$session_id")

    local new_session_id
    new_session_id=$(extract_session_id "$output_file")
    save_session "$new_session_id"
    save_state "$STATE_REVIEW"

    local result
    result=$(extract_result "$output_file")

    # 問題をカウント - メトリクス表から実際の件数を取得
    # 形式: "| Critical Issues Count | **N** |" または "Critical Issues Count.*N"
    local critical=0
    local high=0

    # メトリクス表から Critical Issues Count を抽出
    if echo "$result" | grep -qE 'Critical Issues Count.*\*\*[0-9]+\*\*'; then
        critical=$(echo "$result" | grep -oE 'Critical Issues Count.*\*\*[0-9]+\*\*' | head -1 | grep -oE '\*\*[0-9]+\*\*' | tr -d '*')
    fi

    # メトリクス表から High Issues Count を抽出
    if echo "$result" | grep -qE 'High Issues Count.*\*\*[0-9]+\*\*'; then
        high=$(echo "$result" | grep -oE 'High Issues Count.*\*\*[0-9]+\*\*' | head -1 | grep -oE '\*\*[0-9]+\*\*' | tr -d '*')
    fi

    # フォールバック: テーブル形式でSeverity列にCRITICALがある行をカウント
    if [[ "$critical" -eq 0 ]]; then
        critical=$(echo "$result" | grep -cE '\|\s*\*\*CRITICAL\*\*\s*\|' || echo 0)
    fi

    if [[ "$critical" -gt 0 ]]; then
        log_error "CRITICAL issues found: $critical"
        echo "" >&2
        (echo "$result" | grep -E '\|\s*\*\*CRITICAL\*\*\s*\|' | head -10) >&2
        echo "" >&2
        log_warn "Please review and fix before implementation"
        save_state "$STATE_PAUSED"
        return 1
    elif [[ "$high" -gt 0 ]]; then
        log_warn "HIGH issues found: $high (continuing with caution)"
    else
        log_success "No critical issues found"
    fi

    echo "$new_session_id"
}

# Step: implement loop
run_implement_loop() {
    local session_id="$1"
    local max_iterations=30
    local iteration=0

    save_state "$STATE_IMPLEMENT"

    while [[ $iteration -lt $max_iterations ]]; do
        iteration=$((iteration + 1))

        log_step "Implementation Iteration $iteration"

        local output_file
        output_file=$(run_claude "/speckit.implement" "$session_id")

        local new_session_id
        new_session_id=$(extract_session_id "$output_file")
        save_session "$new_session_id"
        session_id="$new_session_id"

        local result
        result=$(extract_result "$output_file")

        # チェックリスト確認
        if needs_interaction "implement" "$result"; then
            (echo "$result" | grep -i "checklist" | head -5) >&2
            read -r -p "Continue anyway? (y/n): " choice
            if [[ "$choice" != "y" ]]; then
                log_info "Paused by user"
                save_state "$STATE_PAUSED"
                return 0
            fi
        fi

        # 完了チェック
        log_info "Checking task completion..."
        local check_output
        check_output=$(run_claude "tasks.mdを確認し、未完了タスク数を報告してください。全完了なら'ALL_COMPLETE'、未完了があれば'REMAINING: N'と出力。" "$session_id")
        local check_result
        check_result=$(extract_result "$check_output")

        if echo "$check_result" | grep -q "ALL_COMPLETE"; then
            log_success "All tasks completed!"
            save_state "$STATE_COMPLETE"
            return 0
        fi

        local remaining
        remaining=$(echo "$check_result" | grep -oE 'REMAINING: [0-9]+' | grep -oE '[0-9]+' || echo "?")
        log_info "Tasks remaining: $remaining"

        # 自動続行（確認なし）
        sleep 1
    done

    log_warn "Reached maximum iterations ($max_iterations)"
}

#=============================================================================
# Main Workflow
#=============================================================================

# 自動コミット: 実装完了後に変更をコミット
auto_commit() {
    local feature_name="$1"

    log_step "Auto-committing changes"

    # 変更があるか確認
    if ! git -C "$REPO_ROOT" diff --quiet HEAD 2>/dev/null; then
        log_info "Uncommitted changes detected"
    elif ! git -C "$REPO_ROOT" diff --cached --quiet 2>/dev/null; then
        log_info "Staged changes detected"
    else
        # Untracked files check
        local untracked
        untracked=$(git -C "$REPO_ROOT" ls-files --others --exclude-standard 2>/dev/null | wc -l)
        if [[ "$untracked" -eq 0 ]]; then
            log_info "No changes to commit"
            return 0
        fi
    fi

    # 変更をステージング
    git -C "$REPO_ROOT" add -A

    # コミットメッセージを生成
    local commit_msg="feat: implement ${feature_name}

Automated implementation via speckit-auto workflow.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"

    # コミット実行
    if git -C "$REPO_ROOT" commit -m "$commit_msg"; then
        log_success "Changes committed successfully"
        return 0
    else
        log_error "Failed to commit changes"
        return 1
    fi
}

# 実装計画の完了チェック
check_implementation_complete() {
    log_info "Checking if implementation plan is complete..."

    local check_prompt="implementation-plan.mdと現在の実装を比較してください。

すべてのPhaseの検証基準が満たされている場合は 'COMPLETE' と出力。
まだ実装すべき機能がある場合は 'INCOMPLETE' と出力。

JSONなどは不要です。単語のみ出力してください。"

    local output_file
    output_file=$(run_claude "$check_prompt")

    local result
    result=$(extract_result "$output_file")

    if echo "$result" | grep -qi "COMPLETE"; then
        return 0  # 完了
    else
        return 1  # 未完了
    fi
}

run_full_workflow() {
    local feature_desc="$1"
    local auto_mode="${2:-false}"  # true の場合は確認なしで進行

    log_info "Starting full workflow"
    log "INFO" "Feature: $feature_desc"

    # Step 1: specify
    local session_id
    session_id=$(run_specify "$feature_desc")

    # Step 2: clarify
    session_id=$(run_clarify "$session_id")

    # Step 3: plan
    session_id=$(run_plan "$session_id")

    # Step 4: tasks
    session_id=$(run_tasks "$session_id")

    # Step 5: analyze
    if ! session_id=$(run_analyze "$session_id"); then
        log_warn "Workflow paused due to critical issues"
        echo "" >&2
        echo "To continue after fixing issues:" >&2
        echo "  ./speckit-auto --continue" >&2
        return 1
    fi

    log_step "Planning Phase Complete!"

    if [[ "$auto_mode" == "true" ]]; then
        # 自動モード: 確認なしで実装開始
        log_info "Auto mode: proceeding to implementation"
        run_implement_loop "$session_id"
    else
        echo "" >&2
        echo "Generated artifacts are ready for review." >&2
        echo "" >&2
        read -r -p "Start implementation? (y/n): " start_impl

        if [[ "$start_impl" == "y" ]]; then
            run_implement_loop "$session_id"
        else
            log_info "You can start implementation later with: ./speckit-auto --implement"
        fi
    fi
}

# フルオートループ: implementation-plan.md完了まで繰り返す
run_full_auto_loop() {
    local max_features=20  # 無限ループ防止
    local feature_count=0

    log_step "Starting Full Auto Loop"
    log_info "Will loop until implementation-plan.md is complete"
    echo "" >&2

    while [[ $feature_count -lt $max_features ]]; do
        feature_count=$((feature_count + 1))

        log_step "Feature Iteration $feature_count"

        # セッションをクリア（新しい機能ごとに新規セッション）
        rm -f "$SESSION_FILE" "$STATE_FILE"

        # 次の機能を検出
        log_info "Detecting next feature..."
        local feature_desc
        feature_desc=$(detect_next_feature)

        if [[ -z "$feature_desc" ]]; then
            log_warn "Could not detect next feature"

            # 完了チェック
            if check_implementation_complete; then
                log_success "Implementation plan is COMPLETE!"
                break
            else
                log_error "Failed to detect feature but plan is incomplete"
                return 1
            fi
        fi

        echo "" >&2
        echo -e "${CYAN}Feature $feature_count: $feature_desc${NC}" >&2
        echo "" >&2

        # フルワークフローを自動モードで実行
        if ! run_full_workflow "$feature_desc" "true"; then
            log_error "Workflow failed for feature: $feature_desc"
            log_info "Pausing auto loop. Resume with: ./speckit-auto --loop"
            return 1
        fi

        # 自動コミット
        local feature_name
        feature_name=$(echo "$feature_desc" | head -c 50 | tr -d '\n')
        if ! auto_commit "$feature_name"; then
            log_warn "Commit failed, but continuing..."
        fi

        # 完了チェック
        if check_implementation_complete; then
            log_success "Implementation plan is COMPLETE!"
            break
        fi

        log_info "Moving to next feature..."
        echo "" >&2
        sleep 2  # 小休止
    done

    if [[ $feature_count -ge $max_features ]]; then
        log_warn "Reached maximum feature count ($max_features)"
    fi

    log_step "Full Auto Loop Finished"
    log_info "Total features processed: $feature_count"
}

#=============================================================================
# Entry Points
#=============================================================================

show_usage() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS] [FEATURE_DESCRIPTION]

Fully automated spec-kit workflow with intelligent decision making.

OPTIONS:
    -h, --help          Show this help message
    -c, --continue      Continue from last saved session
    -i, --implement     Run implementation loop only
    -l, --loop          Full auto loop: detect → implement → commit → repeat
    -s, --status        Show current workflow status

EXAMPLES:
    # Auto-detect next feature and run full workflow (with confirmation)
    $(basename "$0")

    # Full auto loop until implementation-plan.md is complete
    $(basename "$0") --loop

    # Manually specify feature
    $(basename "$0") "Phase 2: クロージャとTail Call最適化を実装"

    # Continue paused workflow
    $(basename "$0") --continue

    # Run implementation only
    $(basename "$0") --implement

LOG FILE: $LOG_FILE

EOF
}

show_status() {
    local state=$(load_state)
    local session=$(load_session)

    echo "Current State: $state"
    echo "Session ID: ${session:-none}"
    echo "Log File: $LOG_FILE"

    if [[ -f "$LOG_FILE" ]]; then
        echo ""
        echo "Recent log entries:"
        tail -10 "$LOG_FILE"
    fi
}

continue_workflow() {
    local state=$(load_state)
    local session=$(load_session)

    if [[ -z "$session" ]]; then
        log_error "No saved session found"
        exit 1
    fi

    log_info "Resuming from state: $state"

    case "$state" in
        "$STATE_SPECIFY")
            session=$(run_clarify "$session")
            session=$(run_plan "$session")
            session=$(run_tasks "$session")
            run_analyze "$session" && run_implement_loop "$session"
            ;;
        "$STATE_CLARIFY")
            session=$(run_plan "$session")
            session=$(run_tasks "$session")
            run_analyze "$session" && run_implement_loop "$session"
            ;;
        "$STATE_PLAN")
            session=$(run_tasks "$session")
            run_analyze "$session" && run_implement_loop "$session"
            ;;
        "$STATE_TASKS"|"$STATE_REVIEW"|"$STATE_PAUSED")
            run_analyze "$session" && run_implement_loop "$session"
            ;;
        "$STATE_IMPLEMENT")
            run_implement_loop "$session"
            ;;
        *)
            log_error "Unknown state: $state"
            exit 1
            ;;
    esac
}

main() {
    case "${1:-}" in
        -h|--help)
            show_usage
            exit 0
            ;;
        -c|--continue)
            continue_workflow
            ;;
        -i|--implement)
            local session=$(load_session)
            if [[ -z "$session" ]]; then
                log_error "No saved session"
                exit 1
            fi
            run_implement_loop "$session"
            ;;
        -s|--status)
            show_status
            ;;
        -l|--loop)
            run_full_auto_loop
            ;;
        "")
            # 自動で次の機能を検出
            echo -e "${CYAN}Detecting next feature from implementation plan...${NC}" >&2
            echo "" >&2

            local feature_desc
            feature_desc=$(detect_next_feature)

            if [[ -z "$feature_desc" ]]; then
                log_error "Could not detect next feature"
                exit 1
            fi

            echo "" >&2
            read -r -p "Proceed with this feature? (y/n/custom): " choice

            case "$choice" in
                y|Y|yes)
                    run_full_workflow "$feature_desc"
                    ;;
                n|N|no)
                    log_info "Cancelled"
                    exit 0
                    ;;
                *)
                    read -r -p "Enter custom feature description: " custom_desc
                    run_full_workflow "$custom_desc"
                    ;;
            esac
            ;;
        *)
            # 手動で機能を指定
            run_full_workflow "$*"
            ;;
    esac
}

main "$@"
