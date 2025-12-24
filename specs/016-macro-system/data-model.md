# Data Model: Macro System

**Date**: 2025-12-24
**Branch**: 016-macro-system

## Entities

### 1. Macro Registry

Compile-time storage for macro definitions.

```
MacroRegistry
├── table: HashMap<Symbol, MacroFunction>
└── depth-limit: Integer (default: 1000)
```

**Operations**:
- `register-macro(name, expander)`: Add macro to registry
- `macro-function*(name)`: Lookup macro by name
- `macro-form-p(form)`: Check if form is a macro call

### 2. Macro Definition

Parsed result of a `defmacro` form.

```
DefmacroResult
├── name: Symbol
├── lambda-list: LambdaList
├── body: List<Form>
└── docstring: String | nil
```

### 3. Lambda List (Extended)

Parsed macro lambda list supporting destructuring.

```
LambdaList
├── required: List<Parameter>
├── optional: List<OptionalParam>
├── rest: Parameter | nil
├── rest-kind: :rest | :body
├── keys: List<KeyParam>        # NEW
└── allow-other-keys: Boolean   # NEW
```

**Parameter Types**:
```
Parameter = Symbol | DestructuringPattern

OptionalParam
├── name: Symbol
├── default: Form | nil
└── supplied-p: Symbol | nil

KeyParam                        # NEW
├── name: Symbol
├── keyword: Keyword
├── default: Form | nil
└── supplied-p: Symbol | nil
```

### 4. Quasiquote Form

Intermediate representation during backquote expansion.

```
QuasiquoteForm
├── (quasiquote <template>)
├── (unquote <expr>)
└── (unquote-splicing <expr>)
```

**Expansion Output**:
- `(quote <constant>)` - fully constant templates
- `(list ...)` - simple list construction
- `(append ...)` - splicing required

### 5. Compile-Time Environment

Environment passed through macro expansion.

```
CompileEnv
├── macros: MacroRegistry
├── symbols: HashMap<Symbol, SymbolInfo>
└── expansion-depth: Integer    # NEW - tracks current depth
```

## State Transitions

### Macro Expansion State

```
Form
  │
  ├─[is macro]──→ Expanding ──→ Expanded ──→ Form (recurse)
  │                   │
  │              [depth > limit]
  │                   │
  │                   ▼
  │               ERROR
  │
  └─[not macro]──→ Walk Subforms ──→ Done
```

### Backquote Expansion

```
(quasiquote template)
         │
         ▼
    expand-bq
         │
    ┌────┴────┐
    │         │
[atom]    [list]
    │         │
    ▼         ▼
(quote x)  expand-bq-list
              │
        ┌─────┴─────┐
        │           │
   [all const]  [has splice]
        │           │
        ▼           ▼
   (quote ...)  (append ...)
```

## Validation Rules

1. **Macro name uniqueness**: Redefining a macro overwrites the previous definition
2. **Lambda list syntax**: Must follow Common Lisp conventions
3. **Expansion depth**: Maximum 1000 levels before error
4. **Unquote context**: `,@` only valid inside list context within backquote

## Relationships

```
MacroRegistry 1───* DefmacroResult
     │
     └──uses──→ CompileEnv
                    │
                    └──produces──→ ExpandedForm
                                       │
                                       └──contains──→ QuasiquoteForm (resolved)
```
