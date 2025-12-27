# API Contract: List and Sequence Functions

**Feature**: 043-self-hosting-blockers
**Date**: 2025-12-28

## Overview

Defines the extended interface for list and sequence functions with keyword arguments.

---

## List Functions

### assoc

Find association by key.

**Signature**:
```lisp
(assoc item alist &key test key) => cons-or-nil
```

**Parameters**:
| Name | Type | Default | Description |
|------|------|---------|-------------|
| item | t | - | Key to find |
| alist | list | - | Association list |
| :test | function | #'eql | Comparison function |
| :key | function | #'identity | Key extractor applied to car |

**Returns**: First cons whose car matches item, or NIL

---

### rassoc

Find association by value.

**Signature**:
```lisp
(rassoc item alist &key test key) => cons-or-nil
```

**Parameters**:
| Name | Type | Default | Description |
|------|------|---------|-------------|
| item | t | - | Value to find |
| alist | list | - | Association list |
| :test | function | #'eql | Comparison function |
| :key | function | #'identity | Key extractor applied to cdr |

**Returns**: First cons whose cdr matches item, or NIL

---

### member

Find element in list.

**Signature**:
```lisp
(member item list &key test key) => tail-or-nil
```

**Parameters**:
| Name | Type | Default | Description |
|------|------|---------|-------------|
| item | t | - | Element to find |
| list | list | - | List to search |
| :test | function | #'eql | Comparison function |
| :key | function | #'identity | Key extractor |

**Returns**: Tail of list starting with matching element, or NIL

---

### adjoin

Add element if not present.

**Signature**:
```lisp
(adjoin item list &key test key) => list
```

**Parameters**:
| Name | Type | Default | Description |
|------|------|---------|-------------|
| item | t | - | Element to add |
| list | list | - | Target list |
| :test | function | #'eql | Comparison function |
| :key | function | #'identity | Key extractor |

**Returns**: List with item added (at front) if not already present

**Non-destructive**: Returns new cons cell if item added

---

### union

Compute set union.

**Signature**:
```lisp
(union list1 list2 &key test key) => list
```

**Parameters**:
| Name | Type | Default | Description |
|------|------|---------|-------------|
| list1 | list | - | First set |
| list2 | list | - | Second set |
| :test | function | #'eql | Comparison function |
| :key | function | #'identity | Key extractor |

**Returns**: List containing elements from both sets (order unspecified)

---

### intersection

Compute set intersection.

**Signature**:
```lisp
(intersection list1 list2 &key test key) => list
```

**Parameters**:
| Name | Type | Default | Description |
|------|------|---------|-------------|
| list1 | list | - | First set |
| list2 | list | - | Second set |
| :test | function | #'eql | Comparison function |
| :key | function | #'identity | Key extractor |

**Returns**: List containing elements present in both sets

---

## Sequence Functions

### position

Find position of element.

**Signature**:
```lisp
(position item sequence &key test key start end from-end) => index-or-nil
```

**Parameters**:
| Name | Type | Default | Description |
|------|------|---------|-------------|
| item | t | - | Element to find |
| sequence | sequence | - | Sequence to search |
| :test | function | #'eql | Comparison function |
| :key | function | #'identity | Key extractor |
| :start | integer | 0 | Starting index |
| :end | integer | nil | Ending index (nil = length) |
| :from-end | boolean | nil | Search from end |

**Returns**: Index of first match, or NIL

---

### find

Find element.

**Signature**:
```lisp
(find item sequence &key test key start end from-end) => element-or-nil
```

**Parameters**: Same as `position`

**Returns**: Matching element, or NIL

---

### remove

Remove matching elements.

**Signature**:
```lisp
(remove item sequence &key test key count start end from-end) => sequence
```

**Parameters**:
| Name | Type | Default | Description |
|------|------|---------|-------------|
| item | t | - | Element to remove |
| sequence | sequence | - | Source sequence |
| :test | function | #'eql | Comparison function |
| :key | function | #'identity | Key extractor |
| :count | integer | nil | Max removals (nil = all) |
| :start | integer | 0 | Starting index |
| :end | integer | nil | Ending index |
| :from-end | boolean | nil | Remove from end first |

**Returns**: New sequence with matches removed

**Non-destructive**: Always returns a new sequence

---

### substitute

Replace matching elements.

**Signature**:
```lisp
(substitute new old sequence &key test key count start end from-end) => sequence
```

**Parameters**:
| Name | Type | Default | Description |
|------|------|---------|-------------|
| new | t | - | Replacement element |
| old | t | - | Element to replace |
| sequence | sequence | - | Source sequence |
| :test | function | #'eql | Comparison function |
| :key | function | #'identity | Key extractor |
| :count | integer | nil | Max substitutions |
| :start | integer | 0 | Starting index |
| :end | integer | nil | Ending index |
| :from-end | boolean | nil | Substitute from end first |

**Returns**: New sequence with substitutions

---

## Keyword Argument Handling

All functions with :test/:key support:

1. **Default behavior**: :test defaults to #'eql, :key defaults to #'identity
2. **Function call**: When provided, test/key functions are called via closure dispatch
3. **Compile-time optimization**: If :test is literal #'eq or #'eql, use direct comparison

### Codegen Pattern

```wat
;; For :test #'equal
;; 1. Get element
;; 2. Apply :key if provided
;; 3. Call test function with item and keyed-element
;; 4. Check if result is non-nil (not ref.is_null)
```
