# API Contract: Hash Table Operations

**Feature**: 043-self-hosting-blockers
**Date**: 2025-12-28

## Overview

Defines the interface for hash table operations in Clysm.

---

## make-hash-table

Create a new hash table.

**Signature**:
```lisp
(make-hash-table &key test size) => hash-table
```

**Parameters**:
| Name | Type | Default | Description |
|------|------|---------|-------------|
| :test | function-designator | #'eql | Equality test function |
| :size | integer | 17 | Initial number of buckets |

**Returns**: A new empty hash-table

**Errors**:
- `type-error` if :test is not one of #'eq, #'eql, #'equal, #'equalp
- `type-error` if :size is not a positive integer

---

## gethash

Retrieve a value from a hash table.

**Signature**:
```lisp
(gethash key hash-table &optional default) => value, present-p
```

**Parameters**:
| Name | Type | Description |
|------|------|-------------|
| key | t | Key to look up |
| hash-table | hash-table | Hash table to search |
| default | t | Value to return if key not found (default: nil) |

**Returns**:
- Primary value: The associated value, or default if not found
- Secondary value: T if key was present, NIL otherwise

---

## (setf gethash)

Store a value in a hash table.

**Signature**:
```lisp
(setf (gethash key hash-table &optional default) new-value) => new-value
```

**Parameters**:
| Name | Type | Description |
|------|------|-------------|
| key | t | Key to store |
| hash-table | hash-table | Hash table to modify |
| new-value | t | Value to associate with key |

**Returns**: new-value

**Side Effects**: Modifies hash-table in place

---

## remhash

Remove an entry from a hash table.

**Signature**:
```lisp
(remhash key hash-table) => generalized-boolean
```

**Parameters**:
| Name | Type | Description |
|------|------|-------------|
| key | t | Key to remove |
| hash-table | hash-table | Hash table to modify |

**Returns**: T if key was present and removed, NIL otherwise

**Side Effects**: Modifies hash-table in place

---

## maphash

Iterate over hash table entries.

**Signature**:
```lisp
(maphash function hash-table) => nil
```

**Parameters**:
| Name | Type | Description |
|------|------|-------------|
| function | function | Function to call with (key value) |
| hash-table | hash-table | Hash table to iterate |

**Returns**: NIL

**Side Effects**: Calls function for each entry; function may have side effects

**Constraints**:
- It is undefined to add/remove entries during iteration (ANSI CL)
- Function is called with key as first arg, value as second

---

## WasmGC Generation

All hash table operations generate WasmGC instructions:

```wat
;; Example: (gethash 'x ht)
;; 1. Hash the key
;; 2. Compute bucket index
;; 3. Traverse chain looking for matching key
;; 4. Return value and present-p
```

Type validation via `ref.test $hash-table` before access.
