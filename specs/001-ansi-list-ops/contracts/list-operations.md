# Function Contracts: ANSI List Operations

**Date**: 2025-12-30
**Feature**: Phase 15A - ANSI List Operations Extension

## Overview

This document specifies the function signatures and contracts for all 21 list operations implemented in this feature. Each function follows ANSI Common Lisp semantics.

---

## List Tail Operations

### last

**Signature**: `(last list &optional (n 1)) → tail`

**Arguments**:
- `list`: A proper list
- `n`: Non-negative integer (default 1)

**Returns**: The last n cons cells of the list, or NIL if list has fewer than n elements.

**Contract**:
```lisp
;; Preconditions
(assert (listp list))
(assert (and (integerp n) (>= n 0)))

;; Postconditions
(assert (or (null result) (consp result)))
(assert (<= (length result) n))
```

**HyperSpec**: [f_last.htm](resources/HyperSpec/Body/f_last.htm)

---

### butlast

**Signature**: `(butlast list &optional (n 1)) → new-list`

**Arguments**:
- `list`: A proper list
- `n`: Non-negative integer (default 1)

**Returns**: A fresh list containing all but the last n elements.

**Contract**:
```lisp
;; Preconditions
(assert (listp list))
(assert (and (integerp n) (>= n 0)))

;; Postconditions
(assert (listp result))
(assert (= (length result) (max 0 (- (length list) n))))
;; Result shares no structure with input
```

**HyperSpec**: [f_butlas.htm](resources/HyperSpec/Body/f_butlas.htm)

---

### nbutlast

**Signature**: `(nbutlast list &optional (n 1)) → list`

**Arguments**:
- `list`: A proper list (may be modified)
- `n`: Non-negative integer (default 1)

**Returns**: The modified list with last n elements removed.

**Contract**:
```lisp
;; Preconditions
(assert (listp list))
(assert (and (integerp n) (>= n 0)))

;; Postconditions
;; Input list is destructively modified
;; May return different cons than original head if n >= length
```

**HyperSpec**: [f_butlas.htm](resources/HyperSpec/Body/f_butlas.htm)

---

### nth

**Signature**: `(nth n list) → element`

**Arguments**:
- `n`: Non-negative integer (zero-indexed)
- `list`: A proper list

**Returns**: The nth element of the list, or NIL if n >= length.

**Contract**:
```lisp
;; Preconditions
(assert (and (integerp n) (>= n 0)))
(assert (listp list))

;; Postconditions
;; Equivalent to (car (nthcdr n list))
```

**HyperSpec**: [f_nth.htm](resources/HyperSpec/Body/f_nth.htm)

---

### nthcdr

**Signature**: `(nthcdr n list) → tail`

**Arguments**:
- `n`: Non-negative integer
- `list`: A proper list

**Returns**: The result of applying cdr n times to list.

**Contract**:
```lisp
;; Preconditions
(assert (and (integerp n) (>= n 0)))
(assert (listp list))

;; Postconditions
(assert (listp result))
;; Result is eq to a tail of the input list
```

**HyperSpec**: [f_nthcdr.htm](resources/HyperSpec/Body/f_nthcdr.htm)

---

## Membership Operations

### member

**Signature**: `(member item list &key test key) → tail`

**Arguments**:
- `item`: Object to search for
- `list`: A proper list
- `test`: Two-argument equality function (default `#'eql`)
- `key`: One-argument key extraction function (default `#'identity`)

**Returns**: The tail of list starting from the first element that satisfies `(funcall test item (funcall key element))`, or NIL if not found.

**Contract**:
```lisp
;; Preconditions
(assert (listp list))
(assert (functionp test))
(assert (functionp key))

;; Postconditions
(assert (or (null result) (consp result)))
;; If non-nil, (funcall test item (funcall key (car result))) is true
```

**HyperSpec**: [f_mem_m.htm](resources/HyperSpec/Body/f_mem_m.htm)

---

### member-if

**Signature**: `(member-if predicate list &key key) → tail`

**Arguments**:
- `predicate`: One-argument predicate function
- `list`: A proper list
- `key`: One-argument key extraction function (default `#'identity`)

**Returns**: The tail of list starting from the first element where `(funcall predicate (funcall key element))` is true.

**HyperSpec**: [f_mem_m.htm](resources/HyperSpec/Body/f_mem_m.htm)

---

### member-if-not

**Signature**: `(member-if-not predicate list &key key) → tail`

**Arguments**:
- `predicate`: One-argument predicate function
- `list`: A proper list
- `key`: One-argument key extraction function (default `#'identity`)

**Returns**: The tail of list starting from the first element where `(funcall predicate (funcall key element))` is false.

**HyperSpec**: [f_mem_m.htm](resources/HyperSpec/Body/f_mem_m.htm)

---

## Association List Operations

### assoc

**Signature**: `(assoc item alist &key test key) → entry`

**Arguments**:
- `item`: Key to search for
- `alist`: An association list
- `test`: Two-argument equality function (default `#'eql`)
- `key`: One-argument key extraction function (default `#'identity`)

**Returns**: The first cons in alist whose CAR satisfies the test, or NIL.

**Contract**:
```lisp
;; Preconditions
(assert (listp alist))

;; Postconditions
(assert (or (null result) (consp result)))
;; Non-cons elements in alist are skipped
```

**HyperSpec**: [f_assocc.htm](resources/HyperSpec/Body/f_assocc.htm)

---

### assoc-if

**Signature**: `(assoc-if predicate alist &key key) → entry`

**Arguments**:
- `predicate`: One-argument predicate function
- `alist`: An association list
- `key`: One-argument key extraction function (default `#'identity`)

**Returns**: The first cons in alist whose CAR satisfies the predicate.

**HyperSpec**: [f_assocc.htm](resources/HyperSpec/Body/f_assocc.htm)

---

### rassoc

**Signature**: `(rassoc item alist &key test key) → entry`

**Arguments**:
- `item`: Value to search for
- `alist`: An association list
- `test`: Two-argument equality function (default `#'eql`)
- `key`: One-argument key extraction function (default `#'identity`)

**Returns**: The first cons in alist whose CDR satisfies the test, or NIL.

**HyperSpec**: [f_rassoc.htm](resources/HyperSpec/Body/f_rassoc.htm)

---

### rassoc-if

**Signature**: `(rassoc-if predicate alist &key key) → entry`

**Arguments**:
- `predicate`: One-argument predicate function
- `alist`: An association list
- `key`: One-argument key extraction function (default `#'identity`)

**Returns**: The first cons in alist whose CDR satisfies the predicate.

**HyperSpec**: [f_rassoc.htm](resources/HyperSpec/Body/f_rassoc.htm)

---

### pairlis

**Signature**: `(pairlis keys values &optional alist) → new-alist`

**Arguments**:
- `keys`: A list of keys
- `values`: A list of values (same length as keys)
- `alist`: Existing alist to append to (default NIL)

**Returns**: A new alist with pairs from keys and values prepended to alist.

**Contract**:
```lisp
;; Preconditions
(assert (= (length keys) (length values)))
(assert (listp alist))

;; Postconditions
;; New pairs are prepended (order may vary per implementation)
```

**HyperSpec**: [f_pairli.htm](resources/HyperSpec/Body/f_pairli.htm)

---

### acons

**Signature**: `(acons key value alist) → new-alist`

**Arguments**:
- `key`: Key for new entry
- `value`: Value for new entry
- `alist`: Existing alist

**Returns**: `(cons (cons key value) alist)`

**HyperSpec**: [f_acons.htm](resources/HyperSpec/Body/f_acons.htm)

---

### copy-alist

**Signature**: `(copy-alist alist) → new-alist`

**Arguments**:
- `alist`: An association list

**Returns**: A fresh copy of alist where both spine cons cells and entry cons cells are copied.

**Contract**:
```lisp
;; Postconditions
;; (not (eq result alist))
;; For each entry: (not (eq (nth i result) (nth i alist)))
;; But: (eq (caar result) (caar alist)) - keys/values not copied
```

**HyperSpec**: [f_cp_ali.htm](resources/HyperSpec/Body/f_cp_ali.htm)

---

## Set Operations

### intersection

**Signature**: `(intersection list1 list2 &key test key) → result-list`

**Arguments**:
- `list1`, `list2`: Lists treated as sets
- `test`: Two-argument equality function (default `#'eql`)
- `key`: One-argument key extraction function (default `#'identity`)

**Returns**: A list containing elements present in both lists (order unspecified).

**HyperSpec**: [f_intera.htm](resources/HyperSpec/Body/f_intera.htm)

---

### union

**Signature**: `(union list1 list2 &key test key) → result-list`

**Arguments**:
- `list1`, `list2`: Lists treated as sets
- `test`: Two-argument equality function (default `#'eql`)
- `key`: One-argument key extraction function (default `#'identity`)

**Returns**: A list containing elements from either list, without duplicates (order unspecified).

**HyperSpec**: [f_unionc.htm](resources/HyperSpec/Body/f_unionc.htm)

---

### set-difference

**Signature**: `(set-difference list1 list2 &key test key) → result-list`

**Arguments**:
- `list1`, `list2`: Lists treated as sets
- `test`: Two-argument equality function (default `#'eql`)
- `key`: One-argument key extraction function (default `#'identity`)

**Returns**: A list containing elements in list1 but not in list2.

**HyperSpec**: [f_set_di.htm](resources/HyperSpec/Body/f_set_di.htm)

---

### subsetp

**Signature**: `(subsetp list1 list2 &key test key) → boolean`

**Arguments**:
- `list1`, `list2`: Lists treated as sets
- `test`: Two-argument equality function (default `#'eql`)
- `key`: One-argument key extraction function (default `#'identity`)

**Returns**: T if every element of list1 is in list2, NIL otherwise.

**HyperSpec**: [f_subset.htm](resources/HyperSpec/Body/f_subset.htm)

---

### adjoin

**Signature**: `(adjoin item list &key test key) → result-list`

**Arguments**:
- `item`: Item to add
- `list`: A list treated as a set
- `test`: Two-argument equality function (default `#'eql`)
- `key`: One-argument key extraction function (default `#'identity`)

**Returns**: If item is already a member, returns list unchanged. Otherwise returns `(cons item list)`.

**HyperSpec**: [f_adjoin.htm](resources/HyperSpec/Body/f_adjoin.htm)

---

## Macro

### pushnew

**Signature**: `(pushnew item place &key test key) → new-value`

**Arguments**:
- `item`: Item to add
- `place`: A generalized reference (setf-able)
- `test`: Two-argument equality function (default `#'eql`)
- `key`: One-argument key extraction function (default `#'identity`)

**Expansion**:
```lisp
(setf place (adjoin item place :test test :key key))
```

**Returns**: The new value of place.

**HyperSpec**: [m_push.htm](resources/HyperSpec/Body/m_push.htm)
