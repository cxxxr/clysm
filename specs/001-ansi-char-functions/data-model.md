# Data Model: Phase 16A - ANSI Character Functions

**Date**: 2025-12-31
**Feature**: 001-ansi-char-functions

## Entities

### Character (i31ref)

Characters are represented as immediate i31ref values containing Unicode code points.

| Attribute | Type | Description |
|-----------|------|-------------|
| code-point | i32 | Unicode code point (0-1,114,111) |

**Representation in WasmGC**:
```wat
;; Characters are i31ref values
;; Extract: (:ref.cast :i31) :i31.get_s -> i32
;; Create:  i32 :ref.i31 -> i31ref
```

**Validation Rules**:
- Valid range: 0 to #x10FFFF (Unicode maximum)
- Standard characters: 96 specific code points per ANSI CL

### Character Name Mapping

Static mapping between character code points and their string names.

| Code Point | Name | Alternate Names |
|------------|------|-----------------|
| 0 | "Null" | "NUL" |
| 8 | "Backspace" | "BS" |
| 9 | "Tab" | "HT" |
| 10 | "Newline" | "NL", "LF", "Linefeed" |
| 12 | "Page" | "FF" |
| 13 | "Return" | "CR" |
| 32 | "Space" | "SP" |
| 127 | "Rubout" | "DEL", "Delete" |

**Validation Rules**:
- Name lookup is case-insensitive
- Only named characters return non-NIL from `char-name`
- Printable characters (33-126) have no standard name

### Radix

Integer specifying numeric base for digit operations.

| Attribute | Type | Constraints |
|-----------|------|-------------|
| value | i32 | 2 ≤ value ≤ 36 |

**Validation Rules**:
- Values outside 2-36 signal type error
- Default value is 10 when omitted

### Weight

Non-negative integer representing digit value in a radix.

| Attribute | Type | Constraints |
|-----------|------|-------------|
| value | i32 | 0 ≤ value < radix |

**Validation Rules**:
- weight < 0: `digit-char` returns NIL
- weight ≥ radix: `digit-char` returns NIL
- Valid weights: 0-9 map to #\0-#\9, 10-35 map to #\A-#\Z

## Character Classification Sets

### Standard Characters (96 total)

```
Uppercase:  A B C D E F G H I J K L M N O P Q R S T U V W X Y Z  (26)
Lowercase:  a b c d e f g h i j k l m n o p q r s t u v w x y z  (26)
Digits:     0 1 2 3 4 5 6 7 8 9                                  (10)
Special:    Space, Newline                                        (2)
Semi-std:   ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~  (32)
```

**Code Point Ranges**:
- Uppercase: 65-90
- Lowercase: 97-122
- Digits: 48-57
- Space: 32
- Newline: 10
- Semi-standard: Various (see [13.1.7](../../resources/HyperSpec/Body/13_ag.htm))

### Graphic Characters

All printable characters: code points 32-126 (ASCII printable range).

### Both-Case Characters

Characters with both uppercase and lowercase variants:
- A-Z (65-90)
- a-z (97-122)

## State Transitions

N/A - All character functions are stateless predicates or converters.

## Relationships

```
Character (i31ref)
    ↓ char-int
Integer (i31ref)

Character (i31ref)
    ↓ char-name (if named)
String ($string) | NIL

String ($string)
    ↓ name-char (if valid name)
Character (i31ref) | NIL

Weight + Radix
    ↓ digit-char (if valid)
Character (i31ref) | NIL

Character (i31ref)
    ↓ digit-char-p (existing)
Weight (i31ref) | NIL
```

## WasmGC Type References

| Type Index | Name | Usage |
|------------|------|-------|
| N/A | i31ref | Character representation |
| 2 | $string | char-name return value |
| N/A | ref.null none | NIL representation |
