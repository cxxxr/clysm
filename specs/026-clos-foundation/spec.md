# Feature Specification: CLOS Foundation

**Feature Branch**: `026-clos-foundation`
**Created**: 2025-12-27
**Status**: Draft
**Input**: User description: "Phase 7: CLOS基盤 (Lisp-6) を実装する。目標は基本的なオブジェクトシステムの構築。defclass（スロット定義、:initarg、:accessor、:initform）、make-instance（インスタンス生成）、defmethod（メソッド定義と型特化）、総称関数ディスパッチ（compute-applicable-methods）を実装する。WasmGC型として$standard-class、$instance (struct: class + slot_vector)、$slot_vector (array anyref)を定義。単一継承から開始し、メソッド結合(:before, :after, :around)は後続フェーズとする。検証基準: (make-instance 'point :x 3 :y 4)がインスタンスを生成し、アクセサとメソッドディスパッチが正常動作すること。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Define a Class with Slots (Priority: P1)

A developer defines a new class using `defclass` with named slots. Each slot can specify initialization arguments (`:initarg`), accessor functions (`:accessor`), and default values (`:initform`). This is the foundational capability that all other CLOS features depend on.

**Why this priority**: Without class definition, no other CLOS features can function. This is the foundation of the entire object system.

**Independent Test**: Can be fully tested by defining a simple class like `point` with `:x` and `:y` slots and verifying the class object is created with correct slot metadata.

**Acceptance Scenarios**:

1. **Given** the compiler processes a `defclass` form, **When** the form defines a class `point` with slots `x` and `y` each having `:initarg`, `:accessor`, and `:initform`, **Then** a class object is created and registered in the class namespace.

2. **Given** a `defclass` form with a superclass, **When** the class is defined with a parent class specification, **Then** the child class inherits all slots from the parent class.

3. **Given** a `defclass` form with duplicate slot names (including inherited), **When** compilation occurs, **Then** an error is signaled indicating the duplicate slot.

---

### User Story 2 - Create Instances with make-instance (Priority: P1)

A developer creates instances of defined classes using `make-instance`. The function accepts the class name and keyword arguments matching slot `:initarg` specifications. Slots are initialized according to the provided arguments or their `:initform` defaults.

**Why this priority**: Instance creation is essential for validating class definitions and is required for all object-oriented programming patterns.

**Independent Test**: Can be fully tested by calling `(make-instance 'point :x 3 :y 4)` and verifying the returned object has the correct slot values.

**Acceptance Scenarios**:

1. **Given** a defined class `point` with slots `x` and `y`, **When** `(make-instance 'point :x 3 :y 4)` is called, **Then** an instance is returned with `x` slot containing `3` and `y` slot containing `4`.

2. **Given** a class with a slot having `:initform 0`, **When** `make-instance` is called without providing that slot's `:initarg`, **Then** the slot is initialized to `0`.

3. **Given** a class with a slot having no `:initform` and the `:initarg` is not provided, **When** `make-instance` is called, **Then** the slot is unbound (accessing it signals an error).

4. **Given** an unknown class name, **When** `make-instance` is called with that name, **Then** an error is signaled indicating the class does not exist.

---

### User Story 3 - Access Slots via Accessors (Priority: P2)

A developer reads and writes slot values using automatically generated accessor functions. When a slot specifies `:accessor`, both a reader function (the accessor name) and a writer function (via `setf`) are generated.

**Why this priority**: Accessors provide the standard interface for slot manipulation and are essential for encapsulation, though direct slot access could work as a fallback.

**Independent Test**: Can be fully tested by creating a `point` instance and calling `(point-x instance)` to read and `(setf (point-x instance) 10)` to write.

**Acceptance Scenarios**:

1. **Given** a class `point` with slot `x` having `:accessor point-x`, **When** an instance is created and `(point-x instance)` is called, **Then** the current value of slot `x` is returned.

2. **Given** the same class and instance, **When** `(setf (point-x instance) 10)` is called, **Then** the slot `x` is updated to `10` and subsequent reads return `10`.

3. **Given** an accessor is called with a non-instance argument, **When** the accessor is invoked, **Then** an error is signaled indicating type mismatch.

---

### User Story 4 - Define Methods with Type Specialization (Priority: P2)

A developer defines methods using `defmethod` that specialize on argument types. Methods can be specialized on class names, allowing different implementations for different argument types.

**Why this priority**: Methods with specialization enable polymorphism, which is the core value proposition of an object system.

**Independent Test**: Can be fully tested by defining `(defmethod area ((p point)) ...)` and verifying it can be called on point instances.

**Acceptance Scenarios**:

1. **Given** a generic function `area` does not exist, **When** `(defmethod area ((p point)) (* (point-x p) (point-y p)))` is defined, **Then** a generic function `area` is created with one method specialized on `point`.

2. **Given** the generic function `area` exists, **When** another method `(defmethod area ((c circle)) ...)` is defined, **Then** the method is added to the existing generic function.

3. **Given** a method with multiple specialized parameters, **When** the method is defined, **Then** dispatch considers all specialized parameters (left-to-right precedence).

4. **Given** a method with unspecialized parameters (no class name), **When** the method is defined, **Then** those parameters match any argument type (specialized on `t`).

---

### User Story 5 - Generic Function Dispatch (Priority: P2)

When a generic function is called, the system determines the most specific applicable method based on the runtime types of arguments. This enables polymorphic behavior where the same function name produces different results based on argument types.

**Why this priority**: Dispatch is what makes methods useful - without it, specialization has no effect at runtime.

**Independent Test**: Can be fully tested by defining methods for `area` on both `point` (returning product) and `circle` (returning pi*r^2), then calling `area` on each type and verifying correct method executes.

**Acceptance Scenarios**:

1. **Given** two methods on `area` specialized on `point` and `circle`, **When** `(area point-instance)` is called, **Then** the point-specialized method executes.

2. **Given** a class hierarchy where `colored-point` inherits from `point`, and only a method specialized on `point` exists, **When** `(area colored-point-instance)` is called, **Then** the point-specialized method is applicable and executes.

3. **Given** no applicable method exists for the argument types, **When** the generic function is called, **Then** an error is signaled indicating no applicable method.

4. **Given** multiple applicable methods exist (parent and child class), **When** dispatch occurs, **Then** the most specific method (child class) is selected.

---

### User Story 6 - Single Inheritance (Priority: P3)

A developer defines classes that inherit from a single parent class. Child classes inherit all slots from their parent and can add additional slots. Methods specialized on parent classes apply to instances of child classes.

**Why this priority**: Single inheritance provides class reuse and hierarchical organization. Multiple inheritance is deferred to a future phase.

**Independent Test**: Can be fully tested by defining `(defclass colored-point (point) ((color :initarg :color)))` and verifying instances have both inherited and new slots.

**Acceptance Scenarios**:

1. **Given** class `point` exists with slots `x` and `y`, **When** `colored-point` is defined inheriting from `point` with additional slot `color`, **Then** `colored-point` instances have all three slots.

2. **Given** the same hierarchy, **When** `(make-instance 'colored-point :x 1 :y 2 :color 'red)` is called, **Then** all three slots are correctly initialized.

3. **Given** an attempt to define a class with multiple parents, **When** compilation occurs, **Then** an error is signaled (multiple inheritance not supported in this phase).

---

### Edge Cases

- What happens when a slot `:initform` references other slots? The initform is evaluated in an environment where no slots are yet accessible; attempting to reference slots should signal an error.
- How does the system handle circular inheritance (A inherits B, B inherits A)? An error should be signaled during class definition.
- What happens when redefining an existing class? The class is updated and existing instances retain their identity (though slot layout changes may cause issues - this is an advanced case).
- How are slot name conflicts handled between parent and child? Child slot definitions shadow parent slots of the same name, with a warning.
- What happens when a generic function is called with zero arguments? If no methods are applicable, an error is signaled; otherwise the applicable method executes.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST support `defclass` macro that defines new classes with named slots
- **FR-002**: System MUST support slot options `:initarg` (keyword for initialization), `:accessor` (generates reader/writer), and `:initform` (default value expression)
- **FR-003**: System MUST support `make-instance` function that creates instances of defined classes
- **FR-004**: System MUST initialize slots from `:initarg` keyword arguments passed to `make-instance`
- **FR-005**: System MUST use `:initform` values for slots not provided via `:initarg`
- **FR-006**: System MUST generate reader functions for slots with `:accessor` option
- **FR-007**: System MUST generate `setf` writer functions for slots with `:accessor` option
- **FR-008**: System MUST support `defmethod` macro that defines methods specialized on class types
- **FR-009**: System MUST support generic function creation (implicitly via first `defmethod`)
- **FR-010**: System MUST dispatch generic function calls to the most specific applicable method
- **FR-011**: System MUST signal an error when no applicable method exists
- **FR-012**: System MUST support single inheritance via parent class specification in `defclass`
- **FR-013**: System MUST inherit all parent class slots to child classes
- **FR-014**: System MUST allow methods specialized on parent classes to apply to child class instances
- **FR-015**: System MUST signal an error for undefined class names in `make-instance`
- **FR-016**: System MUST signal an error for multiple inheritance attempts (deferred to future phase)

### Key Entities

- **Standard-Class**: Metaclass representing user-defined classes; contains class name, parent class reference, and slot definitions
- **Instance**: Runtime object containing a reference to its class and a vector of slot values
- **Slot Definition**: Metadata describing a slot including name, initarg keyword, accessor name, and initform expression
- **Generic Function**: Named function object containing a collection of methods and dispatch logic
- **Method**: Specialized implementation with a list of type specializers and a body function

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can define classes with 1-10 slots in a single `defclass` form
- **SC-002**: Instance creation with `make-instance` completes successfully for all valid class/argument combinations
- **SC-003**: Accessor functions read and write slot values correctly for all slot types
- **SC-004**: Method dispatch selects the correct specialized method in inheritance hierarchies up to 5 levels deep
- **SC-005**: The verification example `(make-instance 'point :x 3 :y 4)` produces a valid instance with accessible slot values
- **SC-006**: Compilation of CLOS forms produces valid output that passes validation
- **SC-007**: Error conditions (undefined class, no applicable method, type mismatch) produce clear, descriptive error messages
- **SC-008**: All user scenarios pass their acceptance criteria when tested independently

## Assumptions

- Single inheritance is sufficient for this phase; multiple inheritance will be added in a subsequent feature
- Method combination (:before, :after, :around) is explicitly deferred to a future phase
- Class redefinition behavior follows a simplified model (update class, existing instances unchanged)
- Generic functions are implicitly created by the first `defmethod`; explicit `defgeneric` is optional
- Slot access within methods uses generated accessors rather than lower-level slot-value access (slot-value may be added later)
- The system does not need to support MOP (Meta-Object Protocol) introspection in this phase

## Scope Boundaries

### In Scope
- defclass with :initarg, :accessor, :initform slot options
- make-instance with keyword argument initialization
- defmethod with class specialization
- Generic function dispatch (compute-applicable-methods equivalent)
- Single inheritance
- Reader and writer (setf) accessor generation

### Out of Scope (Deferred)
- Multiple inheritance
- Method combination (:before, :after, :around, :primary)
- defgeneric explicit declaration
- change-class, update-instance-for-redefined-class
- MOP (Meta-Object Protocol)
- slot-value direct access
- with-slots, with-accessors macros
- class-of, type-of introspection (unless needed for dispatch)
