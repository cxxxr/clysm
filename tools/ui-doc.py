#!/usr/bin/env python3
"""Generate the reference manual of the UI framework from its source.

    tools/ui-doc.py            write docs/ui/reference/*.md
    tools/ui-doc.py --check    fail if an exported symbol has no documentation,
                               or if docs/ui/reference is not up to date

Every symbol that the package UI exports is documented where it is defined:
functions, macros, generic functions, variables and classes by their
docstrings, and the accessors of a class by the comment above their slot (or
the slot's :documentation).  The reference lists, for each of them, its kind,
its lambda list, its description and, for classes, the superclasses, the
initargs with their default values and the accessors.
"""

import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
UI = os.path.join(ROOT, 'ui')
OUT = os.path.join(ROOT, 'docs', 'ui', 'reference')

# the files, in the order of the manual, with their titles
FILES = [
    ('ui.lisp', 'core', 'Markup, components and state'),
    ('command.lisp', 'commands', 'Commands and menu items'),
    ('pane.lisp', 'panes', 'Panes, layouts, menus and interfaces'),
    ('present.lisp', 'presentations', 'Presentations and popup menus'),
    ('dialog.lisp', 'dialogs', 'Dialogs, files and the clipboard'),
    ('graphics.lisp', 'graphics', 'Drawing and graphs'),
    ('stream.lisp', 'streams', 'Stream panes and the interactor'),
    ('dom.lisp', 'dom', 'The DOM backend'),
    ('desktop.lisp', 'desktop', 'The desktop backend'),
    ('sexp.lisp', 'sexp', 'The S-expression backend'),
]

# exported symbols that are not defined but used in the syntax of a macro,
# which documents them
SYNTAX = {'&state': 'defcomponent', 'self': 'defcomponent'}

# ---------------------------------------------------------------------------
# A reader of Lisp source that keeps the text of every form and the comments
# in front of it.


class Node:
    def __init__(self, kind, value, start, end, comment, block=None):
        self.kind = kind          # 'list', 'atom', 'string', 'quote'
        self.value = value        # children, name, string or quoted node
        self.start = start
        self.end = end
        self.comment = comment    # the ;; lines right in front of it
        self.block = block or []  # all the comment lines since the previous form

    def name(self):
        return self.value.lower() if self.kind == 'atom' else None

    def is_list(self):
        return self.kind == 'list'

    def head(self):
        if self.kind == 'list' and self.value and self.value[0].kind == 'atom':
            return self.value[0].name()
        return None


class Reader:
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.comment = []
        self.block = []

    def skip(self):
        """Skip white space and comments; collect the ;-comments."""
        text = self.text
        while self.pos < len(text):
            c = text[self.pos]
            if c in ' \t\r':
                self.pos += 1
            elif c == '\n':
                self.pos += 1
                # a blank line ends a comment that belongs to what follows
                line_end = text.find('\n', self.pos)
                line = text[self.pos:line_end if line_end >= 0 else len(text)]
                if line.strip() == '':
                    self.comment = []
            elif c == ';':
                end = text.find('\n', self.pos)
                end = len(text) if end < 0 else end
                self.comment.append(text[self.pos:end])
                self.block.append(text[self.pos:end])
                self.pos = end
            elif text.startswith('#|', self.pos):
                end = text.find('|#', self.pos)
                self.pos = end + 2
            else:
                return

    def read(self):
        self.skip()
        if self.pos >= len(self.text):
            return None
        comment, self.comment = self.comment, []
        block, self.block = self.block, []
        start = self.pos
        text = self.text
        c = text[self.pos]
        if c == '(':
            self.pos += 1
            items = []
            while True:
                self.skip()
                if self.pos >= len(text):
                    raise SyntaxError('end of file in a list')
                if text[self.pos] == ')':
                    self.pos += 1
                    self.comment = []
                    self.block = []
                    break
                item = self.read()
                items.append(item)
            return Node('list', items, start, self.pos, comment, block)
        if c == ')':
            raise SyntaxError('unexpected ) at %d' % self.pos)
        if c == '"':
            self.pos += 1
            out = []
            while text[self.pos] != '"':
                if text[self.pos] == '\\':
                    self.pos += 1
                out.append(text[self.pos])
                self.pos += 1
            self.pos += 1
            return Node('string', ''.join(out), start, self.pos, comment)
        if c in "'`":
            self.pos += 1
            inner = self.read()
            return Node('quote', inner, start, self.pos, comment)
        if c == ',':
            self.pos += 2 if text.startswith(',@', self.pos) else 1
            inner = self.read()
            return Node('quote', inner, start, self.pos, comment)
        if c == '#':
            nxt = text[self.pos + 1]
            if nxt == "'":
                self.pos += 2
                inner = self.read()
                return Node('quote', inner, start, self.pos, comment)
            if nxt == '\\':
                self.pos += 3
                while self.pos < len(text) and (text[self.pos].isalnum() or text[self.pos] in '-_'):
                    self.pos += 1
                return Node('atom', text[start:self.pos], start, self.pos, comment)
            if nxt == '(':
                self.pos += 1
                inner = self.read()
                return Node('quote', inner, start, self.pos, comment)
            if nxt in '+-':
                # a feature expression: read and keep the following form
                self.pos += 2
                self.read()
                return self.read()
        # an atom
        while self.pos < len(text):
            ch = text[self.pos]
            if ch == '|':
                end = text.find('|', self.pos + 1)
                self.pos = end + 1
            elif ch in ' \t\r\n()";\'':
                break
            else:
                self.pos += 1
        return Node('atom', text[start:self.pos], start, self.pos, comment)

    def forms(self):
        out = []
        while True:
            form = self.read()
            if form is None:
                return out
            out.append(form)


def source(text, node):
    """The text of NODE, on one line."""
    return re.sub(r'\s+', ' ', text[node.start:node.end]).strip()


def comment_text(lines):
    """The text of comment lines, without the semicolons; NIL for section rules."""
    out = []
    for line in lines:
        body = line.lstrip(';').strip()
        if re.fullmatch(r'-+', body):
            continue
        out.append(body)
    return ' '.join(x for x in out if x).strip()


def strip_package(name):
    return name.split(':')[-1].lower()


# ---------------------------------------------------------------------------
# Definitions


class Definition:
    def __init__(self, name, kind, file, section):
        self.name = name                # lower case, like "push-button" or "(setf foo)"
        self.kind = kind
        self.file = file
        self.section = section
        self.lambda_list = None
        self.doc = ''
        self.supers = []
        self.slots = []                 # Slot
        self.value = None               # of variables
        self.extra = []                 # more lines
        self.aliases = []               # other names it documents (constructor, predicate)


class Slot:
    def __init__(self, name):
        self.name = name
        self.initargs = []
        self.initform = None
        self.readers = []               # (kind, name): 'accessor' or 'reader'
        self.doc = ''


def docstring(body):
    """The docstring at the start of BODY (a list of nodes), if there is one
    followed by more forms."""
    if len(body) >= 2 and body[0].kind == 'string':
        return body[0].value
    return ''


def option(options, key):
    for opt in options:
        if opt.is_list() and opt.value and opt.value[0].kind == 'atom' and opt.value[0].name() == key:
            return opt.value[1:]
    return None


def parse_slot(spec):
    if spec.kind == 'atom':
        return Slot(spec.name())
    items = spec.value
    slot = Slot(items[0].name())
    i = 1
    while i + 1 < len(items):
        key = items[i].name()
        val = items[i + 1]
        if key == ':initarg':
            slot.initargs.append(val.name())
        elif key == ':initform':
            slot.initform = val
        elif key in (':accessor', ':reader'):
            slot.readers.append((key[1:], val.name()))
        elif key == ':documentation' and val.kind == 'string':
            slot.doc = val.value
        i += 2
    if not slot.doc:
        slot.doc = comment_text(spec.comment)
    return slot


def collect(path, short):
    text = open(path, encoding='utf-8').read()
    forms = Reader(text).forms()
    defs = []
    section = None

    def visit(form):
        nonlocal section
        if form.block:
            lines = form.block
            for i, line in enumerate(lines):
                if re.fullmatch(r';+ *-{10,}', line.strip()):
                    rest = [l for l in lines[i + 1:] if l.strip(';').strip()]
                    if rest:
                        section = rest[0].lstrip(';').strip()
        head = form.head()
        if head is None:
            return
        items = form.value
        if head in ('progn', 'eval-when'):
            for child in items[2 if head == 'eval-when' else 1:]:
                visit(child)
            return

        def new(name, kind):
            d = Definition(name, kind, short, section)
            defs.append(d)
            return d

        if head in ('defun', 'defmacro') and len(items) >= 3:
            name = items[1]
            name = ('(setf %s)' % name.value[1].name()) if name.is_list() else name.name()
            d = new(name, 'Function' if head == 'defun' else 'Macro')
            d.lambda_list = source(text, items[2])
            d.doc = docstring(items[3:])
        elif head == 'defgeneric':
            name = items[1]
            name = ('(setf %s)' % name.value[1].name()) if name.is_list() else name.name()
            d = new(name, 'Generic function')
            d.lambda_list = source(text, items[2])
            doc = option(items[3:], ':documentation')
            d.doc = doc[0].value if doc else ''
        elif head in ('defvar', 'defparameter'):
            d = new(items[1].name(), 'Variable')
            if len(items) > 2:
                d.value = source(text, items[2])
            if len(items) > 3 and items[3].kind == 'string':
                d.doc = items[3].value
        elif head == 'defstate':
            d = new(items[1].name(), 'State')
            if len(items) > 2:
                d.value = source(text, items[2])
            if len(items) > 3 and items[3].kind == 'string':
                d.doc = items[3].value
        elif head in ('defclass', 'defmodel', 'define-condition'):
            kind = {'defclass': 'Class', 'defmodel': 'Model class', 'define-condition': 'Condition'}[head]
            d = new(items[1].name(), kind)
            d.supers = [s.name() for s in items[2].value]
            d.slots = [parse_slot(s) for s in items[3].value]
            doc = option(items[4:], ':documentation')
            d.doc = doc[0].value if doc else ''
            initargs = option(items[4:], ':default-initargs')
            if initargs:
                d.extra.append(('default-initargs', ' '.join(source(text, x) for x in initargs)))
        elif head == 'defstruct':
            spec = items[1]
            name = spec.value[0].name() if spec.is_list() else spec.name()
            conc = name + '-'
            include = None
            constructor = None
            if spec.is_list():
                for opt in spec.value[1:]:
                    if opt.is_list() and opt.head() == ':conc-name':
                        conc = opt.value[1].name() if len(opt.value) > 1 else ''
                    if opt.is_list() and opt.head() == ':include':
                        include = opt.value[1].name()
                    if opt.is_list() and opt.head() == ':constructor' and len(opt.value) > 1:
                        constructor = source(text, opt)
            d = new(name, 'Structure')
            rest = items[2:]
            if rest and rest[0].kind == 'string':
                d.doc = rest[0].value
                rest = rest[1:]
            if include:
                d.supers = [include]
            for s in rest:
                slot = Slot(s.value[0].name() if s.is_list() else s.name())
                slot.doc = comment_text(s.comment)
                # a comment after the slot, on the same line
                line_end = text.find('\n', s.end)
                tail = text[s.end:line_end]
                if ';' in tail and not slot.doc:
                    slot.doc = tail[tail.index(';'):].lstrip(';').strip()
                if s.is_list() and len(s.value) > 1:
                    slot.initform = s.value[1]
                slot.readers.append(('accessor', conc + slot.name))
                d.slots.append(slot)
            if constructor:
                d.extra.append(('constructor', constructor))
                d.aliases.append(re.match(r'\(:constructor (\S+)', constructor).group(1).lower())
            else:
                d.aliases.append('make-' + name)
            d.aliases.append(name + '-p')
        elif head == 'defcomponent':
            d = new(items[1].name(), 'Component')
            d.lambda_list = source(text, items[2])
            d.doc = docstring(items[3:])
        elif head == 'define-command':
            spec = items[1]
            name = spec.value[0].name() if spec.is_list() else spec.name()
            d = new(name, 'Command')
            d.lambda_list = source(text, items[2])
            d.doc = docstring(items[3:])
            if spec.is_list():
                d.extra.append(('options', source(text, spec)[len(name) + 1:-1].strip()))
        elif head == 'define-presentation-type':
            d = new(items[1].name(), 'Presentation type')
        elif head == 'define-interface':
            d = new(items[1].name(), 'Interface class')

    for form in forms:
        visit(form)
    return text, defs


def exported_symbols():
    text = open(os.path.join(UI, 'ui.lisp'), encoding='utf-8').read()
    start = text.index('(defpackage "UI"')
    form = Reader(text[start:]).read()
    names = []
    for opt in form.value[2:]:
        if opt.is_list() and opt.head() == ':export':
            names += [s.value.lower() for s in opt.value[1:] if s.kind == 'string']
    return names


# ---------------------------------------------------------------------------
# The manual


FILE_OF = {short: 'ui/' + filename for filename, short, _ in FILES}


def anchor(name):
    return re.sub(r'[^a-z0-9-]', '', name.replace(' ', '-').replace('*', 'x').replace('&', '').lower())


def lisp(name):
    """A name as it is written in a program: (setf foo) stays, others get ui:."""
    return name


def build():
    exported = exported_symbols()
    exported_set = set(exported)
    files = []
    owner = {}          # exported name -> (file short name, anchor, kind)
    accessor_of = {}    # accessor name -> (definition, slot)
    all_defs = []
    for filename, short, title in FILES:
        text, defs = collect(os.path.join(UI, filename), short)
        files.append((filename, short, title, defs))
        all_defs += defs
    classes = {d.name: d for d in all_defs if d.slots or d.kind in ('Model class', 'Class', 'Structure', 'Condition')}
    for d in all_defs:
        for slot in d.slots:
            for _, reader in slot.readers:
                accessor_of.setdefault(reader, (d, slot))

    # the documented entries: exported definitions; a definition that is not
    # exported but has exported accessors is listed too
    entries_by_file = {}
    for filename, short, title, defs in files:
        entries = []
        seen = set()
        for d in defs:
            base = d.name[6:-1] if d.name.startswith('(setf ') else d.name
            exported_here = base in exported_set
            has_exported_accessor = any(r in exported_set for s in d.slots for _, r in s.readers)
            if not (exported_here or has_exported_accessor):
                continue
            key = (d.name, d.kind)
            if key in seen:
                continue
            seen.add(key)
            entries.append(d)
            if exported_here and base not in owner:
                owner[base] = (short, anchor(d.name), d.kind)
        entries_by_file[short] = entries
    for name, (d, slot) in accessor_of.items():
        if name in exported_set and name not in owner:
            owner[name] = (d.file, anchor(d.name), 'Accessor')
    for d in all_defs:
        for alias in d.aliases:
            if alias in exported_set and alias not in owner:
                owner[alias] = (d.file, anchor(d.name), 'Function')
    # symbols that are part of the syntax of a macro
    for name, macro in SYNTAX.items():
        if name in exported_set and name not in owner and macro in owner:
            short, anc, _ = owner[macro]
            owner[name] = (short, anc, 'Symbol (in %s)' % macro)

    # what is missing
    problems = []
    for name in exported:
        if name not in owner:
            problems.append('%s: exported, but no definition was found' % name)
    for short, entries in entries_by_file.items():
        for d in entries:
            base = d.name[6:-1] if d.name.startswith('(setf ') else d.name
            if base in exported_set and not d.doc and d.kind not in ('Presentation type',):
                if d.name.startswith('(setf ') and any(e.name == base and e.doc for e in entries):
                    continue
                problems.append('%s (%s, %s): no documentation' % (d.name, d.kind, FILE_OF[d.file]))
            for slot in d.slots:
                if any(r in exported_set for _, r in slot.readers) and not slot.doc:
                    problems.append('%s: the slot %s (accessor %s) has no comment'
                                    % (d.name, slot.name, ', '.join(r for _, r in slot.readers)))

    def link(name, here=None):
        name = name.lower()
        base = name[6:-1] if name.startswith('(setf ') else name
        if base in owner:
            short, anc, _ = owner[base]
            target = ('#' + anc) if short == here else ('%s.md#%s' % (short, anc))
            return '[`%s`](%s)' % (name, target)
        return '`%s`' % name

    def initargs_of(d, seen=None):
        """(initarg default slot owner) of D and its superclasses."""
        seen = seen or set()
        out = []
        for slot in d.slots:
            for arg in slot.initargs:
                out.append((arg, slot, d))
        for s in d.supers:
            if s in classes and s not in seen:
                seen.add(s)
                out += initargs_of(classes[s], seen)
        return out

    def default_initargs(d):
        for key, value in d.extra:
            if key == 'default-initargs':
                return value
        return None

    pages = {}
    for filename, short, title, defs in files:
        entries = entries_by_file[short]
        out = ['# %s' % title, '',
               '*Generated from `ui/%s` by `tools/ui-doc.py`; do not edit.*' % filename, '']
        # the sections, with the entries in them
        sections = []
        for d in entries:
            if not sections or sections[-1][0] != d.section:
                sections.append((d.section, []))
            sections[-1][1].append(d)
        if len(sections) > 1 or (sections and sections[0][0]):
            out.append('**Contents**')
            out.append('')
            for name, items in sections:
                if name:
                    out.append('- %s: %s' % (name, ', '.join(link(x.name, short) for x in items)))
                else:
                    out.append('- %s' % ', '.join(link(x.name, short) for x in items))
            out.append('')
        for section, items in sections:
            if section:
                out += ['## %s' % section, '']
            for d in items:
                out.append('<a id="%s"></a>' % anchor(d.name))
                out.append('### %s' % d.name)
                out.append('')
                out.append('*%s*' % d.kind)
                out.append('')
                if d.lambda_list is not None:
                    if d.kind == 'Command':
                        out += ['```lisp', '(%s %s)' % (d.name, d.lambda_list[1:-1]) if len(d.lambda_list) > 2 else '(%s)' % d.name, '```', '']
                    elif d.name.startswith('(setf '):
                        ll = d.lambda_list[1:-1].split(' ', 1)
                        out += ['```lisp', '(setf (%s %s) %s)' % (d.name[6:-1], ll[1] if len(ll) > 1 else '', ll[0]), '```', '']
                    else:
                        inner = d.lambda_list[1:-1]
                        out += ['```lisp', ('(%s %s)' % (d.name, inner)) if inner else '(%s)' % d.name, '```', '']
                if d.value is not None and d.kind in ('Variable', 'State'):
                    out += ['Initial value: `%s`' % d.value, '']
                if d.supers:
                    out += ['Superclasses: %s' % ', '.join(link(s, short) for s in d.supers), '']
                if d.doc:
                    out += [format_doc(d.doc), '']
                for key, value in d.extra:
                    if key == 'options':
                        out += ['Options: `%s`' % value, '']
                    elif key == 'constructor':
                        out += ['Constructor: `%s`' % value, '']
                if d.kind in ('Model class', 'Class', 'Condition', 'Structure') or d.slots:
                    own = [s for s in d.slots if s.initargs or any(r in exported_set for _, r in s.readers)]
                    if own:
                        out += ['| Slot | Initargs | Default | Accessors | Description |', '|---|---|---|---|---|']
                        for s in own:
                            readers = ', '.join(link(r, short) if r in exported_set else '`%s`' % r
                                                for _, r in s.readers)
                            out.append('| %s | %s | %s | %s | %s |' % (
                                s.name, ', '.join('`%s`' % a for a in s.initargs),
                                ('`%s`' % source_of(s.initform)) if s.initform is not None else '',
                                readers, format_doc(s.doc).replace('|', '\\|').replace('\n', ' ')))
                        out.append('')
                    inherited = [(arg, owner_d) for arg, slot, owner_d in initargs_of(d) if owner_d is not d]
                    if inherited:
                        by_class = {}
                        for arg, owner_d in inherited:
                            by_class.setdefault(owner_d.name, []).append(arg)
                        out.append('Inherited initargs: ' + '; '.join(
                            '%s from %s' % (', '.join('`%s`' % a for a in args), link(cls, short))
                            for cls, args in by_class.items()))
                        out.append('')
                    di = default_initargs(d)
                    if di:
                        out += ['Default initargs: `%s`' % di, '']
                out.append('')
        pages[short] = '\n'.join(out).rstrip() + '\n'

    # the index of all symbols
    index = ['# Reference: index of symbols', '',
             '*Generated by `tools/ui-doc.py`; do not edit.*', '',
             'Every symbol exported by the package `UI`, with what it is and where it is documented.', '',
             '| Symbol | Kind | Chapter |', '|---|---|---|']
    titles = {short: title for _, short, title in FILES}
    for name in sorted(exported):
        if name in owner:
            short, anc, kind = owner[name]
            index.append('| [`%s`](%s.md#%s) | %s | [%s](%s.md) |' % (name, short, anc, kind, titles[short], short))
        else:
            index.append('| `%s` | ? | |' % name)
    pages['index'] = '\n'.join(index) + '\n'
    return pages, problems


_source_text = {}


def source_of(node):
    return re.sub(r'\s+', ' ', NODE_TEXT[id(node)]) if id(node) in NODE_TEXT else '...'


NODE_TEXT = {}


PLAIN_WORDS = {'OK', 'CSS', 'DOM', 'SVG', 'URL', 'JSON', 'CAPI', 'CLIM', 'HTML', 'REPL', 'IME',
               'JSPI', 'CSV', 'JS', 'API'}
SYMBOL = re.compile(r'(?<![\w`&:*-])([&:]?[A-Z*][A-Z0-9*+]*(?:-[A-Z0-9*+]+)+|[&:]?[A-Z*]{2,}[A-Z0-9*]*)(?![\w`])')


def format_doc(doc):
    """A docstring as Markdown: names in capitals are code in lower case, and
    the indented lines are a code block."""
    out = []
    code = []

    def end_code():
        if code:
            out.extend(['', '```lisp'] + code + ['```', ''])
            del code[:]

    for line in doc.split('\n'):
        if line.startswith('  ') and line.strip():
            code.append(SYMBOL.sub(lambda m: m.group(1).lower(), line[2:]))
        else:
            end_code()
            out.append(SYMBOL.sub(lambda m: m.group(1) if m.group(1) in PLAIN_WORDS
                                  else '`%s`' % m.group(1).lower(), line.strip()))
    end_code()
    return re.sub(r'\n{3,}', '\n\n', '\n'.join(out)).strip()


def examples(out_path):
    """Write the examples of the guide to OUT_PATH: every ```lisp block whose
    first line is ";;; example: NAME", in a package of its own; a block whose
    first line is ";;; continued" goes on with the previous example."""
    guide = os.path.join(ROOT, 'docs', 'ui', 'guide')
    blocks = []
    for name in sorted(os.listdir(guide)):
        if not name.endswith('.md'):
            continue
        text = open(os.path.join(guide, name), encoding='utf-8').read()
        for code in re.findall(r'```lisp\n(.*?)```', text, re.S):
            first = code.split('\n', 1)[0].strip()
            m = re.match(r';;; example: (\S+)', first)
            if m:
                blocks.append([name[:-3] + '/' + m.group(1), code])
            elif first == ';;; continued' and blocks:
                blocks[-1][1] += '\n' + code
    out = ['(in-package "UI-EXAMPLES")']
    for i, (name, code) in enumerate(blocks):
        package = 'GUIDE-EXAMPLE-%d' % (i + 1)
        out += ['(in-package "UI-EXAMPLES")',
                '(begin-example "%s")' % name,
                '(defpackage "%s" (:use "CL" "UI"))' % package,
                '(in-package "%s")' % package,
                code,
                '(in-package "UI-EXAMPLES")',
                '(end-example)']
    with open(out_path, 'w', encoding='utf-8') as f:
        f.write('\n'.join(out) + '\n')
    print('%d examples' % len(blocks))


def main():
    if '--examples' in sys.argv:
        examples(sys.argv[sys.argv.index('--examples') + 1])
        return
    # keep the text of the slot initforms
    original_collect = collect

    def collect_with_text(path, short):
        text, defs = original_collect(path, short)
        for d in defs:
            for s in d.slots:
                if s.initform is not None:
                    NODE_TEXT[id(s.initform)] = text[s.initform.start:s.initform.end]
        return text, defs
    globals()['collect'] = collect_with_text

    pages, problems = build()
    check = '--check' in sys.argv
    if problems:
        print('undocumented:')
        for p in problems:
            print('  ' + p)
        if check:
            sys.exit(1)
    if check:
        stale = []
        for short, page in pages.items():
            path = os.path.join(OUT, short + '.md')
            if not os.path.exists(path) or open(path, encoding='utf-8').read() != page:
                stale.append(path)
        if stale:
            print('not up to date (run tools/ui-doc.py):', ', '.join(os.path.relpath(p, ROOT) for p in stale))
            sys.exit(1)
        print('reference: %d symbols documented, up to date' % len(exported_symbols()))
        return
    os.makedirs(OUT, exist_ok=True)
    for short, page in pages.items():
        with open(os.path.join(OUT, short + '.md'), 'w', encoding='utf-8') as f:
            f.write(page)
    print('wrote %d pages to %s' % (len(pages), os.path.relpath(OUT, ROOT)))


if __name__ == '__main__':
    main()
