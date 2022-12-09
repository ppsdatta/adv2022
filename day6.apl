⍝ Very basic approach with APL

Range ← {⍺,⍺+⍳(⍵-⍺)}
F1 ← {0≠+/(≢⊃∩/⊃¨{Range/(⊃⍵)}¨⍵)=(≢∘⊃)¨({Range/(⊃⍵)}¨⍵)}
F2 ← {0>⍨≢⊃∩/⊃¨{Range/(⊃⍵)}¨⍵}

d1 ← (⊂2 8) (⊂3 7)
d2 ← (⊃2 4) (⊃6 8)
d3 ← (⊃5 7) (⊃7 9)

F1 d1
F1 d2

F2 d1
F2 d3
