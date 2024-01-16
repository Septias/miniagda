# miniagda
rust implementation for a subset of the [agda](https://github.com/agda/agda) programming language[^0]

## features

### implemented
- indexed families of data types

### in progress
- definitions by dependent pattern matching

### planned
- implicit arguments 
- holes
- granular caching

## structure

```
crates/             -- rust code
├─ miniagda-bin/      -- standalone binary elaborate an agda file once
├─ miniagda-core/     -- the actual language implementation
└─ miniagda-watch/    -- standalone binary with tui to actively work on agda file
examples/           -- example agda files that can be elaborated using miniagda
materials/          -- presentation slides
scripts/            -- development and installation scripts
```

## literature

- J. Cockx: Dependent pattern matching and proof-relevant unification
- J. Cockx, A. Abel: Elaborating dependent (co)pattern matching
- A. Kovacs: Elaboration with first-class implicit function types
- A. Abel, B. Pientka: Higher-Order Dynamic Pattern Unification for Dependent Types and Records

[^0]: masters project at the chair of programming languages @ uni freiburg