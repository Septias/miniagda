# miniagda
rust implementation for a subset of the [agda](https://github.com/agda/agda) programming language

## features

### done
- nothing :)

### implemented
- indexed families of data types

### in progress
- definitions by dependent pattern matching

### planned
- implicit arguments 
- holes

## development

```bash
cargo watch -x 'run --bin miniagda-watch -- path/to/example.agda' --ignore examples
```

## literature

- J. Cockx: Dependent pattern matching and proof-relevant unification
- J. Cockx, A. Abel: Elaborating dependent (co)pattern matching
- A. Kovacs: Elaboration with first-class implicit function types
