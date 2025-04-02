## Overall concerns

### Driver
- Overall driver of whole compilation process is needed.

### Config
- Global config of compilation is needed for Overseer module 
to correctly and efficiently work.

### Logging
- switch to `scribe` lib. scala-logging is bs and doesn't support 
few usecases we have for logging. Also configuration of it is sheise.

### Generic logging

### CLI parsing and docs for it

### Syntax of the language
- Formal grammar or some sort of doc is greatly needed

## Backend

### IR
- Write data structure for instructions and basicblocks(holy shit scala collections suck).

### Codegen
- Write machine IR part and overall codegen.

