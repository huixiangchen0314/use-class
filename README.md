## use-class

A Clojure library for easily creating protocols and adapters from Java classes—reuse Java implementations without boilerplate.

### Features

- **`define-class`** – Generate a Clojure protocol from a Java interface.
- **`use-class`** – Adapt a Java class to an existing protocol, with options for method filtering, renaming, custom implementations, delegate-through via, and automatic naming conventions.
- **Pipeline API** – Composeable functions (`analyze-class`, `via-methods`, `filter-methods`, `rename-methods`, `wrap-results`, etc.) for fine-grained control.
- **Auto unwrapping** – Built-in conversion for `Optional` → `nil`, extensible via `register-result-converter!`.
- **Argument conversion** – Transform parameters between Clojure maps and Java objects.
- **REPL-friendly** – `inspect-java-class` to see method mappings.

### Quick Start

```clojure
(require '[top.kzre.use-class.core :as uc])

;; 1. Generate a protocol from a Java interface
(uc/define-class com.example.WorldFactory WorldFactoryProtocol
  :except [getClass])   ;; exclude Object methods

;; 2. Adapt a class to an existing protocol
(uc/use-class com.example.WorldInstance IWorld
  :except [getClass]
  :rename {getWorldId world-id}
  :via [[time-millis getGameWorld]         ;; delegate to GameWorld
        [find-entity getEntityManager :findByEntityId]]
  :custom {player-positions (fn [this] ...)}
  :dangerous #{destroy remove})            ;; methods get a ! suffix

;; 3. Inspect a class to see the generated method names
(uc/inspect-java-class 'com.example.Entity)
```

### Pipeline example

For complex adaptations, combine functions:

```clojure
(let [methods (-> (uc/analyze-class 'MyClass)
                  (uc/via-methods :getHelper [[:helper-method]])
                  (uc/filter-methods :except ['getClass])
                  (uc/rename-methods {'oldName 'newName})
                  uc/auto-wrap-results)]
  (uc/extend-by-methods* MyClass MyProtocol methods))   ;; runtime extension
```

### Naming conventions

- `getXxx()` → `xxx`
- `setXxx()` → `set-xxx!`
- `isXxx()`  → `xxx?`
- Other camelCase → kebab-case

### Installation

Add the dependency to your `deps.edn` or `project.clj` (replace `VERSION` with the latest).

### License

MIT (or your preferred license)