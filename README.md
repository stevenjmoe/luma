# Luma

Luma is an unnecessary but fun ECS-based game engine written in OCaml, built on top of Raylib.
It exists mostly because writing a game engine is a great way to learn language design, ECS architecture, and engine internals. It does not exist because the world needs another one.

```ocaml
let () =
  App.create ()
  |> Plugin.add_default_plugins
  |> App.add_plugin my_plugin
  |> App.on Startup (setup_player ())
  |> App.on Startup (setup_camera ())
  |> App.on Update (input_system ())
  |> App.run
```

## Status

Luma is unfinished, unstable, and changing frequently.
It lacks documentation, features, tests, and general polish.

This is a learning project, experimental by design, and not intended to reach production quality.

## About

### Luma is:

- A toy/experimental ECS engine inspired by Bevy.
- A place to explore engine architecture in OCaml.
- An engine that can target Raylib and (very experimentally) JavaScript.

### Luma isn't:

- A general-purpose, battle-tested game engine.
- A stable API.

## Installation (Local Development / Curiosity Use)

Luma isn’t published on opam yet. These instructions are meant for people who want to play with the engine source directly.

### Prerequisites

- OCaml + opam installed.
- Raylib development libraries (depending on your system).

### Steps

- Clone the repository and cd into the engine directory.
- Create a local opam switch and install dependencies:

```sh
opam switch create . 5.4.0
```

On some systems this may complain or appear partially successful; pressing n to continue is typically fine for now.

- Load the environment:

```sh
eval $(opam env)
```

- Install build dependencies:

```sh
opam install . --deps-only --with-test --with-doc
```

- Build the engine:
```
dune build
```

## Using Luma in a Separate Game Project

If you want to create a small test project that depends on your local Luma:

- Create a fresh project somewhere else:

```sh
dune init proj my_game
cd my_game
```

- Create a new switch here as well (optional but keeps environments isolated):

```sh
opam switch create . 5.4.0
eval $(opam env)
```

- Edit your dune-project and add a local pin:

```dune-project
(pin
 (url "/path/to/luma/engine")
 (package (name luma)))
```

Then add luma to your package → depends stanza. Example:

```dune-project
(package
 (name my_game)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends luma ocaml)
 (tags
  ("add topics" "to describe" your project)))
```

- Update and lock the pinned version:

```sh
dune build     # updates the opam file
dune pkg lock  # locks the pinned engine version
dune build     # full rebuild
```
Luma should now be available as a library.

## Getting Started
### 1. Pick a Backend Driver

Luma is designed to be backend-agnostic in principle, though in practice the Raylib driver is the only one that currently works reliably.

#### Included drivers:

- **Raylib driver:** uses the raylib-ocaml bindings.
- **JavaScript/Browser driver:** experimental, incomplete, and absolutely not recommended unless you're hacking on the engine itself.

### 2. Add the Library to Your dune File

```dune
(executable
 (public_name game)
 (name main)
 (libraries luma luma.driver.raylib))
```

Then rebuild.

### 3. Instantiate the Engine

Inside your OCaml source:

```ocaml
module Luma = Luma.Make(Luma_driver_raylib.Driver)
open Luma
```

`Make` packages the entire engine API (App, World, components, queries, scheduling, plugins, etc.) into a single backend-specialized module. Everything you do with the engine happens via this module.

### 4. Running a Minimal Game

```ocaml
let () =
  App.create ()
  |> Plugin.add_default_plugins
  |> App.run
```

*Note:* Default plugins register essential engine systems (window creation, event loop, time, transforms, rendering, input). Without them, the engine will not run.

## Roadmap (Loose)

- Better documentation
- Better examples
- Better debugging/introspection tools
- More tests
- Less boilerplate around components & resources
- Improved JS backend
- An editor
