# Luma

Luma is an unnecessary but fun ECS-based game engine inspired by Bevy, written in OCaml, and built (mostly) on top of Raylib. 

It has been built out of a curiosity about game and game engine development, as well as a way to explore functional programming in OCaml. 

```OCaml
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

Very unfinished. Very unlikely to ever be "finished". Development has slowed right down due to life, and to be honest, waning interest. It will continue but only when random surges of motivation hit. 

## Installation 

### Prerequisites 

- OCaml and opam
- Raylib

### Steps 

- clone the repo and navigate to the project directory 

```bash
# create local switch
opam switch create . 5.4.0

# load environment 
eval $(opam env)

# install dependencies 
opam install . --deps-only --with-test --with-doc

# build 
dune build 
```

## Importing the library

Luma can be imported using the following steps 

1. Initialising a fresh project 

```bash
# create a fresh project if needed
dune init proj my_game
cd my_game

# create an opam switch if needed. Adjust the version number 
opam switch create . 5.4.0
eval $(opam env)
```

 2. Pin luma to the project by adding this to the dune-project file

```
(pin
 (url "/path/to/luma/engine")
 (package (name luma)))
```

3. In the same file, add luma to the depends stanza

```
(package
 (name my_game)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends luma ocaml)
 (tags
  ("add topics" "to describe" your project)))
```

4. Update and lock the pinned version

```bash
dune build     # updates the opam file
dune pkg lock  # locks the pinned engine version
dune build     # full rebuild
```

The library is now ready to use!

## Getting started

### 1. Initialise the Raylib driver 

At some point I decided to try and make the engine "backend agnostic", to target different platforms and to tinker with different libraries. There is a very rough JavaScript driver for targetting the web, but realistically the Raylib driver is the only usable one. Add the dependency and rebuild: 


```
(executable
 (public_name game)
 (name main)
 (libraries luma luma.driver.raylib))
```


### 3. Instantiate the engine 

The library uses a functor to initialise the engine with the desired driver. Use the provided Raylib driver or make your own! It just needs to satisfy the driver signature. 

```OCaml
module Luma = Luma.Make(Luma_driver_raylib.Driver)
open Luma
```

### 4. Running a minimal game

```OCaml
let () =
  App.create ()
  |> Plugin.add_default_plugins
  |> App.run
```

Default plugins are currently required. They initialise necessary components/systems like the window, input, time, transform, rendering, and probably other stuff that I've forgotten but will crash your game if absent. 

## Roadmap

There is none. I would love to build this until it's "stable" but that seems very unlikely. Adding tests and better docs would be great, but I'd rather eventually try to build an actual game. Or move on to the next thing! 
