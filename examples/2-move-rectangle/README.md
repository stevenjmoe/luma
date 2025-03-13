## 1-move rectangle

This will build upon the last example and demonstrate how to add `Update` systems to the world, how to `Query` for entities with specific components, and how to access global `Resources`.

In the previous example, `setup_rectangle` was an expression that accepts a unit and returns a value of type `(Luma.World.t, unit) Luma.System.without_resource`. In this example, some new `Update` systems will be added, some of which will require `Resources` and, unlike the `setup_rectangle` function, will query for both components and resources.

The `System` module provides two helper functions for making a system. 

- `make` accepts an optional `Query.Filter.t`, a `Query.t`, and a `run_fn` which is the function that will be run on the provided schedule (update or startup).
- `make_with_resources` accepts the same params, with the addition of a `Resource.Resource_query.t` param.

Systems with resources will have an extra tuple of resources available to them in the `run_fn`. 

In the first example when making the `setup_rectangle` system, an empty query was provided: `Luma.Query.(End)`. In our new systems, we will query for specific entities that contain the passed in components.

```ocaml
Luma.Query.(
      Required (module Rectangle.C)
      & Required (module Position.C)
      & Required (module Luma.Camera.C)
      & End)
```

`Query.term` is either `Required` or `Optional`. When the query is evaluated, the entities tuple will contain all the required components, or an option.

The avove query will produce a list of tuples that looks like this:

```ocaml
 ((_ : Luma.Id.Entity.t), ((rect : Raylib.Rectangle.t), ((velocity : Vector2.t), ((camera : Camera2D.t), (_ : unit)))))
```

Which looks a bit confusing at first, but isn't too bad when you remove the type annotations.

```ocaml 
(_, (rect, (velocity, (camera, _))))
```

The `input_function` will demonstrate both the optional filter and resources:

```ocaml
let input_system () =
  Luma.System.make_with_resources
    ?filter:(Some Luma.Query.Filter.(With Player_tag.C.id))
    Luma.Query.(Required (module Velocity.C) & End)
    Luma.Reource.Resource_query.(Resource (module Luma.Time.R) & End)
    (fun world entities (time, _) ->
      let open Raylib in
      entities
      |> List.iter (fun (_, (velocity, _)) ->
             let dt = time.dt in
             let vx =
               if is_key_down Key.A then
                 Vector2.x velocity -. (10. *. dt)
               else if is_key_down Key.D then
                 Vector2.x velocity +. (10. *. dt)
               else
                 0.
             in

             let vy =
               if is_key_down Key.W then
                 Vector2.y velocity -. (10. *. dt)
               else if is_key_down Key.S then
                 Vector2.y velocity +. (10. *. dt)
               else
                 0.
             in
             Vector2.set_x velocity vx;
             Vector2.set_y velocity vy;
             ());
      world)

```

It filters entities on a `Player_tag` component, which isn't returned in the tuple, but the entity is guaranteed to contain it.

It also returns a resource tuple with a single resource, `Time.t`. This is a resource provided by the engine that is accessible to any system that needs it. The API for accessing global resources is very likely to change.

And the entry point with the newly created systems looks like this:

```ocaml
let () =
  Luma.create ()
  |> Luma.add_system (Luma.Scheduler.Startup (Luma.System.WithoutResources (setup_rectangle ())))
  |> Luma.add_system
       (Luma.Scheduler.Startup (Luma.System.WithoutResources (setup_other_rectangle ())))
  |> Luma.add_system (Luma.Scheduler.Update (Luma.System.WithResources (input_system ())))
  |> Luma.add_system (Luma.Scheduler.Update (Luma.System.WithResources (movement_system ())))
  |> Luma.add_system (Luma.Scheduler.Update (Luma.System.WithoutResources (render_system ())))
  |> Luma.run
```
