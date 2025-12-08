open Luma__serialize
open Luma__codecs
open Luma__transform
open Luma__sprite
open Luma__app

module Transform_serializers_plugin = struct
  let apply app =
    let packed_transform = Serialize.pack_json (module Codecs.Json.Transform) in
    App.register_component Transform.C.name (module Transform.C) [ packed_transform ] app
end

module Sprite_serializers_plugin = struct
  let apply app =
    let packed_sprite = Serialize.pack_json (module Codecs.Json.Sprite_spec) in
    App.register_component Sprite.Spec_c.name (module Sprite.Spec_c) [ packed_sprite ] app
end

let serializers_plugin app =
  app |> Transform_serializers_plugin.apply |> Sprite_serializers_plugin.apply
