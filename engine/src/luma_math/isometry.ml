type t = {
  rotation : Rot2.t;
  translation : Vec2.t;
}

let create rotation translation = { rotation; translation }
let rotation i = i.rotation
let translation i = i.translation
