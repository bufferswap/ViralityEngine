(in-package #:vumbra.common)

(defstruct mesh-attrs
  (mesh/pos :vec3)
  (mesh/normal :vec3)
  (mesh/tangent :vec4)
  (mesh/color :vec4)
  (mesh/uv1 :vec2)
  (mesh/uv2 :vec2)
  (mesh/joints :vec4)
  (mesh/weights :vec4))
