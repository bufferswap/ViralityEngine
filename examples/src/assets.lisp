(in-package #:virality-examples)

(v:define-asset-pool meshes ()
  :base "data/meshes"
  :type glb)

(v:define-asset-pool textures ()
  :base "data/textures"
  :type png)

(v:define-asset-pool mesh-textures ()
  :base "data/textures/mesh"
  :type png)

(v:define-asset-pool ptp-textures ()
  :base "data/textures/protect-the-planets"
  :type png)

(v:define-asset-pool metadata ()
  :base "data/metadata")
