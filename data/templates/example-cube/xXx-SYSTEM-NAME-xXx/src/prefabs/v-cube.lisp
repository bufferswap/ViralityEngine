(in-package #:xXx-SYSTEM-NAME-xXx)

(v:define-prefab "v-cube" (:library lib/main)
 (("cube" :copy "/mesh")
   (comp:transform :translate (v3:vec 0f0 0f0 0f0)
                   :rotate/velocity (v3:velocity (v3:ones) o:pi/2))
   (comp:mesh :name "cube")
   (comp:render :material 'v-letter)))
