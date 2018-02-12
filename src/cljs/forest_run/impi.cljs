(ns forest-run.impi
  (:require [impi.core :as impi]))

(defmethod impi/update-prop! :pixi.event/mouse-move [object index _ listener]
  (impi/replace-listener object "mousemove" index listener))

(defmethod impi/update-prop! :pixi.event/pointer-move [object index _ listener]
  (impi/replace-listener object "pointermove" index listener))

(defmethod impi/update-prop! :pixi.event/pointer-down [object index _ listener]
  (impi/replace-listener object "pointerdown" index listener))

(defmethod impi/update-prop! :pixi.event/pointer-up [object index _ listener]
  (impi/replace-listener object "pointerup" index listener))

(defmethod impi/update-prop! :pixi.event/pointer-up-outside [object index _ listener]
  (impi/replace-listener object "pointerupoutside" index listener))

(defmethod impi/update-prop! :card/flipped [object _ _ rotation]
  (.set (.-skew object) 0 (* rotation js/PIXI.DEG_TO_RAD)))

(defmethod impi/update-prop! :pixi.sprite/tint [object _ _ tint]
  (set! (.-tint object) tint))

(defmethod impi/update-prop! :pixi.text/anchor [object _ _ [x y]]
  (.set (.-anchor object) x y))

(defmethod impi/update-prop! :pixi.object/width [object _ _ width]
  (set! (.-width object) width))

(defmethod impi/update-prop! :pixi.object/height [object _ _ height]
  (set! (.-height object) height))

(defmethod impi/update-prop! :pixi.object/pivot [object _ _ [x y]]
  (set! (.-pivot object) (js/PIXI.Point. x y)))
