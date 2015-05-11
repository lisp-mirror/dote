(in-package :level-config)

(define-level
  set seed "1"
  set clouds random (clear cloudy covered)
  set buildings-level (random (0 1 2))
  
  load "radial-mountain-function.lisp"
  
  generate cloud-1               from function pixmap:clouds 
  with parameters (512)
  with tags ("general")
  with normalmap-parameters      (ka 1.0 kd 1.0 ks 1.0 roughness 0.0 shininess 128.0)
  
  generate cloud-2               from function pixmap:clouds 
  with parameters (512)
  with tags ("general")
  with normalmap-parameters      (ka 1.0 kd 1.0 ks 1.0 roughness 0.0 shininess 128.0)
  
  generate cloud-3               from function pixmap:clouds 
  with parameters (512)
  with tags ("general")
  with normalmap-parameters      (ka 1.0 kd 1.0 ks 0.0 roughness 0.0 shininess 128.0)

  generate skydome               from function pixmap:skydome
  with parameters (512)
  with tags ("general")
  with normalmap-parameters      (ka 1.0 kd 1.0 ks 1.0 roughness 0.0 shininess 128.0)
  
  generate smoke-tray            from function pixmap:smoke-tray 
  with parameters (512)
  with tags ("general")
  with normalmap-parameters      (ka 1.0 kd 1.0 ks 1.0 roughness 0.0 shininess 128.0)
  
  generate brick-wall            from function pixmap:brick-wall 
  with parameters (512 128 random (64 65))
  with tags ("wall-level-3")
  with normalmap-parameters      (ka 0.0 kd 1.0 ks 1.0 roughness 0.1 shininess 50.0)
  
  generate dry-stone-wall        from function pixmap:dry-stone-wall
  with parameters (512)
  with tags ("wall-level-2")
  with normalmap-parameters      (ka 0.01 kd 1.0 ks 0.1 roughness 1.0 shininess 10.0) and
  with postprocess pixmap:tileize
  
  generate wood-wall             from function pixmap:wood-wall
  with parameters (512 random (§cca6f38ff §ca83b02ff) random (§ccc6601ff §cffc069ff))
  with tags ("wall-level-1")
  with normalmap-parameters      (ka 0.5 kd 0.5 ks 0.1 roughness 0.0 shininess 128.0)
  
  generate sand                  from function pixmap:sand
  with parameters (512)
  with tags ("shore-terrain")
  with normalmap-parameters      (ka 1.0 kd 1.0 ks 1.0 roughness 0.0 shininess 128.0)
  
  generate grass                 from function pixmap:grass
  with parameters (512)
  with tags ("grass-terrain")
  with normalmap-parameters      (ka 1.0 kd 1.0 ks 1.0 roughness 0.0 shininess 128.0)
  
  generate snow                  from function pixmap:snow
  with parameters (8)
  with tags ("snow-terrain")
  with normalmap-parameters      (ka 1.0 kd 1.0 ks 1.0 roughness 0.0 shininess 128.0)
  
  generate soil                  from function pixmap:soil
  with parameters (512)
  with tags ("soil-level-1" "floor-level-1" "floor-level-2")
  with normalmap-parameters      (ka 0.0 kd 1.0 ks .2 roughness 0.0 shininess 10.0) and
  with postprocess pixmap:tileize
  
  generate rock-1                from function pixmap:rock-1
  with parameters (512)
  with tags ("soil-level-2" "floor-level-3")
  with normalmap-parameters      (ka 0.0 kd 1.0 ks .5 roughness 0.1 shininess 96.0) and
  with postprocess pixmap:tileize

  generate grass-stones-floor from function pixmap:grass-stones-floor
  with parameters (512)  
  with tags ("building-decal")
  with normalmap-parameters      (ka 1.0 kd 1.0 ks 1.0 roughness 0.0 shininess 128.0) and
  with postprocess pixmap:tileize

  generate dry-soil              from function pixmap:dry-soil
  with parameters (512)
  with tags ("soil-decal")
  with normalmap-parameters      (ka 0.0 kd 1.0 ks 0.0 roughness 0.1 shininess 128.0) and
  with postprocess pixmap:tileize

  generate stone-floor-road   from function pixmap:stone-floor-road
  with parameters (256 32 32)
  with tags ("road-decal")
  with normalmap-parameters      (ka 1.0 kd 1.0 ks 1.0 roughness 0.0 shininess 128.0)
  
  generate luxurious-floor-2  from function pixmap:stone-floor-fancy
  with parameters (256)
  with tags ("floor-level-3")
  with normalmap-parameters      (ka .01 kd 1.0 ks 1.0 roughness 0.0 shininess 128.0)
  
  generate decal-wall-1          from function pixmap:blood-splat
  with parameters (256)
  with tags ("int-decal" "ext-decal")
  with normalmap-parameters       (ka 1.0 kd 1.0 ks 0.0 roughness 0.0 shininess 128.0)

  generate blood-splat-2          from function pixmap:blood-splat
  with parameters (256)
  with tags ("int-decal" "ext-decal")
  with normalmap-parameters       (ka 1.0 kd 1.0 ks 0.0 roughness 0.0 shininess 128.0)
  
  generate decal-wall-2          from function pixmap:glass-tile
  with parameters (256)
  with tags ("int-decal")
  with normalmap-parameters      (ka 1.0 kd 1.0 ks 1.0 roughness 0.0 shininess 128.0)

  generate starfish             from function pixmap:starfish
  with parameters (256)
  with tags ("int-decal")
  with normalmap-parameters      (ka 1.0 kd 1.0 ks 1.0 roughness 0.0 shininess 128.0) and
  with postprocess pixmap:voronoize
    
  generate wood-light-color      from function pixmap:wood-2
  with parameters (256)
  with tags ("door-level-1" "door-level-2" "door-level-3")
  with normalmap-parameters      (ka 0.5 kd 1.0 ks 1.0 roughness 0.0 shininess 128.0) and
  with postprocess pixmap:tileize

  generate wood-strained         from function pixmap:wood-3
  with parameters (256)
  with tags ("ceil-level-1" "ceil-level-2" "ceil-level-3")
  with normalmap-parameters      (ka 1.0 kd 1.0 ks .1 roughness 0.0 shininess 256.0) and
  with postprocess pixmap:tileize
  
  generate map 
  ;;with size                        (random (tiny small medium large))
  with size                        (random (small))
  with mountain-rate               (random (0.1 0.2))
  ;; from load "radial-mountain-function.lisp"
  with mountain-z-height-function  radial-mountain-z-height-function
  with mountain-w-function         random-terrain:default-mountain-size-function
  with mountain-h-function         random-terrain:default-mountain-size-function
  with mountain-sigma-w-function   random-terrain:default-mountain-sigma-function
  with mountain-sigma-h-function   random-terrain:default-mountain-sigma-function  
  with lake-rate                   (0.05)
  with lake-size-function          random-terrain:default-lake-size-function
  with labyrinth-rate              (random (0.05 0.1))
  with labyrinth-size-function     random-terrain:default-labyrinth-size-function
  with labyrinth-sigma-w-function  random-terrain:default-labyrinth-sigma-w-function
  with labyrinth-sigma-h-function  random-terrain:default-labyrinth-sigma-h-function
  with labyrinth-door-function     random-terrain:default-labyrinth-door-function
  with labyrinth-win-function      random-terrain:default-labyrinth-win-function
  with soil-decal-threshold        (0.8)
  with trees-rate                  (0.05)
  with trees-sparseness            (0.7)

  generate tree                  from script "general/dead-tree.lsys"
  with normalmap-parameters      (ka 1.0 kd 1.0 ks .1 roughness 0.0 shininess 256.0)

  generate tree                  from script "temperate/fir.lsys"
  with normalmap-parameters      (ka 1.0 kd 1.0 ks .1 roughness 0.0 shininess 256.0)

  ;; obj file format is supported only
  generate tree                  from file   "temperate/pine/pine.obj"
  with normalmap-parameters      (ka 1.0 kd 1.0 ks .1 roughness 0.0 shininess 256.0)

  ;; again, only obj file format
  generate furniture             from file   "fountain/model.obj"
  with normalmap-parameters      (ka .2 kd 1.0 ks 1.0 roughness 0.0 shininess 10.0)

  generate furniture             from file   "chest/model.obj"
  with normalmap-parameters      (ka 1.0 kd 1.0 ks .1 roughness 0.0 shininess 256.0)
  
)



