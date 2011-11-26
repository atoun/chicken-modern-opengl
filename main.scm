#!/usr/bin/csi -ss

(use gl glu glut glmisc glew tga srfi-1 srfi-4 utils defstruct)

(defstruct resources
  vertex-buffer
  element-buffer
  textures
  uniforms
  attributes
  (fade-factor 0.4)
  (scale-factor 4)
  program)

(defstruct uniforms
  fade-factor
  textures
  scale-factor)

(set! resources-fade-factor
      (getter-with-setter resources-fade-factor resources-fade-factor-set!))
(set! resources-scale-factor
      (getter-with-setter resources-scale-factor resources-scale-factor-set!))

(define (make-uniforms-from-program x)
  (make-uniforms
   fade-factor: (gl:GetUniformLocation x "fade_factor")
   textures: (list (gl:GetUniformLocation x "textures[0]")
                  (gl:GetUniformLocation x "textures[1]"))
   scale-factor: (gl:GetUniformLocation x "scale_factor")))

(define (make-attributes-from-program x)
  (list (gl:GetAttribLocation x "position")))

(define (make-and-init-resources)
  (let ((program (make-program "hello-gl.v.glsl" "hello-gl.f.glsl")))
    (make-resources
     vertex-buffer: (make-buffer gl:ARRAY_BUFFER
                                 '#f32(-1 -1
                                        1 -1
                                       -1  1
                                        1  1))
     element-buffer: (make-buffer gl:ELEMENT_ARRAY_BUFFER '#u16(0 1 2 3))
     textures:
       (let ((res (list (make-texture "hello2.tga")
                        (make-texture "hello1.tga"))))
         (when (any zero? res) (error "Failed to load a texture"))
         res)
     program: program
     uniforms: (make-uniforms-from-program program)
     attributes: (make-attributes-from-program program))))

(define (make-texture filename)
  (let* ((tex-data (with-input-from-file filename read-tga))
         (width (car tex-data))
         (height (cadr tex-data))
         (pixels (caddr tex-data))
         (res (u32vector 0)))
    (gl:GenTextures 1 res)
    (gl:BindTexture gl:TEXTURE_2D (u32vector-ref res 0))
    (gl:TexParameteri gl:TEXTURE_2D gl:TEXTURE_MIN_FILTER gl:LINEAR)
    (gl:TexParameteri gl:TEXTURE_2D gl:TEXTURE_MAG_FILTER gl:LINEAR)
    (gl:TexParameteri gl:TEXTURE_2D gl:TEXTURE_WRAP_S gl:CLAMP_TO_EDGE)
    (gl:TexParameteri gl:TEXTURE_2D gl:TEXTURE_WRAP_T gl:CLAMP_TO_EDGE)
    (gl:TexImage2D
     gl:TEXTURE_2D 0 gl:RGB8 width height 0 gl:BGR
     gl:UNSIGNED_BYTE (blob-pointer (u8vector->blob/shared pixels)))
    (u32vector-ref res 0)))

(define (make-buffer target data)
  (let ((res (u32vector 0)))
    (gl:GenBuffers res)
    (gl:BindBuffer target (u32vector-ref res 0))
    (gl:BufferData target data gl:STATIC_DRAW)
    (u32vector-ref res 0)))

(define (make-shader type data)
  (let ((shader (gl:CreateShader type)))
    (load-shader-source shader data)
    (gl:CompileShader shader)
    (let ((res (u32vector 0)))
      (gl:GetShaderiv shader gl:COMPILE_STATUS res)
      (if (zero? (u32vector-ref res 0))
          (let ((log (get-shader-info-log shader)))
            (gl:DeleteShader shader)
            (error log))
          shader))))

(define (link-program vert fragment)
  (let ((program (gl:CreateProgram)))
    (gl:AttachShader program vert)
    (gl:AttachShader program fragment)
    (gl:LinkProgram program)
    (let ((res (u32vector 0)))
      (gl:GetProgramiv program gl:LINK_STATUS res)
      (if (zero? (u32vector-ref res 0))
          (let ((log (get-program-info-log program)))
            (gl:DeleteProgram program)
            (error log))
          program))))

(define (make-program vert-file frag-file)
  (let ((vert-shader (make-shader
                      gl:VERTEX_SHADER
                      (with-input-from-file vert-file read-all)))
        (frag-shader (make-shader
                      gl:FRAGMENT_SHADER
                      (with-input-from-file frag-file read-all))))
    (when (or (zero? vert-shader) (zero? frag-shader))
      (error "Failed to make a shader"))
    (let ((program (link-program vert-shader frag-shader)))
      (when (zero? program) (error "Failed to make program"))
      program)))

(define (idle-func resources)
  (lambda ()
    (set! (resources-fade-factor resources)
          (+ (* (sin (* 0.001 (glut:Get glut:ELAPSED_TIME))) 0.5) 0.5))
    (glut:PostRedisplay)))

(define (render-func resources)
  (lambda ()
    (gl:UseProgram (resources-program resources))

    (gl:Uniform1f (uniforms-fade-factor (resources-uniforms resources))
                  (resources-fade-factor resources))

    (gl:ActiveTexture gl:TEXTURE0)
    (gl:BindTexture gl:TEXTURE_2D (car (resources-textures resources)))
    (gl:Uniform1i (car (uniforms-textures (resources-uniforms resources))) 0)

    (gl:ActiveTexture gl:TEXTURE1)
    (gl:BindTexture gl:TEXTURE_2D (cadr (resources-textures resources)))
    (gl:Uniform1i (cadr (uniforms-textures (resources-uniforms resources))) 1)

    (gl:BindBuffer gl:ARRAY_BUFFER (resources-vertex-buffer resources))
    (gl:VertexAttribPointer
     (car (resources-attributes resources))
     2
     gl:FLOAT
     gl:FALSE
     (* 2 sizeof_GLfloat)
     #f)
    (gl:EnableVertexAttribArray (car (resources-attributes resources)))

    (gl:BindBuffer gl:ELEMENT_ARRAY_BUFFER (resources-element-buffer resources))

    (gl:DrawElements
     gl:TRIANGLE_STRIP
     4
     gl:UNSIGNED_SHORT
     #f)

    (gl:DisableVertexAttribArray (car (resources-attributes resources)))

    (glut:SwapBuffers)))

(define (main args)
  (glut:InitDisplayMode (bitwise-ior glut:RGB glut:DOUBLE))
  (glut:InitWindowSize 400 300)
  (glut:CreateWindow "Hello World")

  (glew:Init)
  (unless (glew:IsSupported "GL_VERSION_2_0")
    (printf "OpenGL 2.0 not available\n")
    (exit))

  (let ((resources (make-and-init-resources)))
    (glut:IdleFunc (idle-func resources))
    (glut:DisplayFunc (render-func resources)))

  (glut:MainLoop))

;end
