(import chicken scheme)
(import foreign)
(use data-structures srfi-4 gl)

(foreign-declare
 "#include <GL/glew.h>
#include <GL/gl.h>")

(define gl:GenBuffers
  (let ()
    (define func (foreign-lambda void glGenBuffers int scheme-pointer))
    (lambda (x)
      (func (u32vector-length x) (u32vector->blob/shared x)))))

(define gl:BindBuffer
  (foreign-lambda void glBindBuffer unsigned-int unsigned-int))

(define gl:STATIC_DRAW (foreign-value "GL_STATIC_DRAW" int))

(define gl:ARRAY_BUFFER (foreign-value "GL_ARRAY_BUFFER" int))
(define gl:ELEMENT_ARRAY_BUFFER (foreign-value "GL_ELEMENT_ARRAY_BUFFER" int))
(define gl:PIXEL_PACK_BUFFER (foreign-value "GL_PIXEL_PACK_BUFFER" int))
(define gl:PIXEL_UNPACK_BUFFER (foreign-value "GL_PIXEL_UNPACK_BUFFER" int))

(define gl:BufferData
  (let ()
    (define (->blob/shared x)
      (cond
       ((u8vector? x) (u8vector->blob/shared x))
       ((s8vector? x) (s8vector->blob/shared x))
       ((u16vector? x) (u16vector->blob/shared x))
       ((s16vector? x) (s16vector->blob/shared x))
       ((u32vector? x) (u32vector->blob/shared x))
       ((s32vector? x) (s32vector->blob/shared x))
       ((f32vector? x) (f32vector->blob/shared x))
       ((f64vector? x) (f64vector->blob/shared x))
       (else (error "Don't know how to convert this" x))))
    (define (size-as-blob x)
      (blob-size (->blob/shared x)))
    (define func (foreign-lambda void glBufferData
                                 unsigned-int unsigned-int
                                 scheme-pointer unsigned-int))
    (lambda (target data usage)
      (func target (size-as-blob data) (->blob/shared data) usage))))

(define blob-pointer (foreign-lambda* c-pointer ((scheme-pointer x))
                                      "C_return(x);"))

(define gl:CreateShader
  (foreign-lambda unsigned-int glCreateShader unsigned-int))

(define gl:CompileShader
  (foreign-lambda void glCompileShader unsigned-int))

(define gl:GetShaderiv
  (foreign-lambda void glGetShaderiv unsigned-int unsigned-int u32vector))

(define gl:DeleteShader
  (foreign-lambda void glDeleteShader unsigned-int))

(define load-shader-source
  (let ()
    (define func
      (foreign-lambda* void
                       ((unsigned-int shader) (scheme-pointer data) (int len))
                       "glShaderSource(shader, 1, &data, &len);"))
    (lambda (shader source)
      (func shader source (blob-size (string->blob source))))))

(define gl:COMPILE_STATUS (foreign-value "GL_COMPILE_STATUS" int))
(define gl:INFO_LOG_LENGTH (foreign-value "GL_INFO_LOG_LENGTH" int))

(define gl:GetShaderInfoLog
  (foreign-lambda void glGetShaderInfoLog
                  unsigned-int unsigned-int scheme-pointer scheme-pointer))

(define (get-shader-info-log-len shader)
  (let ((res (u32vector 0)))
    (gl:GetShaderiv shader gl:INFO_LOG_LENGTH res)
    (u32vector-ref res 0)))

(define (get-shader-info-log shader)
  (let* ((len (get-shader-info-log-len shader))
         (res (make-string len)))
    (gl:GetShaderInfoLog shader len #f res)
    res))

(define gl:CreateProgram (foreign-lambda unsigned-int glCreateProgram))
(define gl:AttachShader
  (foreign-lambda void glAttachShader unsigned-int unsigned-int))
(define gl:LinkProgram (foreign-lambda void glLinkProgram unsigned-int))
(define gl:DeleteProgram (foreign-lambda void glDeleteProgram unsigned-int))

(define gl:GetProgramiv
  (foreign-lambda void glGetProgramiv unsigned-int unsigned-int u32vector))

(define gl:LINK_STATUS (foreign-value "GL_LINK_STATUS" int))
(define gl:VERTEX_SHADER (foreign-value "GL_VERTEX_SHADER" int))
(define gl:FRAGMENT_SHADER (foreign-value "GL_FRAGMENT_SHADER" int))

(define (get-program-info-log-len shader)
  (let ((res (u32vector 0)))
    (gl:GetProgramiv shader gl:INFO_LOG_LENGTH res)
    (u32vector-ref res 0)))

(define gl:GetProgramInfoLog
  (foreign-lambda void glGetProgramInfoLog
                  unsigned-int unsigned-int scheme-pointer scheme-pointer))

(define (get-program-info-log shader)
  (let* ((len (get-program-info-log-len shader))
         (res (make-string len)))
    (gl:GetProgramInfoLog shader len #f res)
    res))

(define gl:GetUniformLocation
  (foreign-lambda int glGetUniformLocation unsigned-int c-string))
(define gl:GetAttribLocation
  (foreign-lambda int glGetAttribLocation unsigned-int c-string))

(define gl:UseProgram (foreign-lambda void glUseProgram unsigned-integer))

(define gl:Uniform1f (foreign-lambda void glUniform1f int float))
(define gl:Uniform1i (foreign-lambda void glUniform1f int int))

(define gl:GetUniformfv
  (foreign-lambda void glGetUniformfv unsigned-int int scheme-pointer))
(define gl:GetUniformiv
  (foreign-lambda void glGetUniformiv unsigned-int int scheme-pointer))

(define uniform-iv
  (let ()
    (foreign-lambda* int ((int program) (int location))
                     "int res;
                      glGetUniformiv(program, location, &res);
                      C_return(res);")))

(define uniform-fv
  (let ()
    (foreign-lambda* float ((int program) (int location))
                     "float res;
                      glGetUniformfv(program, location, &res);
                      C_return(res);")))

(define uniform-iv-set!
  (let ()
    (foreign-lambda* void ((int program) (int location) (int val))
                     "glUseProgram(program);
                      glUniform1i(location, val);")))

(define uniform-fv-set!
  (let ()
    (foreign-lambda* void ((int program) (int location) (float val))
                     "glUseProgram(program);
                      glUniform1f(location, val);")))

(set! uniform-iv (getter-with-setter uniform-iv uniform-iv-set!))
(set! uniform-fv (getter-with-setter uniform-fv uniform-fv-set!))

(define gl:VertexAttribPointer
  (foreign-lambda void glVertexAttribPointer
                  unsigned-int int unsigned-int int unsigned-int c-pointer))

(define sizeof_GLfloat (foreign-value "sizeof(GLfloat)" int))

(define gl:EnableVertexAttribArray
  (foreign-lambda void glEnableVertexAttribArray unsigned-int))
(define gl:DisableVertexAttribArray
  (foreign-lambda void glDisableVertexAttribArray unsigned-int))

(define gl:Uniform1f
  (foreign-lambda* void ((int uniform) (float val))
                   "glUniform1f(uniform, val);"))
(define gl:Uniform1i
  (foreign-lambda* void ((int uniform) (int val))
                   "glUniform1i(uniform, val);"))

;; end
