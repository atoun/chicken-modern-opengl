(import chicken scheme)
(import foreign)
(use data-structures srfi-4)
;(export glew:Init glew:VERSION_2_0 glew:VERSION glew:GetString
;        glew:IsSupported)

(foreign-declare
 "#include <stdlib.h>
  #include <GL/glew.h>")

(define glew:Init (foreign-lambda int glewInit))
(define glew:VERSION_2_0 (foreign-value "GLEW_VERSION_2_0" int))
(define glew:GetString (foreign-lambda c-string glewGetString int))
(define glew:VERSION (foreign-value "GLEW_VERSION" int))
(define glew:IsSupported
  (compose not zero? (foreign-lambda int glewIsSupported c-string)))
