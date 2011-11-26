(import chicken scheme data-structures)
(use srfi-4)

(define +tga-header-len+ 18)

(define (tga-header-data-type-code x) (u8vector-ref x 2))

(define (tga-header-bits-per-pixel x) (u8vector-ref x 16))

(define (tga-header-id-length x)
  (s8vector-ref (blob->s8vector/shared (u8vector->blob/shared x)) 0))

(define u8vector->s8vector/shared
  (compose blob->s8vector/shared u8vector->blob/shared))

(define (le-short ary start)
  (bitwise-ior (u8vector-ref ary start)
               (arithmetic-shift
                (s8vector-ref (u8vector->s8vector/shared ary) (add1 start)) 8)))

(define (tga-header-color-map-length x) (le-short x 5))
(define (tga-header-color-map-depth x)
  (s8vector-ref (u8vector->s8vector/shared x) 7))

(define (tga-header-color-map-size x)
  (* (tga-header-color-map-length x)
     (/ (tga-header-color-map-depth x) 8)))

(define (tga-header-width x) (le-short x 12))
(define (tga-header-height x) (le-short x 14))

(define (tga-header-pixels-size x)
  (* (tga-header-width x)
     (tga-header-height x)
     (/ (tga-header-bits-per-pixel x) 8)))

;; reads from current input
;; should return (width height pixels) or throw an error
(define (read-tga)
  (let ((header (read-u8vector +tga-header-len+)))
    (unless (= (u8vector-length header) +tga-header-len+)
      (error "Failed to read tga header"))
    (unless (= (tga-header-data-type-code header) 2)
      (error "Not an uncompressed RGB tga file" header))
    (unless (= (tga-header-bits-per-pixel header) 24)
      (error "Not a 24-bit uncompressed RGB tga file" header))
    (unless (= (u8vector-length (read-u8vector (tga-header-id-length header)))
               (tga-header-id-length header))
      (error "Image has incomplete id string" header))
    (unless (= (u8vector-length
                (read-u8vector (tga-header-color-map-size header)))
               (tga-header-color-map-size header))
      (error "Image has incomplete color-map" header))
    (let ((res (read-u8vector (tga-header-pixels-size header))))
      (unless (= (u8vector-length res) (tga-header-pixels-size header))
        (error "Incomplete image" header))
      (list (tga-header-width header)
            (tga-header-height header)
            res))))
