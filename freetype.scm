;;;; freetype.scm -- Freetype 2 interface for Chicken
;;
;; Copyright (c) 2007 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt
(module freetype *
  (import scheme chicken foreign data-structures extras srfi-1 srfi-13 foreigners)
  
  (import-for-syntax matchable)

  (foreign-declare
   "
#include \"ft2build.h\"
#include FT_FREETYPE_H
")

  (define-syntax define-foreign-enum
    (lambda (e r c)
      `(,(r 'begin)
        ,@(map
           (match-lambda
            ((name realname)
             `(,(r 'define) ,name (,(r 'foreign-value) ,(symbol->string realname) ,(r 'int))))
            (name `(,(r 'define) ,name (,(r 'foreign-value) ,(symbol->string name) ,(r 'int)))))
           (cdr e)))))

  (define-syntax define-foreign-record
    (lambda (e r c)
      (let ((rename identity)
            (record-name (if (atom? (cadr e))
                             (cadr e)
                             (caadr e)))
            (foreign-name (if (atom? (cadr e))
                              '()
                              (cadadr e))))
        (for-each
         (lambda (C)
           (if (eq? (car C) 'rename:)
               (set! rename (eval (cadr C)))))
         (cddr e))
        `(,(r 'define-foreign-record-type)
          ,(if (null? foreign-name)
               record-name
               `(,(string->symbol
                   (rename
                    (->string record-name)))
                 ,foreign-name))
          ,@(remove
             null?
             (map
              (match-lambda
               (('rename: func) '())
               (('constructor: . rest) (cons (r 'constructor:) rest))
               (('destructor: . rest) (cons (r 'destructor:) rest))
               ((type slotname) `(,type ,slotname
                                        ,(string->symbol
                                          (rename
                                           (string-append
                                            (->string record-name)
                                            "-"
                                            (->string slotname))))
                                        ,(string->symbol
                                          (rename
                                           (string-append
                                            (->string record-name)
                                            "-"
                                            (->string slotname)
                                            "-set!")))))
               ((type slotname foreignname) `(,type
                                              ,foreignname
                                              ,(string->symbol
                                                (string-append
                                                 (->string record-name)
                                                 "-"
                                                 (->string slotname)))
                                              ,(string->symbol
                                                (string-append
                                                 (->string record-name)
                                                 "-"
                                                 (->string slotname)
                                                 "-set!")))))
              (cddr e)))))))

  (define-foreign-enum
    FT_ENCODING_NONE
    FT_ENCODING_MS_SYMBOL
    FT_ENCODING_UNICODE
    FT_ENCODING_SJIS
    FT_ENCODING_GB2312
    FT_ENCODING_BIG5
    FT_ENCODING_WANSUNG
    FT_ENCODING_JOHAB
    FT_ENCODING_ADOBE_STANDARD
    FT_ENCODING_ADOBE_EXPERT
    FT_ENCODING_ADOBE_CUSTOM
    FT_ENCODING_ADOBE_LATIN_1
    FT_ENCODING_OLD_LATIN_2
    FT_ENCODING_APPLE_ROMAN)

  (define-foreign-enum
    FT_FACE_FLAG_SCALABLE
    FT_FACE_FLAG_FIXED_SIZES
    FT_FACE_FLAG_FIXED_WIDTH
    FT_FACE_FLAG_SFNT
    FT_FACE_FLAG_HORIZONTAL
    FT_FACE_FLAG_VERTICAL
    FT_FACE_FLAG_KERNING
    FT_FACE_FLAG_FAST_GLYPHS
    FT_FACE_FLAG_MULTIPLE_MASTERS
    FT_FACE_FLAG_GLYPH_NAMES
    FT_FACE_FLAG_EXTERNAL_STREAM)

  (define-foreign-enum
    FT_STYLE_FLAG_ITALIC
    FT_STYLE_FLAG_BOLD)

  (define-foreign-enum
    FT_OPEN_MEMORY
    FT_OPEN_STREAM
    FT_OPEN_PATHNAME
    FT_OPEN_DRIVER
    FT_OPEN_PARAMS)

  (define-foreign-enum
    FT_LOAD_DEFAULT
    FT_LOAD_NO_SCALE
    FT_LOAD_NO_HINTING
    FT_LOAD_RENDER
    FT_LOAD_NO_BITMAP
    FT_LOAD_VERTICAL_LAYOUT
    FT_LOAD_FORCE_AUTOHINT
    FT_LOAD_CROP_BITMAP
    FT_LOAD_PEDANTIC
    FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH
    FT_LOAD_NO_RECURSE
    FT_LOAD_IGNORE_TRANSFORM
    FT_LOAD_MONOCHROME
    FT_LOAD_LINEAR_DESIGN
    FT_LOAD_TARGET_NORMAL
    FT_LOAD_TARGET_LIGHT
    FT_LOAD_TARGET_MONO
    FT_LOAD_TARGET_LCD
    FT_LOAD_TARGET_LCD_V)

  (define-foreign-enum
    FT_RENDER_MODE_NORMAL
    FT_RENDER_MODE_LIGHT
    FT_RENDER_MODE_MONO
    FT_RENDER_MODE_LCD
    FT_RENDER_MODE_LCD_V
    FT_RENDER_MODE_MAX)

  (define-foreign-enum
    FT_KERNING_DEFAULT
    FT_KERNING_UNFITTED
    FT_KERNING_UNSCALED)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-foreign-record (ft-glyph-metrics FT_Glyph_Metrics)
    (rename: (compose string-downcase (cut string-translate <> "_" "-")))
    (destructor: free-ft-glyph-metrics)
    (long width)
    (long height)
    (long hori-bearing-x horiBearingX)
    (long hori-bearing-y horiBearingY)
    (long hori-advance horiAdvance)
    (long vert-bearing-x vertBearingX)
    (long vert-bearing-y vertBearingY)
    (long vert-advance vertAdvance)
    )

  (define ft-init-freetype
    (foreign-lambda*
     c-pointer ()
     "FT_Library library;
    if (FT_Init_FreeType(&library)) {
      /* error */
      return(NULL);
    } else {
      return(library);
    }"
     ))

  (define-foreign-record (ft-face FT_FaceRec)
    (rename: (compose string-downcase (cut string-translate <> "_" "-")))
    (destructor: free-ft-face)
    (long num_faces)
    (long face_index)
    (long face_flags)
    (long style_flags)
    (long num_glyphs)
    (c-string family_name)
    (c-string style_name)
    (int num_fixed_sizes)
    (c-pointer available_sizes)
    (int num_charmaps)
    (c-pointer charmaps)
    (unsigned-short units_per_EM)
    (short ascender)
    (short descender)
    (short height)
    (short max_advance_width)
    (short max_advance_height)
    (short underline_position)
    (short underline_thickness)
    (c-pointer glyph)
    (c-pointer size)
    (c-pointer charmap)
    )

  (define ft-new-face
    (foreign-lambda*
     c-pointer ((c-pointer library) (c-string file))
     "FT_Face face;
    if (FT_New_Face(library, file, 0, &face)) {
      /* error */
      return(NULL);
    } else {
      return(face);
    }"
     ))

  (define ft-set-char-size
    (foreign-lambda int "FT_Set_Char_Size" c-pointer int int int int))

  (define ft-set-pixel-sizes
    (foreign-lambda int "FT_Set_Pixel_Sizes" c-pointer int int))

  (define ft-get-char-index
    (foreign-lambda unsigned-int "FT_Get_Char_Index" c-pointer int))

  (define ft-load-glyph
    (foreign-lambda int "FT_Load_Glyph" c-pointer int int))

  (define ft-load-char
    (foreign-lambda int "FT_Load_Char" c-pointer unsigned-long int))

  (define ft-render-glyph
    (foreign-lambda int "FT_Render_Glyph" c-pointer int))

  (define (ft-load-glyph face ch load-options)
    (let ((i (ft-get-char-index face (if (char? ch) (char->integer ch) ch))))
      (and (not (zero? i))
           (ft-load-glyph face i load-options))))

  (define-foreign-record (ft-glyph-slot FT_GlyphSlotRec)
    (rename: (compose string-downcase (cut string-translate <> "_" "-")))
    (destructor: free-ft-glyph-slot)
    (c-pointer library)
    (c-pointer face)
    (c-pointer next)
    (unsigned-int reserved)
    (long linear_hori_advance linearHoriAdvance)
    (long linear_vert_advance linearVertAdvance)
    (int format)
    (int bitmap_left)
    (int bitmap_top)
    (unsigned-int num_subglyphs)
    (c-pointer subglyphs)
    (c-pointer control_data)
    (long control_len)
    (long lsb_delta)
    (long rsb_delta)
    (c-pointer other)
    )

  (define ft-glyph-slot-advance-x
    (foreign-lambda*
      integer ((c-pointer ptr))
      "FT_GlyphSlot glyph = (FT_GlyphSlot) ptr;
      C_return(glyph->advance.x >> 6);"))

  (define ft-glyph-slot-advance-y
    (foreign-lambda*
      integer ((c-pointer ptr))
      "FT_GlyphSlot glyph = (FT_GlyphSlot) ptr;
      C_return(glyph->advance.y >> 6);"))
    
  (define ft-glyph-slot-metrics
    (foreign-lambda*
     c-pointer ((c-pointer ptr))
     "FT_GlyphSlot glyph = (FT_GlyphSlot) ptr;
    return(&(glyph->metrics));
   "))

  (define ft-glyph-slot-bitmap
    (foreign-lambda*
     c-pointer ((c-pointer ptr))
     "FT_GlyphSlot glyph = (FT_GlyphSlot) ptr;
    return(&(glyph->bitmap));
   "))

  (define-foreign-record FT_Vector
    (rename: (compose string-downcase (cut string-translate <> "_" "-")))
    (constructor: make-ft-vector)
    (destructor: free-ft-vector)
    (long x)
    (long y))

  (define-foreign-record FT_Matrix
    (rename: (compose string-downcase (cut string-translate <> "_" "-")))
    (constructor: make-ft-matrix)
    (destructor: free-ft-matrix)
    (long xx)
    (long xy)
    (long yx)
    (long yy))

  (define ft-has-kerning?
    (foreign-lambda
     bool "FT_HAS_KERNING"
     (c-pointer "FT_FaceRec")))

  (define ft-get-kerning
    (foreign-lambda
     int "FT_Get_Kerning"
     c-pointer unsigned-int unsigned-int unsigned-int c-pointer))

  (define ft-select-charmap
    (foreign-lambda
     int "FT_Select_Charmap"
     c-pointer int))

  (define ft-set-transform
    (foreign-lambda
     void "FT_Set_Transform"
     c-pointer c-pointer c-pointer))

  (define-foreign-record FT_Bitmap
    (rename: (compose string-downcase (cut string-translate <> "_" "-")))
    (destructor: free-ft-bitmap)
    (int rows)
    (int width)
    (int pitch)
    (c-pointer buffer)
    (short num_grays)
    (char pixel_mode)
    (char palette_mode)
    (c-pointer palette)
    )

  )
