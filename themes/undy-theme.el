;;; undy-theme.el --- A fruity color theme for Emacs.

;; Copyright (C) 2011-2016

;; Author: Kelvin Smith <oneKelvinSmith@gmail.com>
;; URL: http://github.com/oneKelvinSmith/undy-emacs
;; Version: 3.5.3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of the popular Textmate theme Undy for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.
;;
;;; Credits:
;;
;; Wimer Hazenberg created the original theme.
;; - http://www.undy.nl/blog/2006/07/15/textmate-color-theme/
;;
;; Bozhidar Batsov created zenburn-theme.el and solarized-theme.el
;;  on which this file is based.
;; - https://github.com/bbatsov/zenburn-emacs
;;
;; Color Scheme Designer 3 for complementary colours.
;; - http://colorschemedesigner.com/
;;
;; Xterm 256 Color Chart
;; - https://upload.wikimedia.org/wikipedia/en/1/15/Xterm_256color_chart.svg
;;
;; K. Adam Christensen for his personal undy theme that addresses 256 colours.
;; - https://github.com/pope/personal/blob/master/etc/emacs.d/undy-theme.el
;;
;; Thomas Frössman for his work on solarized-emacs.
;; - http://github.com/bbatsov/solarized-emacs
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "The undy theme requires Emacs 24 or later!"))

(deftheme undy "The Undy colour theme")

(defgroup undy nil
  "Undy theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom undy-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'undy)

(defcustom undy-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'undy)

(defcustom undy-doc-face-as-comment t
  "Consider `font-lock-doc-face' as comment instead of a string."
  :type 'boolean
  :group 'undy
  :package-version "3.5.1")

(defcustom undy-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'undy)

(defcustom undy-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'undy)

(defcustom undy-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'undy)

(defcustom undy-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'undy)

(defcustom undy-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'undy)

;; Primary colors
(defcustom undy-yellow "#8BA930"
  "Primary colors - yellow"
  :type 'string
  :group 'undy)

(defcustom undy-orange "#FD971F"
  "Primary colors - orange"
  :type 'string
  :group 'undy)

(defcustom undy-red "#C07C49"
  "Primary colors - red"
  :type 'string
  :group 'undy)

(defcustom undy-magenta "#FD5FF0"
  "Primary colors - magenta"
  :type 'string
  :group 'undy)

(defcustom undy-blue "#F1C438"
  "Primary colors - blue"
  :type 'string
  :group 'undy)

(defcustom undy-green "#80FF00"
  "Primary colors - green"
  :type 'string
  :group 'undy)

(defcustom undy-cyan "#A1EFE4"
  "Primary colors - cyan"
  :type 'string
  :group 'undy)

(defcustom undy-violet "#357A8F"
  "Primary colors - violet"
  :type 'string
  :group 'undy)

(defcustom undy-gray "#64645E"
  "Primary colors - gray"
  :type 'string
  :group 'undy)

(defcustom undy-foreground "#F8F8F2"
  "Adaptive colors - foreground"
  :type 'string
  :group 'undy)

(defcustom undy-background "#272822"
  "Adaptive colors - background"
  :type 'string
  :group 'undy)

(defcustom undy-comments "#75715E"
  "Adaptive colors - comments"
  :type 'string
  :group 'undy)

(defcustom undy-emphasis "#F8F8F0"
  "Adaptive colors - emphasis"
  :type 'string
  :group 'undy)

(defcustom undy-line-number "#8F908A"
  "Adaptive colors - line number"
  :type 'string
  :group 'undy)

(defcustom undy-highlight "#49483E"
  "Adaptive colors - highlight"
  :type 'string
  :group 'undy)

(defcustom undy-highlight-alt "#3E3D31"
  "Adaptive colors - highlight"
  :type 'string
  :group 'undy)

(defcustom undy-highlight-line "#3C3D37"
  "Adaptive colors - line highlight"
  :type 'string
  :group 'undy)

(let* (;; Variable pitch
       (undy-pitch (if undy-use-variable-pitch
                          'variable-pitch
                        'default))

       ;; Definitions for guis that support 256 colors
       (undy-class '((class color) (min-colors 257)))

       ;; Darker and lighter accented colors
       (undy-yellow-d       "#BEB244")
       (undy-yellow-l       "#FFF7A8")
       (undy-orange-d       "#D47402")
       (undy-orange-l       "#FFAC4A")
       (undy-red-d          "#F70057")
       (undy-red-l          "#FA518D")
       (undy-magenta-d      "#FB35EA")
       (undy-magenta-l      "#FE8CF4")
       (undy-violet-d       "#945AFF")
       (undy-violet-l       "#C9ACFF")
       (undy-blue-d         "#40CAE4")
       (undy-blue-l         "#92E7F7")
       (undy-cyan-d         "#74DBCD")
       (undy-cyan-l         "#D3FBF6")
       (undy-green-d        "#86C30D")
       (undy-green-l        "#BBEF53")
       (undy-gray-d         "#35331D")
       (undy-gray-l         "#7B7962")
       ;; Adaptive higher/lower contrast accented colors
       (undy-foreground-hc  "#141414")
       (undy-foreground-lc  "#171A0B")
       ;; High contrast colors
       (undy-yellow-hc      "#FFFACE")
       (undy-yellow-lc      "#9A8F21")
       (undy-orange-hc      "#FFBE74")
       (undy-orange-lc      "#A75B00")
       (undy-red-hc         "#FEB0CC")
       (undy-red-lc         "#F20055")
       (undy-magenta-hc     "#FEC6F9")
       (undy-magenta-lc     "#F309DF")
       (undy-violet-hc      "#F0E7FF")
       (undy-violet-lc      "#7830FC")
       (undy-blue-hc        "#CAF5FD")
       (undy-blue-lc        "#1DB4D0")
       (undy-cyan-hc        "#D3FBF6")
       (undy-cyan-lc        "#4BBEAE")
       (undy-green-hc       "#CCF47C")
       (undy-green-lc       "#679A01")

       ;; Distinct fringe
       (undy-fringe-bg (if undy-distinct-fringe-background
                              undy-gray
                            undy-background))

       ;; Definitions for terminals that do not support 256 colors
       (undy-256-class '((class color) (min-colors 89)))
       ;; Primary colors
       (undy-256-yellow         "#CDC673")
       (undy-256-orange         "#FF8C00")
       (undy-256-red            "#FF1493")
       (undy-256-magenta        "#D700D7")
       (undy-256-violet         "#AF87FF")
       (undy-256-blue           "#5FD7FF")
       (undy-256-cyan           "#5FFFFF")
       (undy-256-green          "#87D700")
       (undy-256-gray           "#3D3D3D")
       ;; Darker and lighter accented colors
       (undy-256-yellow-d       "#878700")
       (undy-256-yellow-l       "#FFFF87")
       (undy-256-orange-d       "#AF5F00")
       (undy-256-orange-l       "#FFAF5F")
       (undy-256-red-d          "#870000")
       (undy-256-red-l          "#FF5F87")
       (undy-256-magenta-d      "#AF0087")
       (undy-256-magenta-l      "#FF87DF")
       (undy-256-violet-d       "#5F00AF")
       (undy-256-violet-l       "#AF87D7")
       (undy-256-blue-d         "#008787")
       (undy-256-blue-l         "#87D7FF")
       (undy-256-cyan-d         "#5FAFAF")
       (undy-256-cyan-l         "#AFFFFF")
       (undy-256-green-d        "#5F8700")
       (undy-256-green-l        "#AFD700")
       (undy-256-gray-d         "#333333")
       (undy-256-gray-l         "#707070")
       ;; Adaptive colors
       (undy-256-foreground     "#F5F5F5")
       (undy-256-background     "#1B1E1C")
       (undy-256-comments       "#8B8878")
       (undy-256-emphasis       "#FFFAFA")
       (undy-256-line-number    "#8F908A")
       (undy-256-highlight      "#474747")
       (undy-256-highlight-alt  "#3E3E3E")
       (undy-256-highlight-line "#000000")
       ;; Adaptive higher/lower contrast accented colors
       (undy-256-foreground-hc  "#171A0B")
       (undy-256-foreground-lc  "#141414")
       ;; High contrast colors
       (undy-256-yellow-hc      undy-256-yellow-d)
       (undy-256-yellow-lc      undy-256-yellow-l)
       (undy-256-orange-hc      undy-256-orange-d)
       (undy-256-orange-lc      undy-256-orange-l)
       (undy-256-red-hc         undy-256-red-d)
       (undy-256-red-lc         undy-256-red-l)
       (undy-256-magenta-hc     undy-256-magenta-d)
       (undy-256-magenta-lc     undy-256-magenta-l)
       (undy-256-violet-hc      undy-256-violet-d)
       (undy-256-violet-lc      undy-256-violet-l)
       (undy-256-blue-hc        undy-256-blue-d)
       (undy-256-blue-lc        undy-256-blue-l)
       (undy-256-cyan-hc        undy-256-cyan-d)
       (undy-256-cyan-lc        undy-256-cyan-l)
       (undy-256-green-hc       undy-256-green-d)
       (undy-256-green-lc       undy-256-green-l)

       ;; Distinct fringe
       (undy-256-fringe-bg (if undy-distinct-fringe-background
                                  undy-256-gray
                                undy-256-background)))

  ;; Define faces
  (custom-theme-set-faces
   'undy

   ;; font lock for syntax highlighting
   `(font-lock-builtin-face
     ((,undy-class (:foreground ,undy-red
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-red
                                        :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(font-lock-comment-face
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(font-lock-constant-face
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   `(font-lock-doc-face
     ((,undy-class (:foreground ,(if undy-doc-face-as-comment
                                        undy-comments
                                      undy-yellow)))
      (,undy-256-class (:foreground ,(if undy-doc-face-as-comment
                                            undy-256-comments
                                          undy-256-yellow)))))

   `(font-lock-function-name-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(font-lock-keyword-face
     ((,undy-class (:foreground "#EC691E"
                                   :weight normal))
      (,undy-256-class (:foreground "#EC691E"
                                        :weight normal))))

   `(font-lock-negation-char-face
     ((,undy-class (:foreground ,undy-yellow
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight bold))))

   `(font-lock-preprocessor-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(font-lock-regexp-grouping-construct
     ((,undy-class (:foreground ,undy-yellow
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,undy-class (:foreground ,undy-violet
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-violet
                                        :weight normal))))

   `(font-lock-string-face
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(font-lock-type-face
     ((,undy-class (:foreground ,undy-blue
                                   :italic nil))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :italic nil))))

   `(font-lock-variable-name-face
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(font-lock-warning-face
     ((,undy-class (:foreground ,undy-orange
                                   :weight bold
                                   :italic t
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-orange
                                        :weight bold
                                        :italic t
                                        :underline t))))

   `(c-annotation-face
     ((,undy-class (:foreground "#646464"))
      (,undy-256-class (:foreground "#646464"))))

   ;; general colouring
   '(button ((t (:underline t))))

   `(default
      ((,undy-class (:foreground ,undy-foreground
                                    :background ,undy-background))
       (,undy-256-class (:foreground ,undy-256-foreground
                                         :background ,undy-256-background))))

   `(highlight
     ((,undy-class (:background ,undy-highlight))
      (,undy-256-class (:background ,undy-256-highlight))))

   `(lazy-highlight
     ((,undy-class (:inherit highlight
                                :background ,undy-highlight-alt))
      (,undy-256-class (:inherit highlight
                                     :background ,undy-256-highlight-alt))))

   `(region
     ((,undy-class (:inherit highlight
                                :background ,undy-highlight))
      (,undy-256-class (:inherit highlight
                                     :background ,undy-256-highlight))))

   `(secondary-selection
     ((,undy-class (:inherit region
                                :background ,undy-highlight-alt))
      (,undy-256-class (:inherit region
                                     :background ,undy-256-highlight-alt))))

   `(shadow
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(match
     ((,undy-class (:background ,undy-green
                                   :foreground ,undy-background
                                   :weight bold))
      (,undy-256-class (:background ,undy-256-green
                                        :foreground ,undy-256-background
                                        :weight bold))))

   `(cursor
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-foreground
                                   :inverse-video t))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-foreground
                                        :inverse-video t))))

   `(mouse
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-foreground
                                   :inverse-video t))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-foreground
                                        :inverse-video t))))

   `(escape-glyph
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(escape-glyph-face
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(fringe
     ((,undy-class (:foreground ,undy-foreground
                                   :background ,undy-fringe-bg))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :background ,undy-256-fringe-bg))))

   `(link
     ((,undy-class (:foreground ,undy-blue
                                   :underline t
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :underline t
                                        :weight bold))))

   `(link-visited
     ((,undy-class (:foreground ,undy-violet
                                   :underline t
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-violet
                                        :underline t
                                        :weight normal))))

   `(success
     ((,undy-class (:foreground ,undy-green ))
      (,undy-256-class (:foreground ,undy-256-green ))))

   `(warning
     ((,undy-class (:foreground ,undy-yellow ))
      (,undy-256-class (:foreground ,undy-256-yellow ))))

   `(error
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(eval-sexp-fu-flash
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-green))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-green))))

   `(eval-sexp-fu-flash-error
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-red))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-red))))

   `(trailing-whitespace
     ((,undy-class (:background ,undy-red))
      (,undy-256-class (:background ,undy-256-red))))

   `(vertical-border
     ((,undy-class (:foreground ,undy-gray))
      (,undy-256-class (:foreground ,undy-256-gray))))

   `(menu
     ((,undy-class (:foreground ,undy-foreground
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :background ,undy-256-background))))

   `(minibuffer-prompt
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   ;; mode-line and powerline
   `(mode-line-buffer-id
     ((,undy-class (:foreground ,undy-green
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-green
                                        :weight bold))))

   `(mode-line
     ((,undy-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,undy-emphasis
                                      :background ,undy-highlight
                                      :box (:line-width 1
                                                        :color ,undy-gray
                                                        :style unspecified)))
      (,undy-256-class (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,undy-256-foreground
                                           :background ,undy-256-background
                                           :box (:line-width 1
                                                             :color ,undy-256-highlight
                                                             :style unspecified)))))

   `(powerline-active1
     ((,undy-class (:background ,undy-gray-d))
      (,undy-256-class (:background ,undy-256-gray-d))))

   `(powerline-active2
     ((,undy-class (:background ,undy-background))
      (,undy-256-class (:background ,undy-256-background))))


   `(mode-line-inactive
     ((,undy-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,undy-comments
                                      :background ,undy-background
                                      :box (:line-width 1
                                                        :color ,undy-gray
                                                        :style unspecified)))
      (,undy-256-class (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,undy-256-comments
                                           :background ,undy-256-background
                                           :box (:line-width 1
                                                             :color ,undy-256-gray
                                                             :style unspecified)))))

   `(powerline-inactive1
     ((,undy-class (:background ,undy-gray-d))
      (,undy-256-class (:background ,undy-256-gray-d))))

   `(powerline-inactive2
     ((,undy-class (:background ,undy-background))
      (,undy-256-class (:background ,undy-256-background))))

   ;; header-line
   `(header-line
     ((,undy-class (:foreground ,undy-emphasis
                                   :background ,undy-highlight
                                   :box (:color ,undy-gray
                                                :line-width 1
                                                :style unspecified)))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :background ,undy-256-highlight
                                        :box (:color ,undy-256-gray
                                                     :line-width 1
                                                     :style unspecified)))))

   ;; cua
   `(cua-global-mark
     ((,undy-class (:background ,undy-yellow
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-yellow
                                        :foreground ,undy-256-background))))

   `(cua-rectangle
     ((,undy-class (:inherit region))
      (,undy-256-class (:inherit region))))

   `(cua-rectangle-noselect
     ((,undy-class (:inherit secondary-selection))
      (,undy-256-class (:inherit secondary-selection))))

   ;; diary
   `(diary
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   ;; dired
   `(dired-directory
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(dired-flagged
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(dired-header
     ((,undy-class (:foreground ,undy-blue
                                   :background ,undy-background
                                   :inherit bold))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :background ,undy-256-background
                                        :inherit bold))))

   `(dired-ignored
     ((,undy-class (:inherit shadow))
      (,undy-256-class (:inherit shadow))))

   `(dired-mark
     ((,undy-class (:foreground ,undy-green
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-green
                                        :weight bold))))

   `(dired-marked
     ((,undy-class (:foreground ,undy-violet
                                   :inherit bold))
      (,undy-256-class (:foreground ,undy-256-violet
                                        :inherit bold))))

   `(dired-perm-write
     ((,undy-class (:foreground ,undy-foreground
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :underline t))))

   `(dired-symlink
     ((,undy-class (:foreground ,undy-cyan
                                   :slant italic))
      (,undy-256-class (:foreground ,undy-256-cyan
                                        :slant italic))))

   `(dired-warning
     ((,undy-class (:foreground ,undy-orange
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-orange
                                        :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-blue))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-blue))))

   `(dropdown-list-selection-face
     ((,undy-class (:background ,undy-green
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-green
                                        :foreground ,undy-256-background))))

   ;; ecb
   `(ecb-default-highlight-face
     ((,undy-class (:background ,undy-blue
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-blue
                                        :foreground ,undy-256-background))))

   `(ecb-history-bucket-node-dir-soure-path-face
     ((,undy-class (:inherit ecb-history-bucket-node-face
                                :foreground ,undy-yellow))
      (,undy-256-class (:inherit ecb-history-bucket-node-face
                                     :foreground ,undy-256-yellow))))

   `(ecb-source-in-directories-buffer-face
     ((,undy-class (:inherit ecb-directories-general-face
                                :foreground ,undy-foreground))
      (,undy-256-class (:inherit ecb-directories-general-face
                                     :foreground ,undy-256-foreground))))

   `(ecb-history-dead-buffer-face
     ((,undy-class (:inherit ecb-history-general-face
                                :foreground ,undy-comments))
      (,undy-256-class (:inherit ecb-history-general-face
                                     :foreground ,undy-256-comments))))

   `(ecb-directory-not-accessible-face
     ((,undy-class (:inherit ecb-directories-general-face
                                :foreground ,undy-comments))
      (,undy-256-class (:inherit ecb-directories-general-face
                                     :foreground ,undy-256-comments))))

   `(ecb-bucket-node-face
     ((,undy-class (:inherit ecb-default-general-face
                                :weight normal
                                :foreground ,undy-blue))
      (,undy-256-class (:inherit ecb-default-general-face
                                     :weight normal
                                     :foreground ,undy-256-blue))))

   `(ecb-tag-header-face
     ((,undy-class (:background ,undy-highlight-line))
      (,undy-256-class (:background ,undy-256-highlight-line))))

   `(ecb-analyse-bucket-element-face
     ((,undy-class (:inherit ecb-analyse-general-face
                                :foreground ,undy-green))
      (,undy-256-class (:inherit ecb-analyse-general-face
                                     :foreground ,undy-256-green))))

   `(ecb-directories-general-face
     ((,undy-class (:inherit ecb-default-general-face
                                :height 1.0))
      (,undy-256-class (:inherit ecb-default-general-face
                                     :height 1.0))))

   `(ecb-method-non-semantic-face
     ((,undy-class (:inherit ecb-methods-general-face
                                :foreground ,undy-cyan))
      (,undy-256-class (:inherit ecb-methods-general-face
                                     :foreground ,undy-256-cyan))))

   `(ecb-mode-line-prefix-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(ecb-tree-guide-line-face
     ((,undy-class (:inherit ecb-default-general-face
                                :foreground ,undy-gray
                                :height 1.0))
      (,undy-256-class (:inherit ecb-default-general-face
                                     :foreground ,undy-256-gray
                                     :height 1.0))))

   ;; ee
   `(ee-bookmarked
     ((,undy-class (:foreground ,undy-emphasis))
      (,undy-256-class (:foreground ,undy-256-emphasis))))

   `(ee-category
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(ee-link
     ((,undy-class (:inherit link))
      (,undy-256-class (:inherit link))))

   `(ee-link-visited
     ((,undy-class (:inherit link-visited))
      (,undy-256-class (:inherit link-visited))))

   `(ee-marked
     ((,undy-class (:foreground ,undy-magenta
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-magenta
                                        :weight bold))))

   `(ee-omitted
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(ee-shadow
     ((,undy-class (:inherit shadow))
      (,undy-256-class (:inherit shadow))))

   ;; grep
   `(grep-context-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(grep-error-face
     ((,undy-class (:foreground ,undy-red
                                   :weight bold
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-red
                                        :weight bold
                                        :underline t))))

   `(grep-hit-face
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(grep-match-face
     ((,undy-class (:foreground ,undy-green
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-green
                                        :weight bold))))

   ;; isearch
   `(isearch
     ((,undy-class (:inherit region
                                :foreground ,undy-background
                                :background ,undy-yellow))
      (,undy-256-class (:inherit region
                                     :foreground ,undy-256-background
                                     :background ,undy-256-yellow))))

   `(isearch-fail
     ((,undy-class (:inherit isearch
                                :foreground ,undy-red
                                :background ,undy-background
                                :bold t))
      (,undy-256-class (:inherit isearch
                                     :foreground ,undy-256-red
                                     :background ,undy-256-background
                                     :bold t))))


   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,undy-class (:foreground ,undy-comments
                                   :background ,undy-background
                                   :inverse-video nil))
      (,undy-256-class (:foreground ,undy-256-comments
                                        :background ,undy-256-background
                                        :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,undy-class (:foreground ,undy-yellow
                                   :background ,undy-background
                                   :inverse-video nil
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :background ,undy-256-background
                                        :inverse-video nil
                                        :weight bold))))

   ;; auctex
   `(font-latex-bold-face
     ((,undy-class (:inherit bold
                                :foreground ,undy-emphasis))
      (,undy-256-class (:inherit bold
                                     :foreground ,undy-256-emphasis))))

   `(font-latex-doctex-documentation-face
     ((,undy-class (:background unspecified))
      (,undy-256-class (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,undy-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))
      (,undy-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,undy-class (:inherit italic :foreground ,undy-emphasis))
      (,undy-256-class (:inherit italic :foreground ,undy-256-emphasis))))

   `(font-latex-math-face
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   `(font-latex-sectioning-0-face
     ((,undy-class (:inherit font-latex-sectioning-1-face
                                :height ,undy-height-plus-1))
      (,undy-256-class (:inherit font-latex-sectioning-1-face
                                     :height ,undy-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,undy-class (:inherit font-latex-sectioning-2-face
                                :height ,undy-height-plus-1))
      (,undy-256-class (:inherit font-latex-sectioning-2-face
                                     :height ,undy-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,undy-class (:inherit font-latex-sectioning-3-face
                                :height ,undy-height-plus-1))
      (,undy-256-class (:inherit font-latex-sectioning-3-face
                                     :height ,undy-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,undy-class (:inherit font-latex-sectioning-4-face
                                :height ,undy-height-plus-1))
      (,undy-256-class (:inherit font-latex-sectioning-4-face
                                     :height ,undy-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,undy-class (:inherit font-latex-sectioning-5-face
                                :height ,undy-height-plus-1))
      (,undy-256-class (:inherit font-latex-sectioning-5-face
                                     :height ,undy-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,undy-class (:inherit ,undy-pitch
                                :foreground ,undy-yellow
                                :weight bold))
      (,undy-256-class (:inherit ,undy-pitch :
                                     foreground ,undy-256-yellow
                                     :weight bold))))

   `(font-latex-sedate-face
     ((,undy-class (:foreground ,undy-emphasis))
      (,undy-256-class (:foreground ,undy-256-emphasis))))

   `(font-latex-slide-title-face
     ((,undy-class (:inherit (,undy-pitch font-lock-type-face)
                                :weight bold
                                :height ,undy-height-plus-3))
      (,undy-256-class (:inherit (,undy-pitch font-lock-type-face)
                                     :weight bold
                                     :height ,undy-height-plus-3))))

   `(font-latex-string-face
     ((,undy-class (:foreground ,undy-cyan))
      (,undy-256-class (:foreground ,undy-256-cyan))))

   `(font-latex-subscript-face
     ((,undy-class (:height ,undy-height-minus-1))
      (,undy-256-class (:height ,undy-height-minus-1))))

   `(font-latex-superscript-face
     ((,undy-class (:height ,undy-height-minus-1))
      (,undy-256-class (:height ,undy-height-minus-1))))

   `(font-latex-verbatim-face
     ((,undy-class (:inherit fixed-pitch
                                :foreground ,undy-foreground
                                :slant italic))
      (,undy-256-class (:inherit fixed-pitch
                                     :foreground ,undy-256-foreground
                                     :slant italic))))

   `(font-latex-warning-face
     ((,undy-class (:inherit bold
                                :foreground ,undy-orange))
      (,undy-256-class (:inherit bold
                                     :foreground ,undy-256-orange))))

   ;; auto-complete
   `(ac-candidate-face
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-blue))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-blue))))

   `(ac-selection-face
     ((,undy-class (:background ,undy-blue
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-blue
                                        :foreground ,undy-256-background))))

   `(ac-candidate-mouse-face
     ((,undy-class (:background ,undy-blue
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-blue
                                        :foreground ,undy-256-background))))

   `(ac-completion-face
     ((,undy-class (:foreground ,undy-emphasis
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :underline t))))

   `(ac-gtags-candidate-face
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-blue))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-blue))))

   `(ac-gtags-selection-face
     ((,undy-class (:background ,undy-blue
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-blue
                                        :foreground ,undy-256-background))))

   `(ac-yasnippet-candidate-face
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-yellow))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-yellow))))

   `(ac-yasnippet-selection-face
     ((,undy-class (:background ,undy-yellow
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-yellow
                                        :foreground ,undy-256-background))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-blue))))

   `(ahs-edit-mode-face
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-highlight))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-highlight))))

   `(ahs-face
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-magenta
                                        :background unspecified))))

   `(ahs-plugin-bod-face
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-violet ))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-cyan ))))

   `(ahs-plugin-defalt-face
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-orange))))

   `(ahs-plugin-whole-buffer-face
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-green))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-green))))

   `(ahs-warning-face
     ((,undy-class (:foreground ,undy-red
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-red
                                        :weight bold))))

   ;; android mode
   `(android-mode-debug-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(android-mode-error-face
     ((,undy-class (:foreground ,undy-orange
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-orange
                                        :weight bold))))

   `(android-mode-info-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(android-mode-verbose-face
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(android-mode-warning-face
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,undy-class (:foreground ,undy-violet
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-violet
                                        :weight bold))))

   ;; bm
   `(bm-face
     ((,undy-class (:background ,undy-yellow-lc
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-yellow-lc
                                        :foreground ,undy-256-background))))

   `(bm-fringe-face
     ((,undy-class (:background ,undy-yellow-lc
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-yellow-lc
                                        :foreground ,undy-256-background))))

   `(bm-fringe-persistent-face
     ((,undy-class (:background ,undy-green-lc
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-green-lc
                                        :foreground ,undy-256-background))))

   `(bm-persistent-face
     ((,undy-class (:background ,undy-green-lc
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-green-lc
                                        :foreground ,undy-256-background))))

   ;; calfw
   `(cfw:face-day-title
     ((,undy-class (:background ,undy-highlight-line))
      (,undy-256-class (:background ,undy-256-highlight-line))))

   `(cfw:face-annotation
     ((,undy-class (:inherit cfw:face-day-title
                                :foreground ,undy-yellow))
      (,undy-256-class (:inherit cfw:face-day-title
                                     :foreground ,undy-256-yellow))))

   `(cfw:face-default-content
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(cfw:face-default-day
     ((,undy-class (:inherit cfw:face-day-title
                                :weight bold))
      (,undy-256-class (:inherit cfw:face-day-title
                                     :weight bold))))

   `(cfw:face-disable
     ((,undy-class (:inherit cfw:face-day-title
                                :foreground ,undy-comments))
      (,undy-256-class (:inherit cfw:face-day-title
                                     :foreground ,undy-256-comments))))

   `(cfw:face-grid
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(cfw:face-header
     ((,undy-class (:foreground ,undy-blue-hc
                                   :background ,undy-blue-lc
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-blue-hc
                                        :background ,undy-256-blue-lc
                                        :weight bold))))

   `(cfw:face-holiday
     ((,undy-class (:background nil
                                   :foreground ,undy-red
                                   :weight bold))
      (,undy-256-class (:background nil
                                        :foreground ,undy-256-red
                                        :weight bold))))

   `(cfw:face-periods
     ((,undy-class (:foreground ,undy-magenta))
      (,undy-256-class (:foreground ,undy-256-magenta))))

   `(cfw:face-select
     ((,undy-class (:background ,undy-magenta-lc
                                   :foreground ,undy-magenta-hc))
      (,undy-256-class (:background ,undy-256-magenta-lc
                                        :foreground ,undy-256-magenta-hc))))

   `(cfw:face-saturday
     ((,undy-class (:foreground ,undy-cyan-hc
                                   :background ,undy-cyan-lc))
      (,undy-256-class (:foreground ,undy-256-cyan-hc
                                        :background ,undy-256-cyan-lc))))

   `(cfw:face-sunday
     ((,undy-class (:foreground ,undy-red-hc
                                   :background ,undy-red-lc
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-red-hc
                                        :background ,undy-256-red-lc
                                        :weight bold))))

   `(cfw:face-title
     ((,undy-class (:inherit ,undy-pitch
                                :foreground ,undy-yellow
                                :weight bold
                                :height ,undy-height-plus-4))
      (,undy-256-class (:inherit ,undy-pitch
                                     :foreground ,undy-256-yellow
                                     :weight bold
                                     :height ,undy-height-plus-4))))

   `(cfw:face-today
     ((,undy-class (:weight bold
                               :background ,undy-highlight-line
                               :foreground nil))
      (,undy-256-class (:weight bold
                                    :background ,undy-256-highlight-line
                                    :foreground nil))))

   `(cfw:face-today-title
     ((,undy-class (:background ,undy-yellow-lc
                                   :foreground ,undy-yellow-hc
                                   :weight bold))
      (,undy-256-class (:background ,undy-256-yellow-lc
                                        :foreground ,undy-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-foreground))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-foreground))))

   `(cfw:face-toolbar-button-off
     ((,undy-class (:background ,undy-yellow-lc
                                   :foreground ,undy-yellow-hc
                                   :weight bold))
      (,undy-256-class (:background ,undy-256-yellow-lc
                                        :foreground ,undy-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar-button-on
     ((,undy-class (:background ,undy-yellow-hc
                                   :foreground ,undy-yellow-lc
                                   :weight bold))
      (,undy-256-class (:background ,undy-256-yellow-hc
                                        :foreground ,undy-256-yellow-lc
                                        :weight bold))))

   ;; cider
   `(cider-enlightened
     ((,undy-class (:foreground ,undy-yellow
                                   :background nil
                                   :box (:color ,undy-yellow :line-width -1 :style nil)))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :background nil
                                        :box (:color ,undy-256-yellow :line-width -1 :style nil))) ))

   `(cider-enlightened-local
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(cider-instrumented-face
     ((,undy-class (:foreground ,undy-violet
                                   :background nil
                                   :box (:color ,undy-violet :line-width -1 :style nil)))
      (,undy-256-class (:foreground ,undy-256-violet
                                        :background nil
                                        :box (:color ,undy-256-violet :line-width -1 :style nil)))))

   `(cider-result-overlay-face
     ((,undy-class (:foreground ,undy-blue
                                   :background nil
                                   :box (:color ,undy-blue :line-width -1 :style nil)))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :background nil
                                        :box (:color ,undy-256-blue :line-width -1 :style nil)))))

   `(cider-test-error-face
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-orange))))

   `(cider-test-failure-face
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-red))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-red))))

   `(cider-test-success-face
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-green))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-green))))

   `(cider-traced-face
     ((,undy-class :box (:color ,undy-blue :line-width -1 :style nil))
      (,undy-256-class  :box (:color ,undy-256-blue :line-width -1 :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face
     ((,undy-class (:foreground ,undy-red
                                   :weight bold
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-error-face
     ((,undy-class (:foreground ,undy-orange
                                   :weight bold
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-success-face
     ((,undy-class (:foreground ,undy-green
                                   :weight bold
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-green
                                        :weight bold
                                        :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-emphasis))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-emphasis))))

   `(company-tooltip-selection
     ((,undy-class (:background ,undy-blue
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-blue
                                        :foreground ,undy-256-background))))

   `(company-tooltip-mouse
     ((,undy-class (:background ,undy-blue
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-blue
                                        :foreground ,undy-256-background))))

   `(company-tooltip-common
     ((,undy-class (:foreground ,undy-blue
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :underline t))))

   `(company-tooltip-common-selection
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-blue
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-blue
                                        :underline t))))

   `(company-preview
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-emphasis))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-emphasis))))

   `(company-preview-common
     ((,undy-class (:foreground ,undy-blue
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :underline t))))

   `(company-scrollbar-bg
     ((,undy-class (:background ,undy-gray))
      (,undy-256-class (:background ,undy-256-gray))))

   `(company-scrollbar-fg
     ((,undy-class (:background ,undy-comments))
      (,undy-256-class (:background ,undy-256-comments))))

   `(company-tooltip-annotation
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-green))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-green))))

   `(company-template-field
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-blue))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-blue))))

   ;; compilation
   `(compilation-column-face
     ((,undy-class (:foreground ,undy-cyan
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-cyan
                                        :underline nil))))

   `(compilation-column-number
     ((,undy-class (:inherit font-lock-doc-face
                                :foreground ,undy-cyan
                                :underline nil))
      (,undy-256-class (:inherit font-lock-doc-face
                                     :foreground ,undy-256-cyan
                                     :underline nil))))

   `(compilation-enter-directory-face
     ((,undy-class (:foreground ,undy-green
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-green
                                        :underline nil))))

   `(compilation-error
     ((,undy-class (:inherit error
                                :underline nil))
      (,undy-256-class (:inherit error
                                     :underline nil))))

   `(compilation-error-face
     ((,undy-class (:foreground ,undy-red
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-red
                                        :underline nil))))

   `(compilation-face
     ((,undy-class (:foreground ,undy-foreground
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :underline nil))))

   `(compilation-info
     ((,undy-class (:foreground ,undy-comments
                                   :underline nil
                                   :bold nil))
      (,undy-256-class (:foreground ,undy-256-comments
                                        :underline nil
                                        :bold nil))))

   `(compilation-info-face
     ((,undy-class (:foreground ,undy-blue
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :underline nil))))

   `(compilation-leave-directory-face
     ((,undy-class (:foreground ,undy-green
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-green
                                        :underline nil))))

   `(compilation-line-face
     ((,undy-class (:foreground ,undy-green
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-green
                                        :underline nil))))

   `(compilation-line-number
     ((,undy-class (:foreground ,undy-green
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-green
                                        :underline nil))))

   `(compilation-warning
     ((,undy-class (:inherit warning
                                :underline nil))
      (,undy-256-class (:inherit warning
                                     :underline nil))))

   `(compilation-warning-face
     ((,undy-class (:foreground ,undy-yellow
                                   :weight normal
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(compilation-mode-line-exit
     ((,undy-class (:inherit compilation-info
                                :foreground ,undy-green
                                :weight bold))
      (,undy-256-class (:inherit compilation-info
                                     :foreground ,undy-256-green
                                     :weight bold))))

   `(compilation-mode-line-fail
     ((,undy-class (:inherit compilation-error
                                :foreground ,undy-red
                                :weight bold))
      (,undy-256-class (:inherit compilation-error
                                     :foreground ,undy-256-red
                                     :weight bold))))

   `(compilation-mode-line-run
     ((,undy-class (:foreground ,undy-orange
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-orange
                                        :weight bold))))

   ;; CSCOPE
   `(cscope-file-face
     ((,undy-class (:foreground ,undy-green
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-green
                                        :weight bold))))

   `(cscope-function-face
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(cscope-line-number-face
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(cscope-line-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(cscope-mouse-face
     ((,undy-class (:background ,undy-blue
                                   :foreground ,undy-foreground))
      (,undy-256-class (:background ,undy-256-blue
                                        :foreground ,undy-256-foreground))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-emphasis
                                   :underline ,undy-emphasis
                                   :weight bold))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-emphasis
                                        :underline ,undy-256-emphasis
                                        :weight bold))))

   `(ctbl:face-continue-bar
     ((,undy-class (:background ,undy-gray
                                   :foreground ,undy-yellow))
      (,undy-256-class (:background ,undy-256-gray
                                        :foreground ,undy-256-yellow))))

   `(ctbl:face-row-select
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-foreground
                                   :underline t))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-foreground
                                        :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,undy-class (:foreground ,undy-yellow
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight bold))))

   `(coffee-mode-function-param
     ((,undy-class (:foreground ,undy-violet
                                   :slant italic))
      (,undy-256-class (:foreground ,undy-256-violet
                                        :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,undy-class (:inherit ,undy-pitch
                                :height ,undy-height-plus-3
                                :foreground ,undy-violet
                                :weight bold))
      (,undy-256-class (:inherit ,undy-pitch
                                     :height ,undy-height-plus-3
                                     :foreground ,undy-256-violet
                                     :weight bold))))

   `(custom-variable-tag
     ((,undy-class (:inherit ,undy-pitch
                                :foreground ,undy-cyan
                                :height ,undy-height-plus-3))
      (,undy-256-class (:inherit ,undy-pitch
                                     :foreground ,undy-256-cyan
                                     :height ,undy-height-plus-3))))

   `(custom-comment-tag
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(custom-group-tag
     ((,undy-class (:inherit ,undy-pitch
                                :foreground ,undy-blue
                                :height ,undy-height-plus-3))
      (,undy-256-class (:inherit ,undy-pitch
                                     :foreground ,undy-256-blue
                                     :height ,undy-height-plus-3))))

   `(custom-group-tag-1
     ((,undy-class (:inherit ,undy-pitch
                                :foreground ,undy-red
                                :height ,undy-height-plus-3))
      (,undy-256-class (:inherit ,undy-pitch
                                     :foreground ,undy-256-red
                                     :height ,undy-height-plus-3))))

   `(custom-state
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   ;; diff
   `(diff-added
     ((,undy-class (:foreground ,undy-green
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-green
                                        :background ,undy-256-background))))

   `(diff-changed
     ((,undy-class (:foreground ,undy-blue
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :background ,undy-256-background))))

   `(diff-removed
     ((,undy-class (:foreground ,undy-red
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-red
                                        :background ,undy-256-background))))

   `(diff-header
     ((,undy-class (:background ,undy-background))
      (,undy-256-class (:background ,undy-256-background))))

   `(diff-file-header
     ((,undy-class (:background ,undy-background
                                   :foreground ,undy-foreground
                                   :weight bold))
      (,undy-256-class (:background ,undy-256-background
                                        :foreground ,undy-256-foreground
                                        :weight bold))))

   `(diff-refine-added
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-green))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-green))))

   `(diff-refine-change
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-blue))))

   `(diff-refine-removed
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-red))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-red))))

   ;; diff-hl
   `(diff-hl-change
     ((,undy-class (:background ,undy-blue-lc
                                   :foreground ,undy-blue-hc))
      (,undy-256-class (:background ,undy-256-blue-lc
                                        :foreground ,undy-256-blue-hc))))

   `(diff-hl-delete
     ((,undy-class (:background ,undy-red-lc
                                   :foreground ,undy-red-hc))
      (,undy-256-class (:background ,undy-256-red-lc
                                        :foreground ,undy-256-red-hc))))

   `(diff-hl-insert
     ((,undy-class (:background ,undy-green-lc
                                   :foreground ,undy-green-hc))
      (,undy-256-class (:background ,undy-256-green-lc
                                        :foreground ,undy-256-green-hc))))

   `(diff-hl-unknown
     ((,undy-class (:background ,undy-violet-lc
                                   :foreground ,undy-violet-hc))
      (,undy-256-class (:background ,undy-256-violet-lc
                                        :foreground ,undy-256-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,undy-class (:background ,undy-orange-lc))
      (,undy-256-class (:background ,undy-256-orange-lc))))

   `(ediff-fine-diff-B
     ((,undy-class (:background ,undy-green-lc))
      (,undy-256-class (:background ,undy-256-green-lc))))

   `(ediff-fine-diff-C
     ((,undy-class (:background ,undy-yellow-lc))
      (,undy-256-class (:background ,undy-256-yellow-lc))))

   `(ediff-current-diff-C
     ((,undy-class (:background ,undy-blue-lc))
      (,undy-256-class (:background ,undy-256-blue-lc))))

   `(ediff-even-diff-A
     ((,undy-class (:background ,undy-comments
                                   :foreground ,undy-foreground-lc ))
      (,undy-256-class (:background ,undy-256-comments
                                        :foreground ,undy-256-foreground-lc ))))

   `(ediff-odd-diff-A
     ((,undy-class (:background ,undy-comments
                                   :foreground ,undy-foreground-hc ))
      (,undy-256-class (:background ,undy-256-comments
                                        :foreground ,undy-256-foreground-hc ))))

   `(ediff-even-diff-B
     ((,undy-class (:background ,undy-comments
                                   :foreground ,undy-foreground-hc ))
      (,undy-256-class (:background ,undy-256-comments
                                        :foreground ,undy-256-foreground-hc ))))

   `(ediff-odd-diff-B
     ((,undy-class (:background ,undy-comments
                                   :foreground ,undy-foreground-lc ))
      (,undy-256-class (:background ,undy-256-comments
                                        :foreground ,undy-256-foreground-lc ))))

   `(ediff-even-diff-C
     ((,undy-class (:background ,undy-comments
                                   :foreground ,undy-foreground ))
      (,undy-256-class (:background ,undy-256-comments
                                        :foreground ,undy-256-foreground ))))

   `(ediff-odd-diff-C
     ((,undy-class (:background ,undy-comments
                                   :foreground ,undy-background ))
      (,undy-256-class (:background ,undy-256-comments
                                        :foreground ,undy-256-background ))))

   ;; edts
   `(edts-face-error-line
     ((,(append '((supports :underline (:style line))) undy-class)
       (:underline (:style line :color ,undy-red)
                   :inherit unspecified))
      (,undy-class (:foreground ,undy-red-hc
                                   :background ,undy-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) undy-256-class )
       (:underline (:style line :color ,undy-256-red)
                   :inherit unspecified))
      (,undy-256-class (:foreground ,undy-256-red-hc
                                        :background ,undy-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-warning-line
     ((,(append '((supports :underline (:style line))) undy-class)
       (:underline (:style line :color ,undy-yellow)
                   :inherit unspecified))
      (,undy-class (:foreground ,undy-yellow-hc
                                   :background ,undy-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) undy-256-class )
       (:underline (:style line :color ,undy-256-yellow)
                   :inherit unspecified))
      (,undy-256-class (:foreground ,undy-256-yellow-hc
                                        :background ,undy-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-error-fringe-bitmap
     ((,undy-class (:foreground ,undy-red
                                   :background unspecified
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-red
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-warning-fringe-bitmap
     ((,undy-class (:foreground ,undy-yellow
                                   :background unspecified
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-error-mode-line
     ((,undy-class (:background ,undy-red
                                   :foreground unspecified))
      (,undy-256-class (:background ,undy-256-red
                                        :foreground unspecified))))

   `(edts-face-warning-mode-line
     ((,undy-class (:background ,undy-yellow
                                   :foreground unspecified))
      (,undy-256-class (:background ,undy-256-yellow
                                        :foreground unspecified))))


   ;; elfeed
   `(elfeed-search-date-face
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(elfeed-search-feed-face
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(elfeed-search-tag-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(elfeed-search-title-face
     ((,undy-class (:foreground ,undy-cyan))
      (,undy-256-class (:foreground ,undy-256-cyan))))

   ;; elixir
   `(elixir-attribute-face
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(elixir-atom-face
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   ;; ein
   `(ein:cell-input-area
     ((,undy-class (:background ,undy-highlight-line))
      (,undy-256-class (:background ,undy-256-highlight-line))))
   `(ein:cell-input-prompt
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))
   `(ein:cell-output-prompt
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))
   `(ein:notification-tab-normal
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))
   `(ein:notification-tab-selected
     ((,undy-class (:foreground ,undy-orange :inherit bold))
      (,undy-256-class (:foreground ,undy-256-orange :inherit bold))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,undy-class (:inherit font-lock-string-face))
      (,undy-256-class (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,undy-class (:inherit font-lock-string-face))
      (,undy-256-class (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,undy-class (:inherit font-lock-string-face))
      (,undy-256-class (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,undy-class (:inherit font-lock-keyword-face))
      (,undy-256-class (:inherit font-lock-keyword-face))))

   ;; erm-syn
   `(erm-syn-errline
     ((,(append '((supports :underline (:style wave))) undy-class)
       (:underline (:style wave :color ,undy-red)
                   :inherit unspecified))
      (,undy-class (:foreground ,undy-red-hc
                                   :background ,undy-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) undy-256-class )
       (:underline (:style wave :color ,undy-256-red)
                   :inherit unspecified))
      (,undy-256-class (:foreground ,undy-256-red-hc
                                        :background ,undy-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(erm-syn-warnline
     ((,(append '((supports :underline (:style wave))) undy-class)
       (:underline (:style wave :color ,undy-orange)
                   :inherit unspecified))
      (,undy-class (:foreground ,undy-orange-hc
                                   :background ,undy-orange-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) undy-256-class )
       (:underline (:style wave :color ,undy-256-orange)
                   :inherit unspecified))
      (,undy-256-class (:foreground ,undy-256-orange-hc
                                        :background ,undy-256-orange-lc
                                        :weight bold
                                        :underline t))))

   ;; epc
   `(epc:face-title
     ((,undy-class (:foreground ,undy-blue
                                   :background ,undy-background
                                   :weight normal
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :background ,undy-256-background
                                        :weight normal
                                        :underline nil))))

   ;; erc
   `(erc-action-face
     ((,undy-class (:inherit erc-default-face))
      (,undy-256-class (:inherit erc-default-face))))

   `(erc-bold-face
     ((,undy-class (:weight bold))
      (,undy-256-class (:weight bold))))

   `(erc-current-nick-face
     ((,undy-class (:foreground ,undy-blue :weight bold))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :weight bold))))

   `(erc-dangerous-host-face
     ((,undy-class (:inherit font-lock-warning-face))
      (,undy-256-class (:inherit font-lock-warning-face))))

   `(erc-default-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(erc-highlight-face
     ((,undy-class (:inherit erc-default-face
                                :background ,undy-highlight))
      (,undy-256-class (:inherit erc-default-face
                                     :background ,undy-256-highlight))))

   `(erc-direct-msg-face
     ((,undy-class (:inherit erc-default-face))
      (,undy-256-class (:inherit erc-default-face))))

   `(erc-error-face
     ((,undy-class (:inherit font-lock-warning-face))
      (,undy-256-class (:inherit font-lock-warning-face))))

   `(erc-fool-face
     ((,undy-class (:inherit erc-default-face))
      (,undy-256-class (:inherit erc-default-face))))

   `(erc-input-face
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(erc-keyword-face
     ((,undy-class (:foreground ,undy-blue
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :weight bold))))

   `(erc-nick-default-face
     ((,undy-class (:foreground ,undy-yellow
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight bold))))

   `(erc-my-nick-face
     ((,undy-class (:foreground ,undy-red
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-red
                                        :weight bold))))

   `(erc-nick-msg-face
     ((,undy-class (:inherit erc-default-face))
      (,undy-256-class (:inherit erc-default-face))))

   `(erc-notice-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(erc-pal-face
     ((,undy-class (:foreground ,undy-orange
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-orange
                                        :weight bold))))

   `(erc-prompt-face
     ((,undy-class (:foreground ,undy-orange
                                   :background ,undy-background
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-orange
                                        :background ,undy-256-background
                                        :weight bold))))

   `(erc-timestamp-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(erc-underline-face
     ((t (:underline t))))

   ;; eshell
   `(eshell-prompt
     ((,undy-class (:foreground ,undy-blue
                                   :inherit bold))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :inherit bold))))

   `(eshell-ls-archive
     ((,undy-class (:foreground ,undy-red
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-red
                                        :inherit bold))))

   `(eshell-ls-backup
     ((,undy-class (:inherit font-lock-comment-face))
      (,undy-256-class (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,undy-class (:inherit font-lock-comment-face))
      (,undy-256-class (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,undy-class (:foreground ,undy-blue
                                   :inherit bold))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :inherit bold))))

   `(eshell-ls-executable
     ((,undy-class (:foreground ,undy-green
                                   :inherit bold))
      (,undy-256-class (:foreground ,undy-256-green
                                        :inherit bold))))

   `(eshell-ls-unreadable
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(eshell-ls-missing
     ((,undy-class (:inherit font-lock-warning-face))
      (,undy-256-class (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,undy-class (:inherit font-lock-doc-face))
      (,undy-256-class (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,undy-class (:foreground ,undy-yellow
                                   :inherit bold))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :inherit bold))))

   `(eshell-ls-symlink
     ((,undy-class (:foreground ,undy-cyan
                                   :inherit bold))
      (,undy-256-class (:foreground ,undy-256-cyan
                                        :inherit bold))))

   ;; evil-ex-substitute
   `(evil-ex-substitute-matches
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-red-l
                                   :inherit italic))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-red-l
                                        :inherit italic))))
   `(evil-ex-substitute-replacement
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-green-l
                                   :inherit italic))
      (,undy-256-class (:background ,undy-256-highlight-line :foreground ,undy-256-green-l :inherit italic))))

   ;; evil-search-highlight-persist
   `(evil-search-highlight-persist-highlight-face
     ((,undy-class (:inherit region))
      (,undy-256-class (:inherit region))))

   ;; fic
   `(fic-author-face
     ((,undy-class (:background ,undy-background
                                   :foreground ,undy-orange
                                   :underline t
                                   :slant italic))
      (,undy-256-class (:background ,undy-256-background
                                        :foreground ,undy-256-orange
                                        :underline t
                                        :slant italic))))

   `(fic-face
     ((,undy-class (:background ,undy-background
                                   :foreground ,undy-orange
                                   :weight normal
                                   :slant italic))
      (,undy-256-class (:background ,undy-256-background
                                        :foreground ,undy-256-orange
                                        :weight normal
                                        :slant italic))))

   `(font-lock-fic-face
     ((,undy-class (:background ,undy-background
                                   :foreground ,undy-orange
                                   :weight normal
                                   :slant italic))
      (,undy-256-class (:background ,undy-256-background
                                        :foreground ,undy-256-orange
                                        :weight normal
                                        :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,undy-class (:foreground ,undy-blue
                                   :weight normal
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) undy-class)
       (:underline (:style wave :color ,undy-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,undy-class (:foreground ,undy-red-hc
                                   :background ,undy-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) undy-256-class )
       (:underline (:style wave :color ,undy-256-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,undy-256-class (:foreground ,undy-256-red-hc
                                        :background ,undy-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) undy-class)
       (:underline (:style wave :color ,undy-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,undy-class (:foreground ,undy-green-hc
                                   :background ,undy-green-lc))
      (,(append '((supports :underline (:style wave))) undy-256-class )
       (:underline (:style wave :color ,undy-256-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,undy-256-class (:foreground ,undy-256-green-hc
                                        :background ,undy-256-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) undy-class)
       (:underline (:style wave :color ,undy-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,undy-class (:foreground ,undy-yellow-hc
                                   :background ,undy-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) undy-256-class )
       (:underline (:style wave :color ,undy-256-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,undy-256-class (:foreground ,undy-256-yellow-hc
                                        :background ,undy-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style line))) undy-class)
       (:underline (:style line :color ,undy-red)))
      (,undy-class (:foreground ,undy-red
                                   :background ,undy-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) undy-256-class )
       (:underline (:style line :color ,undy-256-red)))
      (,undy-256-class (:foreground ,undy-256-red
                                        :background ,undy-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style line))) undy-class)
       (:underline (:style line :color ,undy-orange)))
      (,undy-class (:foreground ,undy-orange
                                   :background ,undy-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) undy-256-class )
       (:underline (:style line :color ,undy-256-orange)))
      (,undy-256-class (:foreground ,undy-256-orange
                                        :background ,undy-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style line))) undy-class)
       (:underline (:style line :color ,undy-blue)))
      (,undy-class (:foreground ,undy-blue
                                   :background ,undy-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) undy-256-class )
       (:underline (:style line :color ,undy-256-blue)))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :background ,undy-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-fringe-error
     ((,undy-class (:foreground ,undy-red-l
                                   :background unspecified
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-red-l
                                        :background unspecified
                                        :weight bold))))

   `(flycheck-fringe-warning
     ((,undy-class (:foreground ,undy-orange-l
                                   :background unspecified
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-orange-l
                                        :background unspecified
                                        :weight bold))))

   `(flycheck-fringe-info
     ((,undy-class (:foreground ,undy-blue-l
                                   :background unspecified
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-blue-l
                                        :background unspecified
                                        :weight bold))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) undy-class)
       (:underline (:style wave :color ,undy-yellow)
                   :inherit unspecified))
      (,undy-class (:foreground ,undy-yellow
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) undy-256-class )
       (:underline (:style wave :color ,undy-256-yellow)
                   :inherit unspecified))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight bold
                                        :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) undy-class)
       (:underline (:style wave :color ,undy-red)
                   :inherit unspecified))
      (,undy-class (:foreground ,undy-red
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) undy-256-class )
       (:underline (:style wave :color ,undy-256-red)
                   :inherit unspecified))
      (,undy-256-class (:foreground ,undy-256-red
                                        :weight bold
                                        :underline t))))


   ;; git-gutter
   `(git-gutter:added
     ((,undy-class (:background ,undy-green
                                   :foreground ,undy-background
                                   :inherit bold))
      (,undy-256-class (:background ,undy-256-green
                                        :foreground ,undy-256-background
                                        :inherit bold))))

   `(git-gutter:deleted
     ((,undy-class (:background ,undy-red
                                   :foreground ,undy-background
                                   :inherit bold))
      (,undy-256-class (:background ,undy-256-red
                                        :foreground ,undy-256-background
                                        :inherit bold))))

   `(git-gutter:modified
     ((,undy-class (:background ,undy-blue
                                   :foreground ,undy-background
                                   :inherit bold))
      (,undy-256-class (:background ,undy-256-blue
                                        :foreground ,undy-256-background
                                        :inherit bold))))

   `(git-gutter:unchanged
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-background
                                   :inherit bold))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-background
                                        :inherit bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,undy-class (:foreground ,undy-green
                                   :inherit bold))
      (,undy-256-class (:foreground ,undy-256-green
                                        :inherit bold))))

   `(git-gutter-fr:deleted
     ((,undy-class (:foreground ,undy-red
                                   :inherit bold))
      (,undy-256-class (:foreground ,undy-256-red
                                        :inherit bold))))

   `(git-gutter-fr:modified
     ((,undy-class (:foreground ,undy-blue
                                   :inherit bold))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :inherit bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,undy-class (:background ,undy-green
                                   :foreground ,undy-background
                                   :inherit bold))
      (,undy-256-class (:background ,undy-256-green
                                        :foreground ,undy-256-background
                                        :inherit bold))))

   `(git-gutter+-deleted
     ((,undy-class (:background ,undy-red
                                   :foreground ,undy-background
                                   :inherit bold))
      (,undy-256-class (:background ,undy-256-red
                                        :foreground ,undy-256-background
                                        :inherit bold))))

   `(git-gutter+-modified
     ((,undy-class (:background ,undy-blue
                                   :foreground ,undy-background
                                   :inherit bold))
      (,undy-256-class (:background ,undy-256-blue
                                        :foreground ,undy-256-background
                                        :inherit bold))))

   `(git-gutter+-unchanged
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-background
                                   :inherit bold))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-background
                                        :inherit bold))))

   `(git-gutter-fr+-added
     ((,undy-class (:foreground ,undy-green
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-green
                                        :weight bold))))

   `(git-gutter-fr+-deleted
     ((,undy-class (:foreground ,undy-red
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-red
                                        :weight bold))))

   `(git-gutter-fr+-modified
     ((,undy-class (:foreground ,undy-blue
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :weight bold))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face
     ((,undy-class (:foreground ,undy-blue
                                   :background ,undy-highlight-line
                                   :inherit bold))
      (,undy-256-class (:foreground ,undy-blue
                                        :background ,undy-256-highlight-line
                                        :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(guide-key/key-face
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(guide-key/prefix-command-face
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   ;; gnus
   `(gnus-group-mail-1
     ((,undy-class (:weight bold
                               :inherit gnus-group-mail-1-empty))
      (,undy-256-class (:weight bold
                                    :inherit gnus-group-mail-1-empty))))

   `(gnus-group-mail-1-empty
     ((,undy-class (:inherit gnus-group-news-1-empty))
      (,undy-256-class (:inherit gnus-group-news-1-empty))))

   `(gnus-group-mail-2
     ((,undy-class (:weight bold
                               :inherit gnus-group-mail-2-empty))
      (,undy-256-class (:weight bold
                                    :inherit gnus-group-mail-2-empty))))

   `(gnus-group-mail-2-empty
     ((,undy-class (:inherit gnus-group-news-2-empty))
      (,undy-256-class (:inherit gnus-group-news-2-empty))))

   `(gnus-group-mail-3
     ((,undy-class (:weight bold
                               :inherit gnus-group-mail-3-empty))
      (,undy-256-class (:weight bold
                                    :inherit gnus-group-mail-3-empty))))

   `(gnus-group-mail-3-empty
     ((,undy-class (:inherit gnus-group-news-3-empty))
      (,undy-256-class (:inherit gnus-group-news-3-empty))))

   `(gnus-group-mail-low
     ((,undy-class (:weight bold
                               :inherit gnus-group-mail-low-empty))
      (,undy-256-class (:weight bold
                                    :inherit gnus-group-mail-low-empty))))

   `(gnus-group-mail-low-empty
     ((,undy-class (:inherit gnus-group-news-low-empty))
      (,undy-256-class (:inherit gnus-group-news-low-empty))))

   `(gnus-group-news-1
     ((,undy-class (:weight bold
                               :inherit gnus-group-news-1-empty))
      (,undy-256-class (:weight bold
                                    :inherit gnus-group-news-1-empty))))

   `(gnus-group-news-2
     ((,undy-class (:weight bold
                               :inherit gnus-group-news-2-empty))
      (,undy-256-class (:weight bold
                                    :inherit gnus-group-news-2-empty))))

   `(gnus-group-news-3
     ((,undy-class (:weight bold
                               :inherit gnus-group-news-3-empty))
      (,undy-256-class (:weight bold
                                    :inherit gnus-group-news-3-empty))))

   `(gnus-group-news-4
     ((,undy-class (:weight bold
                               :inherit gnus-group-news-4-empty))
      (,undy-256-class (:weight bold
                                    :inherit gnus-group-news-4-empty))))

   `(gnus-group-news-5
     ((,undy-class (:weight bold
                               :inherit gnus-group-news-5-empty))
      (,undy-256-class (:weight bold
                                    :inherit gnus-group-news-5-empty))))

   `(gnus-group-news-6
     ((,undy-class (:weight bold
                               :inherit gnus-group-news-6-empty))
      (,undy-256-class (:weight bold
                                    :inherit gnus-group-news-6-empty))))

   `(gnus-group-news-low
     ((,undy-class (:weight bold
                               :inherit gnus-group-news-low-empty))
      (,undy-256-class (:weight bold
                                    :inherit gnus-group-news-low-empty))))

   `(gnus-header-content
     ((,undy-class (:inherit message-header-other))
      (,undy-256-class (:inherit message-header-other))))

   `(gnus-header-from
     ((,undy-class (:inherit message-header-other))
      (,undy-256-class (:inherit message-header-other))))

   `(gnus-header-name
     ((,undy-class (:inherit message-header-name))
      (,undy-256-class (:inherit message-header-name))))

   `(gnus-header-newsgroups
     ((,undy-class (:inherit message-header-other))
      (,undy-256-class (:inherit message-header-other))))

   `(gnus-header-subject
     ((,undy-class (:inherit message-header-subject))
      (,undy-256-class (:inherit message-header-subject))))

   `(gnus-summary-cancelled
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(gnus-summary-high-ancient
     ((,undy-class (:foreground ,undy-blue
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :weight bold))))

   `(gnus-summary-high-read
     ((,undy-class (:foreground ,undy-green
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-green
                                        :weight bold))))

   `(gnus-summary-high-ticked
     ((,undy-class (:foreground ,undy-orange
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-orange
                                        :weight bold))))

   `(gnus-summary-high-unread
     ((,undy-class (:foreground ,undy-foreground
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :weight bold))))

   `(gnus-summary-low-ancient
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(gnus-summary-low-read
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(gnus-summary-low-ticked
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(gnus-summary-low-unread
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(gnus-summary-normal-ancient
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(gnus-summary-normal-read
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(gnus-summary-normal-ticked
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(gnus-summary-normal-unread
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(gnus-summary-selected
     ((,undy-class (:foreground ,undy-yellow
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight bold))))

   `(gnus-cite-1
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(gnus-cite-2
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(gnus-cite-3
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(gnus-cite-4
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(gnus-cite-5
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(gnus-cite-6
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(gnus-cite-7
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(gnus-cite-8
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(gnus-cite-9
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(gnus-cite-10
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(gnus-cite-11
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(gnus-group-news-1-empty
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(gnus-group-news-2-empty
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(gnus-group-news-3-empty
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(gnus-group-news-4-empty
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(gnus-group-news-5-empty
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(gnus-group-news-6-empty
     ((,undy-class (:foreground ,undy-blue-lc))
      (,undy-256-class (:foreground ,undy-256-blue-lc))))

   `(gnus-group-news-low-empty
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(gnus-signature
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(gnus-x-face
     ((,undy-class (:background ,undy-foreground
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-foreground
                                        :foreground ,undy-256-background))))


   ;; helm
   `(helm-apt-deinstalled
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(helm-apt-installed
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(helm-bookmark-directory
     ((,undy-class (:inherit helm-ff-directory))
      (,undy-256-class (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(helm-bookmark-gnus
     ((,undy-class (:foreground ,undy-cyan))
      (,undy-256-class (:foreground ,undy-256-cyan))))

   `(helm-bookmark-info
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(helm-bookmark-man
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   `(helm-bookmark-w3m
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(helm-bookmarks-su
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(helm-buffer-file
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(helm-buffer-directory
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(helm-buffer-process
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(helm-buffer-saved-out
     ((,undy-class (:foreground ,undy-red
                                   :background ,undy-background
                                   :inverse-video t))
      (,undy-256-class (:foreground ,undy-256-red
                                        :background ,undy-256-background
                                        :inverse-video t))))

   `(helm-buffer-size
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(helm-candidate-number
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-emphasis
                                   :bold t))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-emphasis
                                        :bold t))))

   `(helm-ff-directory
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(helm-ff-executable
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(helm-ff-file
     ((,undy-class (:background ,undy-background
                                   :foreground ,undy-foreground))
      (,undy-256-class (:background ,undy-256-background
                                        :foreground ,undy-256-foreground))))

   `(helm-ff-invalid-symlink
     ((,undy-class (:background ,undy-background
                                   :foreground ,undy-orange
                                   :slant italic))
      (,undy-256-class (:background ,undy-256-background
                                        :foreground ,undy-256-orange
                                        :slant italic))))

   `(helm-ff-prefix
     ((,undy-class (:background ,undy-green
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-green
                                        :foreground ,undy-256-background))))

   `(helm-ff-symlink
     ((,undy-class (:foreground ,undy-cyan))
      (,undy-256-class (:foreground ,undy-256-cyan))))

   `(helm-grep-file
     ((,undy-class (:foreground ,undy-cyan
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-cyan
                                        :underline t))))

   `(helm-grep-finish
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(helm-grep-lineno
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(helm-grep-match
     ((,undy-class (:inherit helm-match)))
     ((,undy-256-class (:inherit helm-match))))

   `(helm-grep-running
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(helm-header
     ((,undy-class (:inherit header-line))
      (,undy-256-class (:inherit terminal-header-line))))

   `(helm-lisp-completion-info
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(helm-lisp-show-completion
     ((,undy-class (:foreground ,undy-yellow
                                   :background ,undy-highlight-line
                                   :bold t))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :background ,undy-256-highlight-line
                                        :bold t))))

   `(helm-M-x-key
     ((,undy-class (:foreground ,undy-orange
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-orange
                                        :underline t))))

   `(helm-moccur-buffer
     ((,undy-class (:foreground ,undy-cyan
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-cyan
                                        :underline t))))

   `(helm-match
     ((,undy-class (:foreground ,undy-green :inherit bold))
      (,undy-256-class (:foreground ,undy-256-green :inherit bold))))

   `(helm-match-item
     ((,undy-class (:inherit helm-match))
      (,undy-256-class (:inherit helm-match))))

   `(helm-selection
     ((,undy-class (:background ,undy-highlight
                                   :inherit bold
                                   :underline nil))
      (,undy-256-class (:background ,undy-256-highlight
                                        :inherit bold
                                        :underline nil))))

   `(helm-selection-line
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-emphasis
                                   :underline nil))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-emphasis
                                        :underline nil))))

   `(helm-separator
     ((,undy-class (:foreground ,undy-gray))
      (,undy-256-class (:foreground ,undy-256-gray))))

   `(helm-source-header
     ((,undy-class (:background ,undy-violet-l
                                   :foreground ,undy-background
                                   :underline nil))
      (,undy-256-class (:background ,undy-256-violet-l
                                        :foreground ,undy-256-background
                                        :underline nil))))

   `(helm-swoop-target-line-face
     ((,undy-class (:background ,undy-highlight-line))
      (,undy-256-class (:background ,undy-256-highlight-line))))

   `(helm-swoop-target-line-block-face
     ((,undy-class (:background ,undy-highlight-line))
      (,undy-256-class (:background ,undy-256-highlight-line))))

   `(helm-swoop-target-word-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(helm-time-zone-current
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(helm-time-zone-home
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(helm-visible-mark
     ((,undy-class (:background ,undy-background
                                   :foreground ,undy-magenta :bold t))
      (,undy-256-class (:background ,undy-256-background
                                        :foreground ,undy-256-magenta :bold t))))

   ;; helm-ls-git
   `(helm-ls-git-modified-not-staged-face
     ((,undy-class :foreground ,undy-blue)
      (,undy-256-class  :foreground ,undy-256-blue)))

   `(helm-ls-git-modified-and-staged-face
     ((,undy-class :foreground ,undy-blue-l)
      (,undy-256-class  :foreground ,undy-256-blue-l)))

   `(helm-ls-git-renamed-modified-face
     ((,undy-class :foreground ,undy-blue-l)
      (,undy-256-class  :foreground ,undy-256-blue-l)))

   `(helm-ls-git-untracked-face
     ((,undy-class :foreground ,undy-orange)
      (,undy-256-class  :foreground ,undy-256-orange)))

   `(helm-ls-git-added-copied-face
     ((,undy-class :foreground ,undy-green)
      (,undy-256-class  :foreground ,undy-256-green)))

   `(helm-ls-git-added-modified-face
     ((,undy-class :foreground ,undy-green-l)
      (,undy-256-class  :foreground ,undy-256-green-l)))

   `(helm-ls-git-deleted-not-staged-face
     ((,undy-class :foreground ,undy-red)
      (,undy-256-class  :foreground ,undy-256-red)))

   `(helm-ls-git-deleted-and-staged-face
     ((,undy-class :foreground ,undy-red-l)
      (,undy-256-class  :foreground ,undy-256-red-l)))

   `(helm-ls-git-conflict-face
     ((,undy-class :foreground ,undy-yellow)
      (,undy-256-class  :foreground ,undy-256-yellow)))

   ;; hi-lock-mode
   `(hi-yellow
     ((,undy-class (:foreground ,undy-yellow-lc
                                   :background ,undy-yellow-hc))
      (,undy-256-class (:foreground ,undy-256-yellow-lc
                                        :background ,undy-256-yellow-hc))))

   `(hi-pink
     ((,undy-class (:foreground ,undy-magenta-lc
                                   :background ,undy-magenta-hc))
      (,undy-256-class (:foreground ,undy-256-magenta-lc
                                        :background ,undy-256-magenta-hc))))

   `(hi-green
     ((,undy-class (:foreground ,undy-green-lc
                                   :background ,undy-green-hc))
      (,undy-256-class (:foreground ,undy-256-green-lc
                                        :background ,undy-256-green-hc))))

   `(hi-blue
     ((,undy-class (:foreground ,undy-blue-lc
                                   :background ,undy-blue-hc))
      (,undy-256-class (:foreground ,undy-256-blue-lc
                                        :background ,undy-256-blue-hc))))

   `(hi-black-b
     ((,undy-class (:foreground ,undy-emphasis
                                   :background ,undy-background
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :background ,undy-256-background
                                        :weight bold))))

   `(hi-blue-b
     ((,undy-class (:foreground ,undy-blue-lc
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-blue-lc
                                        :weight bold))))

   `(hi-green-b
     ((,undy-class (:foreground ,undy-green-lc
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-green-lc
                                        :weight bold))))

   `(hi-red-b
     ((,undy-class (:foreground ,undy-red
                                   :weight bold))))

   `(hi-black-hb
     ((,undy-class (:foreground ,undy-emphasis
                                   :background ,undy-background
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :background ,undy-256-background
                                        :weight bold))))

   ;; highlight-changes
   `(highlight-changes
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(highlight-changes-delete
     ((,undy-class (:foreground ,undy-red
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-red
                                        :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,undy-class (:background ,undy-gray))
      (,undy-256-class (:background ,undy-256-gray))))

   `(highlight-indentation-current-column-face
     ((,undy-class (:background ,undy-gray))
      (,undy-256-class (:background ,undy-256-gray))))

   ;; highlight-symbol
   `(highlight-symbol-face
     ((,undy-class (:background ,undy-highlight))
      (,undy-256-class (:background ,undy-256-highlight))))

   ;; hl-line-mode
   `(hl-line
     ((,undy-class (:background ,undy-highlight-line))
      (,undy-256-class (:background ,undy-256-highlight-line))))

   `(hl-line-face
     ((,undy-class (:background ,undy-highlight-line))
      (,undy-256-class (:background ,undy-256-highlight-line))))

   ;; ido-mode
   `(ido-first-match
     ((,undy-class (:foreground ,undy-yellow
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight normal))))

   `(ido-only-match
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-yellow
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-yellow
                                        :weight normal))))

   `(ido-subdir
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(ido-incomplete-regexp
     ((,undy-class (:foreground ,undy-red
                                   :weight bold ))
      (,undy-256-class (:foreground ,undy-256-red
                                        :weight bold ))))

   `(ido-indicator
     ((,undy-class (:background ,undy-red
                                   :foreground ,undy-background
                                   :width condensed))
      (,undy-256-class (:background ,undy-256-red
                                        :foreground ,undy-256-background
                                        :width condensed))))

   `(ido-virtual
     ((,undy-class (:foreground ,undy-cyan))
      (,undy-256-class (:foreground ,undy-256-cyan))))

   ;; info
   `(info-header-xref
     ((,undy-class (:foreground ,undy-green
                                   :inherit bold
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-green
                                        :inherit bold
                                        :underline t))))

   `(info-menu
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(info-node
     ((,undy-class (:foreground ,undy-violet
                                   :inherit bold))
      (,undy-256-class (:foreground ,undy-256-violet
                                        :inherit bold))))

   `(info-quoted-name
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(info-reference-item
     ((,undy-class (:background nil
                                   :underline t
                                   :inherit bold))
      (,undy-256-class (:background nil
                                        :underline t
                                        :inherit bold))))

   `(info-string
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(info-title-1
     ((,undy-class (:height ,undy-height-plus-4))
      (,undy-256-class (:height ,undy-height-plus-4))))

   `(info-title-2
     ((,undy-class (:height ,undy-height-plus-3))
      (,undy-256-class (:height ,undy-height-plus-3))))

   `(info-title-3
     ((,undy-class (:height ,undy-height-plus-2))
      (,undy-256-class (:height ,undy-height-plus-2))))

   `(info-title-4
     ((,undy-class (:height ,undy-height-plus-1))
      (,undy-256-class (:height ,undy-height-plus-1))))

   ;; ivy
   `(ivy-current-match
     ((,undy-class (:background ,undy-gray :inherit bold))
      (,undy-256-class (:background ,undy-gray-l :inherit bold))))

   `(ivy-minibuffer-match-face-1
     ((,undy-class (:inherit bold))
      (,undy-256-class (:inherit bold))))

   `(ivy-minibuffer-match-face-2
     ((,undy-class (:foreground ,undy-violet
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-violet
                                        :underline t))))

   `(ivy-minibuffer-match-face-3
     ((,undy-class (:foreground ,undy-green
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-green
                                        :underline t))))

   `(ivy-minibuffer-match-face-4
     ((,undy-class (:foreground ,undy-yellow
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :underline t))))

   `(ivy-remote
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(swiper-line-face
     ((,undy-class (:background ,undy-highlight-line))))

   `(swiper-match-face-1
     ((,undy-class (:background ,undy-gray-d))))

   `(swiper-match-face-2
     ((,undy-class (:background ,undy-green))))

   `(swiper-match-face-3
     ((,undy-class (:background ,undy-orange))))

   `(swiper-match-face-4
     ((,undy-class (:background ,undy-magenta))))

   ;; jabber
   `(jabber-activity-face
     ((,undy-class (:weight bold
                               :foreground ,undy-red))
      (,undy-256-class (:weight bold
                                    :foreground ,undy-256-red))))

   `(jabber-activity-personal-face
     ((,undy-class (:weight bold
                               :foreground ,undy-blue))
      (,undy-256-class (:weight bold
                                    :foreground ,undy-256-blue))))

   `(jabber-chat-error
     ((,undy-class (:weight bold
                               :foreground ,undy-red))
      (,undy-256-class (:weight bold
                                    :foreground ,undy-256-red))))

   `(jabber-chat-prompt-foreign
     ((,undy-class (:weight bold
                               :foreground ,undy-red))
      (,undy-256-class (:weight bold
                                    :foreground ,undy-256-red))))

   `(jabber-chat-prompt-local
     ((,undy-class (:weight bold
                               :foreground ,undy-blue))
      (,undy-256-class (:weight bold
                                    :foreground ,undy-256-blue))))

   `(jabber-chat-prompt-system
     ((,undy-class (:weight bold
                               :foreground ,undy-green))
      (,undy-256-class (:weight bold
                                    :foreground ,undy-256-green))))

   `(jabber-chat-text-foreign
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(jabber-chat-text-local
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(jabber-chat-rare-time-face
     ((,undy-class (:underline t
                                  :foreground ,undy-green))
      (,undy-256-class (:underline t
                                       :foreground ,undy-256-green))))

   `(jabber-roster-user-away
     ((,undy-class (:slant italic
                              :foreground ,undy-green))
      (,undy-256-class (:slant italic
                                   :foreground ,undy-256-green))))

   `(jabber-roster-user-chatty
     ((,undy-class (:weight bold
                               :foreground ,undy-orange))
      (,undy-256-class (:weight bold
                                    :foreground ,undy-256-orange))))

   `(jabber-roster-user-dnd
     ((,undy-class (:slant italic
                              :foreground ,undy-red))
      (,undy-256-class (:slant italic
                                   :foreground ,undy-256-red))))

   `(jabber-roster-user-error
     ((,undy-class (:weight light
                               :slant italic
                               :foreground ,undy-red))
      (,undy-256-class (:weight light
                                    :slant italic
                                    :foreground ,undy-256-red))))

   `(jabber-roster-user-offline
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(jabber-roster-user-online
     ((,undy-class (:weight bold
                               :foreground ,undy-blue))
      (,undy-256-class (:weight bold
                                    :foreground ,undy-256-blue))))

   `(jabber-roster-user-xa
     ((,undy-class (:slant italic
                              :foreground ,undy-magenta))
      (,undy-256-class (:slant italic
                                   :foreground ,undy-256-magenta))))

   ;; js2-mode colors
   `(js2-error
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(js2-external-variable
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(js2-function-call
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(js2-function-param
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(js2-instance-member
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   `(js2-jsdoc-html-tag-delimiter
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(js2-jsdoc-html-tag-name
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(js2-jsdoc-tag
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   `(js2-jsdoc-type
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(js2-jsdoc-value
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(js2-magic-paren
     ((,undy-class (:underline t))
      (,undy-256-class (:underline t))))

   `(js2-object-property
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(js2-private-function-call
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   `(js2-private-member
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(js2-warning
     ((,undy-class (:underline ,undy-orange))
      (,undy-256-class (:underline ,undy-256-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,undy-class (:inherit bold))
      (,undy-256-class (:inherit bold))))

   ;; linum-mode
   `(linum
     ((,undy-class (:foreground ,undy-line-number
                                   :background ,undy-fringe-bg
                                   :inherit default
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-line-number
                                        :background ,undy-256-fringe-bg
                                        :inherit default
                                        :underline nil))))

   ;; line-number (>= Emacs26)
   `(line-number
     ((,undy-class (:foreground ,undy-line-number
                                   :background ,undy-fringe-bg
                                   :inherit default
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-line-number
                                        :background ,undy-256-fringe-bg
                                        :inherit default
                                        :underline nil))))
   `(line-number-current-line
     ((,undy-class (:foreground ,undy-foreground
                                   :background ,undy-fringe-bg
                                   :inherit default
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :background ,undy-256-fringe-bg
                                        :inherit default
                                        :underline nil))))

   ;; linum-relative-current-face
   `(linum-relative-current-face
     ((,undy-class (:foreground ,undy-line-number
                                   :background ,undy-highlight-line
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-line-number
                                        :background ,undy-256-highlight-line
                                        :underline nil))))

   ;; lusty-explorer
   `(lusty-directory-face
     ((,undy-class (:inherit diundy-red-directory))
      (,undy-256-class (:inherit diundy-red-directory))))

   `(lusty-file-face
     ((,undy-class nil)
      (,undy-256-class  nil)))

   `(lusty-match-face
     ((,undy-class (:inherit ido-first-match))
      (,undy-256-class (:inherit ido-first-match))))

   `(lusty-slash-face
     ((,undy-class (:foreground ,undy-cyan
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-cyan
                                        :weight bold))))

   ;; magit
   ;;
   ;; TODO: Add supports for all magit faces
   ;; https://github.com/magit/magit/search?utf8=%E2%9C%93&q=face
   ;;
   `(magit-diff-added
     ((,undy-class (:foreground ,undy-green
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-green
                                        :background ,undy-256-background))))

   `(magit-diff-added-highlight
     ((,undy-class (:foreground ,undy-green
                                   :background ,undy-highlight-line))
      (,undy-256-class (:foreground ,undy-256-green
                                        :background ,undy-256-highlight-line))))

   `(magit-diff-removed
     ((,undy-class (:foreground ,undy-red
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-red
                                        :background ,undy-256-background))))

   `(magit-diff-removed-highlight
     ((,undy-class (:foreground ,undy-red
                                   :background ,undy-highlight-line))
      (,undy-256-class (:foreground ,undy-256-red
                                        :background ,undy-256-highlight-line))))

   `(magit-section-title
     ((,undy-class (:foreground ,undy-yellow
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight bold))))

   `(magit-branch
     ((,undy-class (:foreground ,undy-orange
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-orange
                                        :weight bold))))

   `(magit-item-highlight
     ((,undy-class (:background ,undy-highlight-line
                                   :weight unspecified))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :weight unspecified))))

   `(magit-log-author
     ((,undy-class (:foreground ,undy-cyan))
      (,undy-256-class (:foreground ,undy-256-cyan))))

   `(magit-log-graph
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(magit-log-head-label-bisect-bad
     ((,undy-class (:background ,undy-red-hc
                                   :foreground ,undy-red-lc
                                   :box 1))
      (,undy-256-class (:background ,undy-256-red-hc
                                        :foreground ,undy-256-red-lc
                                        :box 1))))

   `(magit-log-head-label-bisect-good
     ((,undy-class (:background ,undy-green-hc
                                   :foreground ,undy-green-lc
                                   :box 1))
      (,undy-256-class (:background ,undy-256-green-hc
                                        :foreground ,undy-256-green-lc
                                        :box 1))))

   `(magit-log-head-label-default
     ((,undy-class (:background ,undy-highlight-line
                                   :box 1))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :box 1))))

   `(magit-log-head-label-local
     ((,undy-class (:background ,undy-blue-lc
                                   :foreground ,undy-blue-hc
                                   :box 1))
      (,undy-256-class (:background ,undy-256-blue-lc
                                        :foreground ,undy-256-blue-hc
                                        :box 1))))

   `(magit-log-head-label-patches
     ((,undy-class (:background ,undy-red-lc
                                   :foreground ,undy-red-hc
                                   :box 1))
      (,undy-256-class (:background ,undy-256-red-lc
                                        :foreground ,undy-256-red-hc
                                        :box 1))))

   `(magit-log-head-label-remote
     ((,undy-class (:background ,undy-green-lc
                                   :foreground ,undy-green-hc
                                   :box 1))
      (,undy-256-class (:background ,undy-256-green-lc
                                        :foreground ,undy-256-green-hc
                                        :box 1))))

   `(magit-log-head-label-tags
     ((,undy-class (:background ,undy-yellow-lc
                                   :foreground ,undy-yellow-hc
                                   :box 1))
      (,undy-256-class (:background ,undy-256-yellow-lc
                                        :foreground ,undy-256-yellow-hc
                                        :box 1))))

   `(magit-log-sha1
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   ;; man
   `(Man-overstrike
     ((,undy-class (:foreground ,undy-blue
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :weight bold))))

   `(Man-reverse
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(Man-underline
     ((,undy-class (:foreground ,undy-green :underline t))
      (,undy-256-class (:foreground ,undy-256-green :underline t))))

   ;; monky
   `(monky-section-title
     ((,undy-class (:foreground ,undy-yellow
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight bold))))

   `(monky-diff-add
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(monky-diff-del
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(markdown-header-face-1
     ((,undy-class (:inherit markdown-header-face
                                :height ,undy-height-plus-4))
      (,undy-256-class (:inherit markdown-header-face
                                     :height ,undy-height-plus-4))))

   `(markdown-header-face-2
     ((,undy-class (:inherit markdown-header-face
                                :height ,undy-height-plus-3))
      (,undy-256-class (:inherit markdown-header-face
                                     :height ,undy-height-plus-3))))

   `(markdown-header-face-3
     ((,undy-class (:inherit markdown-header-face
                                :height ,undy-height-plus-2))
      (,undy-256-class (:inherit markdown-header-face
                                     :height ,undy-height-plus-2))))

   `(markdown-header-face-4
     ((,undy-class (:inherit markdown-header-face
                                :height ,undy-height-plus-1))
      (,undy-256-class (:inherit markdown-header-face
                                     :height ,undy-height-plus-1))))

   `(markdown-header-face-5
     ((,undy-class (:inherit markdown-header-face))
      (,undy-256-class (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,undy-class (:inherit markdown-header-face))
      (,undy-256-class (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(message-header-name
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(message-header-other
     ((,undy-class (:foreground ,undy-foreground
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :weight normal))))

   `(message-header-to
     ((,undy-class (:foreground ,undy-foreground
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :weight normal))))

   `(message-header-cc
     ((,undy-class (:foreground ,undy-foreground
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :weight normal))))

   `(message-header-newsgroups
     ((,undy-class (:foreground ,undy-yellow
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight bold))))

   `(message-header-subject
     ((,undy-class (:foreground ,undy-cyan
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-cyan
                                        :weight normal))))

   `(message-header-xheader
     ((,undy-class (:foreground ,undy-cyan))
      (,undy-256-class (:foreground ,undy-256-cyan))))

   `(message-mml
     ((,undy-class (:foreground ,undy-yellow
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight bold))))

   `(message-separator
     ((,undy-class (:foreground ,undy-comments
                                   :slant italic))
      (,undy-256-class (:foreground ,undy-256-comments
                                        :slant italic))))

   ;; mew
   `(mew-face-header-subject
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(mew-face-header-from
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(mew-face-header-date
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(mew-face-header-to
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(mew-face-header-key
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(mew-face-header-private
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(mew-face-header-important
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(mew-face-header-marginal
     ((,undy-class (:foreground ,undy-foreground
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :weight bold))))

   `(mew-face-header-warning
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(mew-face-header-xmew
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(mew-face-header-xmew-bad
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(mew-face-body-url
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(mew-face-body-comment
     ((,undy-class (:foreground ,undy-foreground
                                   :slant italic))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :slant italic))))

   `(mew-face-body-cite1
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(mew-face-body-cite2
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(mew-face-body-cite3
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(mew-face-body-cite4
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(mew-face-body-cite5
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(mew-face-mark-review
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(mew-face-mark-escape
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(mew-face-mark-delete
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(mew-face-mark-unlink
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(mew-face-mark-refile
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(mew-face-mark-unread
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(mew-face-eof-message
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(mew-face-eof-part
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   ;; mingus
   `(mingus-directory-face
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(mingus-pausing-face
     ((,undy-class (:foreground ,undy-magenta))
      (,undy-256-class (:foreground ,undy-256-magenta))))

   `(mingus-playing-face
     ((,undy-class (:foreground ,undy-cyan))
      (,undy-256-class (:foreground ,undy-256-cyan))))

   `(mingus-playlist-face
     ((,undy-class (:foreground ,undy-cyan ))
      (,undy-256-class (:foreground ,undy-256-cyan ))))

   `(mingus-song-file-face
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(mingus-stopped-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   ;; mmm
   `(mmm-init-submode-face
     ((,undy-class (:background ,undy-violet-d))
      (,undy-256-class (:background ,undy-256-violet-d))))

   `(mmm-cleanup-submode-face
     ((,undy-class (:background ,undy-orange-d))
      (,undy-256-class (:background ,undy-256-orange-d))))

   `(mmm-declaration-submode-face
     ((,undy-class (:background ,undy-cyan-d))
      (,undy-256-class (:background ,undy-256-cyan-d))))

   `(mmm-comment-submode-face
     ((,undy-class (:background ,undy-blue-d))
      (,undy-256-class (:background ,undy-256-blue-d))))

   `(mmm-output-submode-face
     ((,undy-class (:background ,undy-red-d))
      (,undy-256-class (:background ,undy-256-red-d))))

   `(mmm-special-submode-face
     ((,undy-class (:background ,undy-green-d))
      (,undy-256-class (:background ,undy-256-green-d))))

   `(mmm-code-submode-face
     ((,undy-class (:background ,undy-gray))
      (,undy-256-class (:background ,undy-256-gray))))

   `(mmm-default-submode-face
     ((,undy-class (:background ,undy-gray-d))
      (,undy-256-class (:background ,undy-256-gray-d))))

   ;; moccur
   `(moccur-current-line-face
     ((,undy-class (:underline t))
      (,undy-256-class (:underline t))))

   `(moccur-edit-done-face
     ((,undy-class (:foreground ,undy-comments
                                   :background ,undy-background
                                   :slant italic))
      (,undy-256-class (:foreground ,undy-256-comments
                                        :background ,undy-256-background
                                        :slant italic))))

   `(moccur-edit-face
     ((,undy-class (:background ,undy-yellow
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-yellow
                                        :foreground ,undy-256-background))))

   `(moccur-edit-file-face
     ((,undy-class (:background ,undy-highlight-line))
      (,undy-256-class (:background ,undy-256-highlight-line))))

   `(moccur-edit-reject-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(moccur-face
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-emphasis
                                   :weight bold))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-emphasis
                                        :weight bold))))

   `(search-buffers-face
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-emphasis
                                   :weight bold))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-emphasis
                                        :weight bold))))

   `(search-buffers-header-face
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-yellow
                                   :weight bold))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-yellow
                                        :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face
     ((,undy-class (:foreground ,undy-green
                                   :slant italic
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-2-face
     ((,undy-class (:foreground ,undy-blue
                                   :slant italic
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-3-face
     ((,undy-class (:foreground ,undy-orange
                                   :slant italic
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-orange
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-4-face
     ((,undy-class (:foreground ,undy-yellow
                                   :slant italic
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-5-face
     ((,undy-class (:foreground ,undy-cyan
                                   :slant italic
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-cyan
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-6-face
     ((,undy-class (:foreground ,undy-green
                                   :slant italic
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-7-face
     ((,undy-class (:foreground ,undy-blue
                                   :slant italic
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-flagged-face
     ((,undy-class (:foreground ,undy-magenta
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-magenta
                                        :weight bold))))

   `(mu4e-view-url-number-face
     ((,undy-class (:foreground ,undy-yellow
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight normal))))

   `(mu4e-warning-face
     ((,undy-class (:foreground ,undy-red
                                   :slant normal
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-red
                                        :slant normal
                                        :weight bold))))

   `(mu4e-header-highlight-face
     ((,undy-class (:inherit unspecified
                                :foreground unspecified
                                :background ,undy-highlight-line
                                :underline ,undy-emphasis
                                :weight normal))
      (,undy-256-class (:inherit unspecified
                                     :foreground unspecified
                                     :background ,undy-256-highlight-line
                                     :underline ,undy-256-emphasis
                                     :weight normal))))


   `(mu4e-draft-face
     ((,undy-class (:inherit font-lock-string-face))
      (,undy-256-class (:inherit font-lock-string-face))))

   `(mu4e-footer-face
     ((,undy-class (:inherit font-lock-comment-face))
      (,undy-256-class (:inherit font-lock-comment-face))))

   `(mu4e-forwarded-face
     ((,undy-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,undy-256-class (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-header-face
     ((,undy-class (:inherit default))
      (,undy-256-class (:inherit default))))

   `(mu4e-header-marks-face
     ((,undy-class (:inherit font-lock-preprocessor-face))
      (,undy-256-class (:inherit font-lock-preprocessor-face))))

   `(mu4e-header-title-face
     ((,undy-class (:inherit font-lock-type-face))
      (,undy-256-class (:inherit font-lock-type-face))))

   `(mu4e-highlight-face
     ((,undy-class (:inherit font-lock-pseudo-keyword-face
                                :weight bold))
      (,undy-256-class (:inherit font-lock-pseudo-keyword-face
                                     :weight bold))))

   `(mu4e-moved-face
     ((,undy-class (:inherit font-lock-comment-face
                                :slant italic))
      (,undy-256-class (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-ok-face
     ((,undy-class (:inherit font-lock-comment-face
                                :slant normal
                                :weight bold))
      (,undy-256-class (:inherit font-lock-comment-face
                                     :slant normal
                                     :weight bold))))

   `(mu4e-replied-face
     ((,undy-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,undy-256-class (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-system-face
     ((,undy-class (:inherit font-lock-comment-face
                                :slant italic))
      (,undy-256-class (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-title-face
     ((,undy-class (:inherit font-lock-type-face
                                :weight bold))
      (,undy-256-class (:inherit font-lock-type-face
                                     :weight bold))))

   `(mu4e-trashed-face
     ((,undy-class (:inherit font-lock-comment-face
                                :strike-through t))
      (,undy-256-class (:inherit font-lock-comment-face
                                     :strike-through t))))

   `(mu4e-unread-face
     ((,undy-class (:inherit font-lock-keyword-face
                                :weight bold))
      (,undy-256-class (:inherit font-lock-keyword-face
                                     :weight bold))))

   `(mu4e-view-attach-number-face
     ((,undy-class (:inherit font-lock-variable-name-face
                                :weight bold))
      (,undy-256-class (:inherit font-lock-variable-name-face
                                     :weight bold))))

   `(mu4e-view-contact-face
     ((,undy-class (:foreground ,undy-foreground
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :weight normal))))

   `(mu4e-view-header-key-face
     ((,undy-class (:inherit message-header-name
                                :weight normal))
      (,undy-256-class (:inherit message-header-name
                                     :weight normal))))

   `(mu4e-view-header-value-face
     ((,undy-class (:foreground ,undy-cyan
                                   :weight normal
                                   :slant normal))
      (,undy-256-class (:foreground ,undy-256-cyan
                                        :weight normal
                                        :slant normal))))

   `(mu4e-view-link-face
     ((,undy-class (:inherit link))
      (,undy-256-class (:inherit link))))

   `(mu4e-view-special-header-value-face
     ((,undy-class (:foreground ,undy-blue
                                   :weight normal
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,undy-class (:background ,undy-highlight-line))
      (,undy-256-class (:background ,undy-256-highlight-line))))

   ;; nav
   `(nav-face-heading
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(nav-face-button-num
     ((,undy-class (:foreground ,undy-cyan))
      (,undy-256-class (:foreground ,undy-256-cyan))))

   `(nav-face-dir
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(nav-face-hdir
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(nav-face-file
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(nav-face-hfile
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,undy-class (:background ,undy-highlight-line))
      (,undy-256-class (:background ,undy-256-highlight-line))))

   ;; neo-tree
   `(neo-banner-face
     ((,undy-class (:foreground ,undy-blue
                                   :background ,undy-background
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :background ,undy-256-background
                                        :weight bold))))


   `(neo-header-face
     ((,undy-class (:foreground ,undy-emphasis
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :background ,undy-256-background))))

   `(neo-root-dir-face
     ((,undy-class (:foreground ,undy-green
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-green
                                        :background ,undy-256-background))))

   `(neo-dir-link-face
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :background ,undy-256-background))))

   `(neo-file-link-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(neo-button-face
     ((,undy-class (:underline nil))
      (,undy-256-class (:underline nil))))

   `(neo-expand-btn-face
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(neo-vc-default-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(neo-vc-user-face
     ((,undy-class (:foreground ,undy-red
                                   :slant italic))
      (,undy-256-class (:foreground ,undy-256-red
                                        :slant italic))))

   `(neo-vc-up-to-date-face
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(neo-vc-edited-face
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(neo-vc-needs-update-face
     ((,undy-class (:underline t))
      (,undy-256-class (:underline t))))

   `(neo-vc-needs-merge-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(neo-vc-unlocked-changes-face
     ((,undy-class (:foreground ,undy-red
                                   :background ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-red
                                        :background ,undy-256-comments))))

   `(neo-vc-added-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(neo-vc-removed-face
     ((,undy-class (:strike-through t))
      (,undy-256-class (:strike-through t))))

   `(neo-vc-conflict-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(neo-vc-missing-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(neo-vc-ignored-face
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   ;; adoc-mode / markup
   `(markup-meta-face
     ((,undy-class (:foreground ,undy-gray-l))
      (,undy-256-class (:foreground ,undy-256-gray-l))))

   `(markup-table-face
     ((,undy-class (:foreground ,undy-blue-hc
                                   :background ,undy-blue-lc))
      (,undy-256-class (:foreground ,undy-256-blue-hc
                                        :background ,undy-256-blue-lc))))

   `(markup-verbatim-face
     ((,undy-class (:background ,undy-orange-lc))
      (,undy-256-class (:background ,undy-256-orange-lc))))

   `(markup-list-face
     ((,undy-class (:foreground ,undy-violet-hc
                                   :background ,undy-violet-lc))
      (,undy-256-class (:foreground ,undy-256-violet-hc
                                        :background ,undy-256-violet-lc))))

   `(markup-replacement-face
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   `(markup-complex-replacement-face
     ((,undy-class (:foreground ,undy-violet-hc
                                   :background ,undy-violet-lc))
      (,undy-256-class (:foreground ,undy-256-violet-hc
                                        :background ,undy-256-violet-lc))))

   `(markup-gen-face
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(markup-secondary-text-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   ;; org-mode
   `(org-agenda-structure
     ((,undy-class (:foreground ,undy-emphasis
                                   :background ,undy-highlight-line
                                   :weight bold
                                   :slant normal
                                   :inverse-video nil
                                   :height ,undy-height-plus-1
                                   :underline nil
                                   :box (:line-width 2 :color ,undy-background)))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :background ,undy-256-highlight-line
                                        :weight bold
                                        :slant normal
                                        :inverse-video nil
                                        :height ,undy-height-plus-1
                                        :underline nil
                                        :box (:line-width 2 :color ,undy-256-background)))))

   `(org-agenda-calendar-event
     ((,undy-class (:foreground ,undy-emphasis))
      (,undy-256-class (:foreground ,undy-256-emphasis))))

   `(org-agenda-calendar-sexp
     ((,undy-class (:foreground ,undy-foreground
                                   :slant italic))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :slant italic))))

   `(org-agenda-date
     ((,undy-class (:foreground ,undy-comments
                                   :background ,undy-background
                                   :weight normal
                                   :inverse-video nil
                                   :overline nil
                                   :slant normal
                                   :height 1.0
                                   :box (:line-width 2 :color ,undy-background)))
      (,undy-256-class (:foreground ,undy-256-comments
                                        :background ,undy-256-background
                                        :weight normal
                                        :inverse-video nil
                                        :overline nil
                                        :slant normal
                                        :height 1.0
                                        :box (:line-width 2 :color ,undy-256-background)))) t)

   `(org-agenda-date-weekend
     ((,undy-class (:inherit org-agenda-date
                                :inverse-video nil
                                :background unspecified
                                :foreground ,undy-comments
                                :weight unspecified
                                :underline t
                                :overline nil
                                :box unspecified))
      (,undy-256-class (:inherit org-agenda-date
                                     :inverse-video nil
                                     :background unspecified
                                     :foreground ,undy-256-comments
                                     :weight unspecified
                                     :underline t
                                     :overline nil
                                     :box unspecified))) t)

   `(org-agenda-date-today
     ((,undy-class (:inherit org-agenda-date
                                :inverse-video t
                                :weight bold
                                :underline unspecified
                                :overline nil
                                :box unspecified
                                :foreground ,undy-blue
                                :background ,undy-background))
      (,undy-256-class (:inherit org-agenda-date
                                     :inverse-video t
                                     :weight bold
                                     :underline unspecified
                                     :overline nil
                                     :box unspecified
                                     :foreground ,undy-256-blue
                                     :background ,undy-256-background))) t)

   `(org-agenda-done
     ((,undy-class (:foreground ,undy-comments
                                   :slant italic))
      (,undy-256-class (:foreground ,undy-256-comments
                                        :slant italic))) t)

   `(org-archived
     ((,undy-class (:foreground ,undy-comments
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-comments
                                        :weight normal))))

   `(org-block
     ((,undy-class (:foreground ,undy-emphasis
                                   :background ,undy-highlight-alt))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :background ,undy-256-highlight-alt))))

   `(org-block-background
     ((,undy-class (:background ,undy-highlight-alt))
      (,undy-256-class (:background ,undy-256-highlight-alt))))

   `(org-block-begin-line
     ((,undy-class (:foreground ,undy-comments
                                   :background ,undy-gray-d
                                   :slant italic))
      (,undy-256-class (:foreground ,undy-256-comments
                                        :background ,undy-256-gray-d
                                        :slant italic))))

   `(org-block-end-line
     ((,undy-class (:foreground ,undy-comments
                                   :background ,undy-gray-d
                                   :slant italic))
      (,undy-256-class (:foreground ,undy-256-comments
                                        :background ,undy-256-gray-d
                                        :slant italic))))

   `(org-checkbox
     ((,undy-class (:background ,undy-background
                                   :foreground ,undy-foreground
                                   :box (:line-width 1 :style released-button)))
      (,undy-256-class (:background ,undy-256-background
                                        :foreground ,undy-256-foreground
                                        :box (:line-width 1 :style released-button)))))

   `(org-code
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(org-date
     ((,undy-class (:foreground ,undy-blue
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :underline t))))

   `(org-done
     ((,undy-class (:weight bold
                               :foreground ,undy-green))
      (,undy-256-class (:weight bold
                                    :foreground ,undy-256-green))))

   `(org-ellipsis
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(org-formula
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(org-headline-done
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(org-hide
     ((,undy-class (:foreground ,undy-background))
      (,undy-256-class (:foreground ,undy-256-background))))

   `(org-level-1
     ((,undy-class (:inherit ,undy-pitch
                                :height ,undy-height-plus-4
                                :foreground ,undy-orange))
      (,undy-256-class (:inherit ,undy-pitch
                                     :height ,undy-height-plus-4
                                     :foreground ,undy-256-orange))))

   `(org-level-2
     ((,undy-class (:inherit ,undy-pitch
                                :height ,undy-height-plus-3
                                :foreground ,undy-green))
      (,undy-256-class (:inherit ,undy-pitch
                                     :height ,undy-height-plus-3
                                     :foreground ,undy-256-green))))

   `(org-level-3
     ((,undy-class (:inherit ,undy-pitch
                                :height ,undy-height-plus-2
                                :foreground ,undy-blue))
      (,undy-256-class (:inherit ,undy-pitch
                                     :height ,undy-height-plus-2
                                     :foreground ,undy-256-blue))))

   `(org-level-4
     ((,undy-class (:inherit ,undy-pitch
                                :height ,undy-height-plus-1
                                :foreground ,undy-yellow))
      (,undy-256-class (:inherit ,undy-pitch
                                     :height ,undy-height-plus-1
                                     :foreground ,undy-256-yellow))))

   `(org-level-5
     ((,undy-class (:inherit ,undy-pitch
                                :foreground ,undy-cyan))
      (,undy-256-class (:inherit ,undy-pitch
                                     :foreground ,undy-256-cyan))))

   `(org-level-6
     ((,undy-class (:inherit ,undy-pitch
                                :foreground ,undy-green))
      (,undy-256-class (:inherit ,undy-pitch
                                     :foreground ,undy-256-green))))

   `(org-level-7
     ((,undy-class (:inherit ,undy-pitch
                                :foreground ,undy-red))
      (,undy-256-class (:inherit ,undy-pitch
                                     :foreground ,undy-256-red))))

   `(org-level-8
     ((,undy-class (:inherit ,undy-pitch
                                :foreground ,undy-blue))
      (,undy-256-class (:inherit ,undy-pitch
                                     :foreground ,undy-256-blue))))

   `(org-link
     ((,undy-class (:foreground ,undy-blue
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :underline t))))

   `(org-sexp-date
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   `(org-scheduled
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(org-scheduled-previously
     ((,undy-class (:foreground ,undy-cyan))
      (,undy-256-class (:foreground ,undy-256-cyan))))

   `(org-scheduled-today
     ((,undy-class (:foreground ,undy-blue
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :weight normal))))

   `(org-special-keyword
     ((,undy-class (:foreground ,undy-comments
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-comments
                                        :weight bold))))

   `(org-table
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(org-tag
     ((,undy-class (:weight bold))
      (,undy-256-class (:weight bold))))

   `(org-time-grid
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(org-todo
     ((,undy-class (:foreground ,undy-red
                                   :weight bold)))
     ((,undy-256-class (:foreground ,undy-256-red
                                        :weight bold))))

   `(org-upcoming-deadline
     ((,undy-class (:foreground ,undy-yellow
                                   :weight normal
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(org-warning
     ((,undy-class (:foreground ,undy-orange
                                   :weight normal
                                   :underline nil))
      (,undy-256-class (:foreground ,undy-256-orange
                                        :weight normal
                                        :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,undy-class (:background ,undy-blue-lc
                                   :foreground ,undy-blue-hc))
      (,undy-256-class (:background ,undy-256-blue-lc
                                        :foreground ,undy-256-blue-hc))))

   `(org-habit-clear-future-face
     ((,undy-class (:background ,undy-blue-lc))
      (,undy-256-class (:background ,undy-256-blue-lc))))

   `(org-habit-ready-face
     ((,undy-class (:background ,undy-green-lc
                                   :foreground ,undy-green))
      (,undy-256-class (:background ,undy-256-green-lc
                                        :foreground ,undy-256-green))))

   `(org-habit-ready-future-face
     ((,undy-class (:background ,undy-green-lc))
      (,undy-256-class (:background ,undy-256-green-lc))))

   `(org-habit-alert-face
     ((,undy-class (:background ,undy-yellow
                                   :foreground ,undy-yellow-lc))
      (,undy-256-class (:background ,undy-256-yellow
                                        :foreground ,undy-256-yellow-lc))))

   `(org-habit-alert-future-face
     ((,undy-class (:background ,undy-yellow-lc))
      (,undy-256-class (:background ,undy-256-yellow-lc))))

   `(org-habit-overdue-face
     ((,undy-class (:background ,undy-red
                                   :foreground ,undy-red-lc))
      (,undy-256-class (:background ,undy-256-red
                                        :foreground ,undy-256-red-lc))))

   `(org-habit-overdue-future-face
     ((,undy-class (:background ,undy-red-lc))
      (,undy-256-class (:background ,undy-256-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(org-agenda-restriction-lock
     ((,undy-class (:background ,undy-yellow))
      (,undy-256-class (:background ,undy-256-yellow))))

   `(org-clock-overlay
     ((,undy-class (:background ,undy-yellow))
      (,undy-256-class (:background ,undy-256-yellow))))

   `(org-column
     ((,undy-class (:background ,undy-highlight-line
                                   :strike-through nil
                                   :underline nil
                                   :slant normal
                                   :weight normal
                                   :inherit default))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :strike-through nil
                                        :underline nil
                                        :slant normal
                                        :weight normal
                                        :inherit default))))

   `(org-column-title
     ((,undy-class (:background ,undy-highlight-line
                                   :underline t
                                   :weight bold))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :underline t
                                        :weight bold))))

   `(org-date-selected
     ((,undy-class (:foreground ,undy-red
                                   :inverse-video t))
      (,undy-256-class (:foreground ,undy-256-red
                                        :inverse-video t))))

   `(org-document-info
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(org-document-title
     ((,undy-class (:foreground ,undy-emphasis
                                   :weight bold
                                   :height ,undy-height-plus-4))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :weight bold
                                        :height ,undy-height-plus-4))))

   `(org-drawer
     ((,undy-class (:foreground ,undy-cyan))
      (,undy-256-class (:foreground ,undy-256-cyan))))

   `(org-footnote
     ((,undy-class (:foreground ,undy-magenta
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-magenta
                                        :underline t))))

   `(org-latex-and-export-specials
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(org-mode-line-clock-overrun
     ((,undy-class (:inherit mode-line))
      (,undy-256-class (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,undy-class (:inherit org-level-1))
      (,undy-256-class (:inherit org-level-1))))

   `(outline-2
     ((,undy-class (:inherit org-level-2))
      (,undy-256-class (:inherit org-level-2))))

   `(outline-3
     ((,undy-class (:inherit org-level-3))
      (,undy-256-class (:inherit org-level-3))))

   `(outline-4
     ((,undy-class (:inherit org-level-4))
      (,undy-256-class (:inherit org-level-4))))

   `(outline-5
     ((,undy-class (:inherit org-level-5))
      (,undy-256-class (:inherit org-level-5))))

   `(outline-6
     ((,undy-class (:inherit org-level-6))
      (,undy-256-class (:inherit org-level-6))))

   `(outline-7
     ((,undy-class (:inherit org-level-7))
      (,undy-256-class (:inherit org-level-7))))

   `(outline-8
     ((,undy-class (:inherit org-level-8))
      (,undy-256-class (:inherit org-level-8))))

   ;; parenface
   `(paren-face
     ((,undy-256-class (:foreground ,undy-comments))))

   ;; perspective
   `(persp-selected-face
     ((,undy-class (:foreground ,undy-blue
                                   :weight bold))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,undy-class (:foreground ,undy-yellow
                                   :weight normal))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight normal))))

   ;; popup
   `(popup-face
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-foreground))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-foreground))))

   `(popup-isearch-match
     ((,undy-class (:background ,undy-green))
      (,undy-256-class (:background ,undy-256-green))))

   `(popup-menu-face
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-foreground))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-foreground))))

   `(popup-menu-mouse-face
     ((,undy-class (:background ,undy-blue
                                   :foreground ,undy-foreground))
      (,undy-256-class (:background ,undy-256-blue
                                        :foreground ,undy-256-foreground))))

   `(popup-menu-selection-face
     ((,undy-class (:background ,undy-magenta
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-magenta
                                        :foreground ,undy-256-background))))

   `(popup-scroll-bar-background-face
     ((,undy-class (:background ,undy-comments))
      (,undy-256-class (:background ,undy-256-comments))))

   `(popup-scroll-bar-foreground-face
     ((,undy-class (:background ,undy-emphasis))
      (,undy-256-class (:background ,undy-256-emphasis))))

   `(popup-tip-face
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-foreground))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-foreground))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   `(rainbow-delimiters-depth-2-face
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(rainbow-delimiters-depth-3-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(rainbow-delimiters-depth-4-face
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(rainbow-delimiters-depth-5-face
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(rainbow-delimiters-depth-6-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(rainbow-delimiters-depth-7-face
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   `(rainbow-delimiters-depth-8-face
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(rainbow-delimiters-depth-9-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(rainbow-delimiters-depth-10-face
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(rainbow-delimiters-depth-11-face
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(rainbow-delimiters-depth-12-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(rainbow-delimiters-unmatched-face
     ((,undy-class (:foreground ,undy-foreground
                                   :background ,undy-background
                                   :inverse-video t))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :background ,undy-256-background
                                        :inverse-video t))))

   ;; realgud
   `(realgud-overlay-arrow1
     ((,undy-class (:foreground ,undy-green-d))
      (,undy-256-class (:foreground ,undy-256-green-d))))

   `(realgud-overlay-arrow2
     ((,undy-class (:foreground ,undy-yellow-d))
      (,undy-256-class (:foreground ,undy-256-yellow-d))))

   `(realgud-overlay-arrow3
     ((,undy-class (:foreground ,undy-orange-d))
      (,undy-256-class (:foreground ,undy-256-orange-d))))

   `(realgud-bp-enabled-face
     ((,undy-class (:inherit error)))
     ((,undy-256-class (:inherit error))))

   `(realgud-bp-disabled-face
     ((,undy-class (:inherit secondary-selection)))
     ((,undy-256-class (:inherit secondary-selection))))

   `(realgud-bp-line-enabled-face
     ((,undy-class (:foreground ,undy-red-d)))
     ((,undy-256-class (:foreground ,undy-256-red-d))))

   `(realgud-bp-line-disabled-face
     ((,undy-class (:inherit secondary-selection)))
     ((,undy-256-class (:inherit secondary-selection))))

   `(realgud-line-number
     ((,undy-class (:inerhit undy-line-number)))
     ((,undy-256-class (:inerhit undy-line-number))))

   `(realgud-backtrace-number
     ((,undy-class (:foreground ,undy-yellow-d
                                   :weight bold)))
     ((,undy-256-class (:foreground ,undy-256-yellow
                                       :weight bold))))

   ;; rhtm-mode
   `(erb-face
     ((,undy-class (:foreground ,undy-emphasis
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :background ,undy-256-background))))

   `(erb-delim-face
     ((,undy-class (:foreground ,undy-cyan
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-cyan
                                        :background ,undy-256-background))))

   `(erb-exec-face
     ((,undy-class (:foreground ,undy-emphasis
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :background ,undy-256-background))))

   `(erb-exec-delim-face
     ((,undy-class (:foreground ,undy-cyan
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-cyan
                                        :background ,undy-256-background))))

   `(erb-out-face
     ((,undy-class (:foreground ,undy-emphasis
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :background ,undy-256-background))))

   `(erb-out-delim-face
     ((,undy-class (:foreground ,undy-cyan
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-cyan
                                        :background ,undy-256-background))))

   `(erb-comment-face
     ((,undy-class (:foreground ,undy-emphasis
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :background ,undy-256-background))))

   `(erb-comment-delim-face
     ((,undy-class (:foreground ,undy-cyan
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-cyan
                                        :background ,undy-256-background))))

   ;; rst-mode
   `(rst-level-1-face
     ((,undy-class (:background ,undy-yellow
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-yellow
                                        :foreground ,undy-256-background))))

   `(rst-level-2-face
     ((,undy-class (:background ,undy-cyan
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-cyan
                                        :foreground ,undy-256-background))))

   `(rst-level-3-face
     ((,undy-class (:background ,undy-blue
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-blue
                                        :foreground ,undy-256-background))))

   `(rst-level-4-face
     ((,undy-class (:background ,undy-violet
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-violet
                                        :foreground ,undy-256-background))))

   `(rst-level-5-face
     ((,undy-class (:background ,undy-magenta
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-magenta
                                        :foreground ,undy-256-background))))

   `(rst-level-6-face
     ((,undy-class (:background ,undy-red
                                   :foreground ,undy-background))
      (,undy-256-class (:background ,undy-256-red
                                        :foreground ,undy-256-background))))

   ;; rpm-mode
   `(rpm-spec-dir-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(rpm-spec-doc-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(rpm-spec-ghost-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(rpm-spec-macro-face
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(rpm-spec-obsolete-tag-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(rpm-spec-package-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(rpm-spec-section-face
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(rpm-spec-tag-face
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(rpm-spec-var-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,undy-class (:foreground ,undy-violet
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-violet
                                        :weight bold))))

   `(sh-escaped-newline
     ((,undy-class (:foreground ,undy-yellow
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight bold))))

   `(sh-heredoc
     ((,undy-class (:foreground ,undy-yellow
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :weight bold))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,undy-class (:background ,undy-highlight-line))
      (,undy-256-class (:background ,undy-256-highlight-line))))

   `(sp-wrap-overlay-face
     ((,undy-class (:background ,undy-highlight-line))
      (,undy-256-class (:background ,undy-256-highlight-line))))

   `(sp-wrap-tag-overlay-face
     ((,undy-class (:background ,undy-highlight-line))
      (,undy-256-class (:background ,undy-256-highlight-line))))

   `(sp-show-pair-enclosing
     ((,undy-class (:inherit highlight))
      (,undy-256-class (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,undy-class (:foreground ,undy-green
                                   :background ,undy-background
                                   :weight normal
                                   :inverse-video t))
      (,undy-256-class (:foreground ,undy-256-green
                                        :background ,undy-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,undy-class (:foreground ,undy-red
                                   :background ,undy-background
                                   :weight normal
                                   :inverse-video t))
      (,undy-256-class (:foreground ,undy-256-red
                                        :background ,undy-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,undy-class (:foreground ,undy-green
                                   :background ,undy-background
                                   :weight normal
                                   :inverse-video t))
      (,undy-256-class (:foreground ,undy-256-green
                                        :background ,undy-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(show-paren-mismatch
     ((,undy-class (:foreground ,undy-red
                                   :background ,undy-background
                                   :weight normal
                                   :inverse-video t))
      (,undy-256-class (:foreground ,undy-256-red
                                        :background ,undy-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,undy-class (:foreground ,undy-green
                                   :background ,undy-background
                                   :weight normal
                                   :inverse-video t))
      (,undy-256-class (:foreground ,undy-256-green
                                        :background ,undy-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-mismatch
     ((,undy-class (:foreground ,undy-red
                                   :background ,undy-background
                                   :weight normal
                                   :inverse-video t))
      (,undy-256-class (:foreground ,undy-256-red
                                        :background ,undy-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-no-match
     ((,undy-class (:foreground ,undy-red
                                   :background ,undy-background
                                   :weight normal
                                   :inverse-video t))
      (,undy-256-class (:foreground ,undy-256-red
                                        :background ,undy-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   ;; speedbar
   `(speedbar-button-face
     ((,undy-class (:inherit ,undy-pitch
                                :foreground ,undy-comments))
      (,undy-256-class (:inherit ,undy-pitch
                                     :foreground ,undy-256-comments))))

   `(speedbar-directory-face
     ((,undy-class (:inherit ,undy-pitch
                                :foreground ,undy-blue))
      (,undy-256-class (:inherit ,undy-pitch
                                     :foreground ,undy-256-blue))))

   `(speedbar-file-face
     ((,undy-class (:inherit ,undy-pitch
                                :foreground ,undy-foreground))
      (,undy-256-class (:inherit ,undy-pitch
                                     :foreground ,undy-256-foreground))))

   `(speedbar-highlight-face
     ((,undy-class (:inherit ,undy-pitch
                                :background ,undy-highlight-line))
      (,undy-256-class (:inherit ,undy-pitch
                                     :background ,undy-256-highlight-line))))

   `(speedbar-selected-face
     ((,undy-class (:inherit ,undy-pitch
                                :foreground ,undy-yellow
                                :underline t))
      (,undy-256-class (:inherit ,undy-pitch
                                     :foreground ,undy-256-yellow
                                     :underline t))))

   `(speedbar-separator-face
     ((,undy-class (:inherit ,undy-pitch
                                :background ,undy-blue
                                :foreground ,undy-background
                                :overline ,undy-cyan-lc))
      (,undy-256-class (:inherit ,undy-pitch
                                     :background ,undy-256-blue
                                     :foreground ,undy-256-background
                                     :overline ,undy-256-cyan-lc))))

   `(speedbar-tag-face
     ((,undy-class (:inherit ,undy-pitch
                                :foreground ,undy-green))
      (,undy-256-class (:inherit ,undy-pitch
                                     :foreground ,undy-256-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,undy-class (:background ,undy-blue
                                   :foreground ,undy-background
                                   :height ,undy-height-plus-1
                                   :weight bold))
      (,undy-256-class (:background ,undy-256-blue
                                        :foreground ,undy-256-background
                                        :height ,undy-height-plus-1
                                        :weight bold))))

   `(sr-editing-path-face
     ((,undy-class (:background ,undy-yellow
                                   :foreground ,undy-background
                                   :weight bold
                                   :height ,undy-height-plus-1))
      (,undy-256-class (:background ,undy-256-yellow
                                        :foreground ,undy-256-background
                                        :weight bold
                                        :height ,undy-height-plus-1))))

   `(sr-highlight-path-face
     ((,undy-class (:background ,undy-green
                                   :foreground ,undy-background
                                   :weight bold
                                   :height ,undy-height-plus-1))
      (,undy-256-class (:background ,undy-256-green
                                        :foreground ,undy-256-background
                                        :weight bold
                                        :height ,undy-height-plus-1))))

   `(sr-passive-path-face
     ((,undy-class (:background ,undy-comments
                                   :foreground ,undy-background
                                   :weight bold
                                   :height ,undy-height-plus-1))
      (,undy-256-class (:background ,undy-256-comments
                                        :foreground ,undy-256-background
                                        :weight bold
                                        :height ,undy-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,undy-class (:inherit diundy-red-marked))
      (,undy-256-class (:inherit diundy-red-marked))))

   `(sr-marked-file-face
     ((,undy-class (:inherit diundy-red-marked))
      (,undy-256-class (:inherit diundy-red-marked))))

   `(sr-alt-marked-dir-face
     ((,undy-class (:background ,undy-magenta
                                   :foreground ,undy-background
                                   :weight bold))
      (,undy-256-class (:background ,undy-256-magenta
                                        :foreground ,undy-256-background
                                        :weight bold))))

   `(sr-alt-marked-file-face
     ((,undy-class (:background ,undy-magenta
                                   :foreground ,undy-background
                                   :weight bold))
      (,undy-256-class (:background ,undy-256-magenta
                                        :foreground ,undy-256-background
                                        :weight bold))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,undy-class (:inherit diundy-red-directory
                                :weight normal))
      (,undy-256-class (:inherit diundy-red-directory
                                     :weight normal))))

   `(sr-symlink-directory-face
     ((,undy-class (:inherit diundy-red-directory
                                :slant italic
                                :weight normal))
      (,undy-256-class (:inherit diundy-red-directory
                                     :slant italic
                                     :weight normal))))

   `(sr-symlink-face
     ((,undy-class (:inherit diundy-red-symlink
                                :slant italic
                                :weight normal))
      (,undy-256-class (:inherit diundy-red-symlink
                                     :slant italic
                                     :weight normal))))

   `(sr-broken-link-face
     ((,undy-class (:inherit diundy-red-warning
                                :slant italic
                                :weight normal))
      (,undy-256-class (:inherit diundy-red-warning
                                     :slant italic
                                     :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(sr-encrypted-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(sr-log-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(sr-packaged-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(sr-html-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(sr-xml-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,undy-class (:background ,undy-red
                                   :foreground ,undy-background
                                   :weight bold))
      (,undy-256-class (:background ,undy-256-red
                                        :foreground ,undy-256-background
                                        :weight bold))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,undy-class (:background unspecified
                                   :foreground ,undy-yellow))
      (,undy-256-class (:background unspecified
                                        :foreground ,undy-256-yellow))))

   `(syslog-hour-face
     ((,undy-class (:background unspecified
                                   :foreground ,undy-green))
      (,undy-256-class (:background unspecified
                                        :foreground ,undy-256-green))))

   `(syslog-error-face
     ((,undy-class (:background unspecified
                                   :foreground ,undy-red
                                   :weight bold))
      (,undy-256-class (:background unspecified
                                        :foreground ,undy-256-red
                                        :weight bold))))

   `(syslog-warn-face
     ((,undy-class (:background unspecified
                                   :foreground ,undy-orange
                                   :weight bold))
      (,undy-256-class (:background unspecified
                                        :foreground ,undy-256-orange
                                        :weight bold))))

   `(syslog-info-face
     ((,undy-class (:background unspecified
                                   :foreground ,undy-blue
                                   :weight bold))
      (,undy-256-class (:background unspecified
                                        :foreground ,undy-256-blue
                                        :weight bold))))

   `(syslog-debug-face
     ((,undy-class (:background unspecified
                                   :foreground ,undy-cyan
                                   :weight bold))
      (,undy-256-class (:background unspecified
                                        :foreground ,undy-256-cyan
                                        :weight bold))))

   `(syslog-su-face
     ((,undy-class (:background unspecified
                                   :foreground ,undy-magenta))
      (,undy-256-class (:background unspecified
                                        :foreground ,undy-256-magenta))))

   ;; table
   `(table-cell
     ((,undy-class (:foreground ,undy-foreground
                                   :background ,undy-highlight-line))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :background ,undy-256-highlight-line))))

   ;; term
   `(term-color-black
     ((,undy-class (:foreground ,undy-background
                                   :background ,undy-highlight-line))
      (,undy-256-class (:foreground ,undy-256-background
                                        :background ,undy-256-highlight-line))))

   `(term-color-red
     ((,undy-class (:foreground ,undy-red
                                   :background ,undy-red-d))
      (,undy-256-class (:foreground ,undy-256-red
                                        :background ,undy-256-red-d))))

   `(term-color-green
     ((,undy-class (:foreground ,undy-green
                                   :background ,undy-green-d))
      (,undy-256-class (:foreground ,undy-256-green
                                        :background ,undy-256-green-d))))

   `(term-color-yellow
     ((,undy-class (:foreground ,undy-yellow
                                   :background ,undy-yellow-d))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :background ,undy-256-yellow-d))))

   `(term-color-blue
     ((,undy-class (:foreground ,undy-blue
                                   :background ,undy-blue-d))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :background ,undy-256-blue-d))))

   `(term-color-magenta
     ((,undy-class (:foreground ,undy-magenta
                                   :background ,undy-magenta-d))
      (,undy-256-class (:foreground ,undy-256-magenta
                                        :background ,undy-256-magenta-d))))

   `(term-color-cyan
     ((,undy-class (:foreground ,undy-cyan
                                   :background ,undy-cyan-d))
      (,undy-256-class (:foreground ,undy-256-cyan
                                        :background ,undy-256-cyan-d))))

   `(term-color-white
     ((,undy-class (:foreground ,undy-emphasis
                                   :background ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :background ,undy-256-foreground))))

   `(term-default-fg-color
     ((,undy-class (:inherit term-color-white))
      (,undy-256-class (:inherit term-color-white))))

   `(term-default-bg-color
     ((,undy-class (:inherit term-color-black))
      (,undy-256-class (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,undy-class (:background ,undy-yellow-hc
                                   :foreground ,undy-background
                                   :inherit ,undy-pitch))))

   ;; treemacs
   `(treemacs-directory-face
      ((,undy-class (:foreground ,undy-violet
                         :background ,undy-background
                         :weight bold))
        (,undy-256-class (:foreground ,undy-256-violet
                              :background ,undy-256-background
                              :weight bold))))

   `(treemacs-header-face
      ((,undy-class (:foreground ,undy-yellow
                         :background ,undy-background
                         :underline t
                         :weight bold))
        (,undy-256-class (:foreground ,undy-256-yellow
                              :background ,undy-256-background
                              :underline t
                              :weight bold))))

   `(treemacs-git-modified-face
      ((,undy-class (:foreground ,undy-green
                         :background ,undy-background))
        (,undy-256-class (:foreground ,undy-256-green
                              :background ,undy-256-background))))

   `(treemacs-git-renamed-face
      ((,undy-class (:foreground ,undy-red
                         :background ,undy-background))
        (,undy-256-class (:foreground ,undy-256-red
                              :background ,undy-256-background))))

   `(treemacs-git-ignored-face
      ((,undy-class (:foreground ,undy-gray-l
                         :background ,undy-background))
        (,undy-256-class (:foreground ,undy-256-gray-l
                              :background ,undy-256-background))))

   `(treemacs-git-untracked-face
      ((,undy-class (:foreground ,undy-red
                         :background ,undy-background))
        (,undy-256-class (:foreground ,undy-256-red
                              :background ,undy-256-background))))

   `(treemacs-git-added-face
      ((,undy-class (:foreground ,undy-green
                         :background ,undy-background))
        (,undy-256-class (:foreground ,undy-256-green
                              :background ,undy-256-background))))

   `(treemacs-git-conflict-face
      ((,undy-class (:foreground ,undy-orange
                         :background ,undy-background))
        (,undy-256-class (:foreground ,undy-256-orange
                              :background ,undy-256-background))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,undy-class (:foreground ,undy-magenta
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-magenta
                                        :weight bold))))

   `(tuareg-font-lock-multistage-face
     ((,undy-class (:foreground ,undy-blue
                                   :background ,undy-highlight-line
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :background ,undy-256-highlight-line
                                        :weight bold))))

   `(tuareg-font-lock-operator-face
     ((,undy-class (:foreground ,undy-emphasis))
      (,undy-256-class (:foreground ,undy-256-emphasis))))

   `(tuareg-font-lock-error-face
     ((,undy-class (:foreground ,undy-yellow
                                   :background ,undy-red
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :background ,undy-256-red
                                        :weight bold))))

   `(tuareg-font-lock-interactive-output-face
     ((,undy-class (:foreground ,undy-cyan))
      (,undy-256-class (:foreground ,undy-256-cyan))))

   `(tuareg-font-lock-interactive-error-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,undy-class (:foreground ,undy-comments
                                   :background ,undy-background))
      (,undy-256-class (:foreground ,undy-256-comments
                                        :background ,undy-256-background))))

   `(undo-tree-visualizer-unmodified-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(undo-tree-visualizer-current-face
     ((,undy-class (:foreground ,undy-blue
                                   :inverse-video t))
      (,undy-256-class (:foreground ,undy-256-blue
                                        :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,undy-class (:foreground ,undy-emphasis
                                   :background ,undy-background
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :background ,undy-256-background
                                        :weight bold))))

   `(undo-tree-visualizer-register-face
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   ;; volatile highlights
   `(vhl/default-face
      ((,undy-class (:background ,undy-highlight-alt))
        (,undy-256-class (:background ,undy-256-highlight-alt))))

   ;; w3m
   `(w3m-anchor
     ((,undy-class (:inherit link))
      (,undy-256-class (:inherit link))))

   `(w3m-arrived-anchor
     ((,undy-class (:inherit link-visited))
      (,undy-256-class (:inherit link-visited))))

   `(w3m-form
     ((,undy-class (:background ,undy-background
                                   :foreground ,undy-foreground))
      (,undy-256-class (:background ,undy-256-background
                                        :foreground ,undy-256-foreground))))

   `(w3m-header-line-location-title
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-yellow))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-yellow))))

   `(w3m-header-line-location-content

     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-foreground))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-foreground))))

   `(w3m-bold
     ((,undy-class (:foreground ,undy-emphasis
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :weight bold))))

   `(w3m-image-anchor
     ((,undy-class (:background ,undy-background
                                   :foreground ,undy-cyan
                                   :inherit link))
      (,undy-256-class (:background ,undy-256-background
                                        :foreground ,undy-256-cyan
                                        :inherit link))))

   `(w3m-image
     ((,undy-class (:background ,undy-background
                                   :foreground ,undy-cyan))
      (,undy-256-class (:background ,undy-256-background
                                        :foreground ,undy-256-cyan))))

   `(w3m-lnum-minibuffer-prompt
     ((,undy-class (:foreground ,undy-emphasis))
      (,undy-256-class (:foreground ,undy-256-emphasis))))

   `(w3m-lnum-match
     ((,undy-class (:background ,undy-highlight-line))
      (,undy-256-class (:background ,undy-256-highlight-line))))

   `(w3m-lnum
     ((,undy-class (:underline nil
                                  :bold nil
                                  :foreground ,undy-red))
      (,undy-256-class (:underline nil
                                       :bold nil
                                       :foreground ,undy-256-red))))

   `(w3m-session-select
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(w3m-session-selected
     ((,undy-class (:foreground ,undy-emphasis
                                   :bold t
                                   :underline t))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :bold t
                                        :underline t))))

   `(w3m-tab-background
     ((,undy-class (:background ,undy-background
                                   :foreground ,undy-foreground))
      (,undy-256-class (:background ,undy-256-background
                                        :foreground ,undy-256-foreground))))

   `(w3m-tab-selected-background
     ((,undy-class (:background ,undy-background
                                   :foreground ,undy-foreground))
      (,undy-256-class (:background ,undy-256-background
                                        :foreground ,undy-256-foreground))))

   `(w3m-tab-mouse
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-yellow))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-yellow))))

   `(w3m-tab-selected
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-emphasis
                                   :bold t))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-emphasis
                                        :bold t))))

   `(w3m-tab-unselected
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-foreground))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-foreground))))

   `(w3m-tab-selected-retrieving
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-red))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-red))))

   `(w3m-tab-unselected-retrieving
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-orange))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-orange))))

   `(w3m-tab-unselected-unseen
     ((,undy-class (:background ,undy-highlight-line
                                   :foreground ,undy-violet))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :foreground ,undy-256-violet))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(web-mode-comment-face
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(web-mode-constant-face
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   `(web-mode-current-element-highlight-face
     ((,undy-class (:underline unspecified
                                  :weight unspecified
                                  :background ,undy-highlight-line))
      (,undy-256-class (:underline unspecified
                                       :weight unspecified
                                       :background ,undy-256-highlight-line))))

   `(web-mode-doctype-face
     ((,undy-class (:foreground ,undy-comments
                                   :slant italic
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-comments
                                        :slant italic
                                        :weight bold))))

   `(web-mode-folded-face
     ((,undy-class (:underline t))
      (,undy-256-class (:underline t))))

   `(web-mode-function-name-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(web-mode-html-attr-name-face
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(web-mode-html-attr-custom-face
     ((,undy-class (:inherit web-mode-html-attr-name-face))
      (,undy-256-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-engine-face
     ((,undy-class (:inherit web-mode-block-delimiter-face))
      (,undy-256-class (:inherit web-mode-block-delimiter-face))))

   `(web-mode-html-attr-equal-face
     ((,undy-class (:inherit web-mode-html-attr-name-face))
      (,undy-256-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-value-face
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(web-mode-html-tag-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(web-mode-keyword-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(web-mode-preprocessor-face
     ((,undy-class (:foreground ,undy-yellow
                                   :slant normal
                                   :weight unspecified))
      (,undy-256-class (:foreground ,undy-256-yellow
                                        :slant normal
                                        :weight unspecified))))

   `(web-mode-string-face
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(web-mode-type-face
     ((,undy-class (:inherit font-lock-type-face))
      (,undy-256-class (:inherit font-lock-type-face))))

   `(web-mode-variable-name-face
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(web-mode-warning-face
     ((,undy-class (:inherit font-lock-warning-face))
      (,undy-256-class (:inherit font-lock-warning-face))))

   `(web-mode-block-face
     ((,undy-class (:background unspecified))
      (,undy-256-class (:background unspecified))))

   `(web-mode-block-delimiter-face
     ((,undy-class (:inherit font-lock-preprocessor-face))
      (,undy-256-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-comment-face
     ((,undy-class (:inherit web-mode-comment-face))
      (,undy-256-class (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,undy-class (:inherit font-lock-preprocessor-face))
      (,undy-256-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-string-face
     ((,undy-class (:inherit web-mode-string-face))
      (,undy-256-class (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,undy-class (:box 1 :weight bold))
      (,undy-256-class (:box 1 :weight bold))))

   `(web-mode-css-at-rule-face
     ((,undy-class (:inherit font-lock-constant-face))
      (,undy-256-class (:inherit font-lock-constant-face))))

   `(web-mode-css-pseudo-class-face
     ((,undy-class (:inherit font-lock-builtin-face))
      (,undy-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-color-face
     ((,undy-class (:inherit font-lock-builtin-face))
      (,undy-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-filter-face
     ((,undy-class (:inherit font-lock-function-name-face))
      (,undy-256-class (:inherit font-lock-function-name-face))))

   `(web-mode-css-function-face
     ((,undy-class (:inherit font-lock-builtin-face))
      (,undy-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-call-face
     ((,undy-class (:inherit font-lock-function-name-face))
      (,undy-256-class (:inherit font-lock-function-name-face))))

   `(web-mode-css-priority-face
     ((,undy-class (:inherit font-lock-builtin-face))
      (,undy-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,undy-class (:inherit font-lock-variable-name-face))
      (,undy-256-class (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,undy-class (:inherit font-lock-keyword-face))
      (,undy-256-class (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,undy-class (:inherit web-mode-string-face))
      (,undy-256-class (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,undy-class (:inherit web-mode-string-face))
      (,undy-256-class (:inherit web-mode-string-face))))

   `(web-mode-json-comment-face
     ((,undy-class (:inherit web-mode-comment-face))
      (,undy-256-class (:inherit web-mode-comment-face))))

   `(web-mode-json-context-face
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   `(web-mode-json-key-face
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   `(web-mode-json-string-face
     ((,undy-class (:inherit web-mode-string-face))
      (,undy-256-class (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(web-mode-part-comment-face
     ((,undy-class (:inherit web-mode-comment-face))
      (,undy-256-class (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,undy-class (:inherit web-mode-block-face))
      (,undy-256-class (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,undy-class (:inherit web-mode-string-face))
      (,undy-256-class (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,undy-class (:foreground ,undy-violet))
      (,undy-256-class (:foreground ,undy-256-violet))))

   `(web-mode-whitespace-face
     ((,undy-class (:background ,undy-red))
      (,undy-256-class (:background ,undy-256-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,undy-class (:background unspecified
                                   :foreground ,undy-comments
                                   :inverse-video unspecified
                                   :slant italic))
      (,undy-256-class (:background unspecified
                                        :foreground ,undy-256-comments
                                        :inverse-video unspecified
                                        :slant italic))))

   `(whitespace-hspace
     ((,undy-class (:background unspecified
                                   :foreground ,undy-emphasis
                                   :inverse-video unspecified))
      (,undy-256-class (:background unspecified
                                        :foreground ,undy-256-emphasis
                                        :inverse-video unspecified))))

   `(whitespace-tab
     ((,undy-class (:background unspecified
                                   :foreground ,undy-red
                                   :inverse-video unspecified
                                   :weight bold))
      (,undy-256-class (:background unspecified
                                        :foreground ,undy-256-red
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-newline
     ((,undy-class(:background unspecified
                                  :foreground ,undy-comments
                                  :inverse-video unspecified))
      (,undy-256-class (:background unspecified
                                       :foreground ,undy-256-comments
                                       :inverse-video unspecified))))

   `(whitespace-trailing
     ((,undy-class (:background unspecified
                                   :foreground ,undy-orange-lc
                                   :inverse-video t))
      (,undy-256-class (:background unspecified
                                        :foreground ,undy-256-orange-lc
                                        :inverse-video t))))

   `(whitespace-line
     ((,undy-class (:background unspecified
                                   :foreground ,undy-magenta
                                   :inverse-video unspecified))
      (,undy-256-class (:background unspecified
                                        :foreground ,undy-256-magenta
                                        :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,undy-class (:background ,undy-red-lc
                                   :foreground unspecified
                                   :inverse-video unspecified))
      (,undy-256-class (:background ,undy-256-red-lc
                                        :foreground unspecified
                                        :inverse-video unspecified))))

   `(whitespace-indentation
     ((,undy-class (:background unspecified
                                   :foreground ,undy-yellow
                                   :inverse-video unspecified
                                   :weight bold))
      (,undy-256-class (:background unspecified
                                        :foreground ,undy-256-yellow
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-empty
     ((,undy-class (:background unspecified
                                   :foreground ,undy-red-lc
                                   :inverse-video t))
      (,undy-256-class (:background unspecified
                                        :foreground ,undy-256-red-lc
                                        :inverse-video t))))

   `(whitespace-space-after-tab
     ((,undy-class (:background unspecified
                                   :foreground ,undy-orange
                                   :inverse-video t
                                   :weight bold))
      (,undy-256-class (:background unspecified
                                        :foreground ,undy-256-orange
                                        :inverse-video t
                                        :weight bold))))

   ;; wanderlust
   `(wl-highlight-folder-few-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(wl-highlight-folder-many-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(wl-highlight-folder-path-face
     ((,undy-class (:foreground ,undy-orange))
      (,undy-256-class (:foreground ,undy-256-orange))))

   `(wl-highlight-folder-unread-face
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(wl-highlight-folder-zero-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(wl-highlight-folder-unknown-face
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(wl-highlight-message-citation-header
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(wl-highlight-message-cited-text-1
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(wl-highlight-message-cited-text-2
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(wl-highlight-message-cited-text-3
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(wl-highlight-message-cited-text-4
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(wl-highlight-message-header-contents-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(wl-highlight-message-headers-face
     ((,undy-class (:foreground ,undy-red))
      (,undy-256-class (:foreground ,undy-256-red))))

   `(wl-highlight-message-important-header-contents
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(wl-highlight-message-header-contents
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(wl-highlight-message-important-header-contents2
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(wl-highlight-message-signature
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   `(wl-highlight-message-unimportant-header-contents
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(wl-highlight-summary-answeundy-red-face
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(wl-highlight-summary-disposed-face
     ((,undy-class (:foreground ,undy-foreground
                                   :slant italic))
      (,undy-256-class (:foreground ,undy-256-foreground
                                        :slant italic))))

   `(wl-highlight-summary-new-face
     ((,undy-class (:foreground ,undy-blue))
      (,undy-256-class (:foreground ,undy-256-blue))))

   `(wl-highlight-summary-normal-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(wl-highlight-summary-thread-top-face
     ((,undy-class (:foreground ,undy-yellow))
      (,undy-256-class (:foreground ,undy-256-yellow))))

   `(wl-highlight-thread-indent-face
     ((,undy-class (:foreground ,undy-magenta))
      (,undy-256-class (:foreground ,undy-256-magenta))))

   `(wl-highlight-summary-refiled-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(wl-highlight-summary-displaying-face
     ((,undy-class (:underline t
                                  :weight bold))
      (,undy-256-class (:underline t
                                       :weight bold))))

   ;; weechat
   `(weechat-error-face
     ((,undy-class (:inherit error))
      (,undy-256-class (:inherit error))))

   `(weechat-highlight-face
     ((,undy-class (:foreground ,undy-emphasis
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-emphasis
                                        :weight bold))))

   `(weechat-nick-self-face
     ((,undy-class (:foreground ,undy-green
                                   :weight unspecified
                                   :inverse-video t))
      (,undy-256-class (:foreground ,undy-256-green
                                        :weight unspecified
                                        :inverse-video t))))

   `(weechat-prompt-face
     ((,undy-class (:inherit minibuffer-prompt))
      (,undy-256-class (:inherit minibuffer-prompt))))

   `(weechat-time-face
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   ;; which-func-mode
   `(which-func
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   ;; which-key
   `(which-key-key-face
     ((,undy-class (:foreground ,undy-green
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-green
                                        :weight bold))))

   `(which-key-separator-face
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(which-key-note-face
     ((,undy-class (:foreground ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments))))

   `(which-key-command-description-face
     ((,undy-class (:foreground ,undy-foreground))
      (,undy-256-class (:foreground ,undy-256-foreground))))

   `(which-key-local-map-description-face
     ((,undy-class (:foreground ,undy-yellow-hc))
      (,undy-256-class (:foreground ,undy-256-yellow-hc))))

   `(which-key-group-description-face
     ((,undy-class (:foreground ,undy-red
                                   :weight bold))
      (,undy-256-class (:foreground ,undy-256-red
                                        :weight bold))))
   ;; window-number-mode
   `(window-number-face
     ((,undy-class (:foreground ,undy-green))
      (,undy-256-class (:foreground ,undy-256-green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,undy-class (:foreground ,undy-comments
                                   :background ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments
                                        :background ,undy-256-comments))))

   `(yascroll:thumb-fringe
     ((,undy-class (:foreground ,undy-comments
                                   :background ,undy-comments))
      (,undy-256-class (:foreground ,undy-256-comments
                                        :background ,undy-256-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,undy-class (:background ,undy-highlight-line
                                   :box ,undy-emphasis))
      (,undy-256-class (:background ,undy-256-highlight-line
                                        :box ,undy-256-emphasis)))))

  (custom-theme-set-variables
   'undy
   `(ansi-color-names-vector [,undy-background ,undy-red ,undy-green ,undy-yellow
                                                  ,undy-blue ,undy-magenta ,undy-cyan ,undy-foreground])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,undy-highlight-line)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors '(,undy-magenta ,undy-violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,undy-highlight-line . 0)
       (,undy-green-lc . 20)
       (,undy-cyan-lc . 30)
       (,undy-blue-lc . 50)
       (,undy-yellow-lc . 60)
       (,undy-orange-lc . 70)
       (,undy-magenta-lc . 85)
       (,undy-highlight-line . 100)))

   ;; pos-tip
   `(pos-tip-foreground-color ,undy-background)
   `(pos-tip-background-color ,undy-yellow-hc)

   ;; vc
   `(vc-annotate-color-map
     '((20 . ,undy-red)
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . ,undy-yellow)
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . ,undy-green)
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . ,undy-cyan)
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . ,undy-blue)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; weechat
   `(weechat-color-list
     '(unspecified ,undy-background ,undy-highlight-line
                  ,undy-red-d ,undy-red
                  ,undy-green-d ,undy-green
                  ,undy-yellow-d ,undy-yellow
                  ,undy-blue-d ,undy-blue
                  ,undy-magenta-d ,undy-magenta
                  ,undy-cyan-d ,undy-cyan
                  ,undy-foreground ,undy-emphasis))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'undy)

;; Local Variables:
;; no-byte-compile: t
;; fill-column: 95
;; End:

;;; undy-theme.el ends here
