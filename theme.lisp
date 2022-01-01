(defpackage #:40ants-doc-theme-40ants
  (:use #:cl)
  (:import-from #:lass)
  (:import-from #:40ants-doc/themes/api)
  (:import-from #:40ants-doc/themes/default
                #:default-theme)
  (:import-from #:40ants-doc/commondoc/html
                #:with-html)
  (:import-from #:alexandria
                #:when-let*)
  (:import-from #:40ants-doc/builder)
  (:import-from #:40ants-doc/github)
  (:nicknames #:40ants-doc-theme-40ants/theme)
  (:export #:40ants-theme))
(in-package 40ants-doc-theme-40ants)


(defclass 40ants-theme (default-theme)
  ())


(defmethod 40ants-doc/themes/api:render-css ((theme 40ants-theme))
  (let ((toc-back "#FFFEFB")
        (orange "#E78B24"))
    (concatenate
     'string
     (call-next-method)

     (lass:compile-and-write
      `(body
        :font-family "\"Lora\",Georgia,\"Times New Roman\",Times,serif"
        :font-size 17px
        :line-height 26px
        :color "#333"

        ((:or |#sidebar-check|
              |#sidebar-trigger|)
         :display none)

        ((:or h1 h2 h3 h4 h5 h6 h7)
         :color ,orange
         :border-bottom none)

        (.sidebar
         :background ,toc-back
         :box-shadow inset -3px 0 3px 0px "#777"
         :transition all .4s

         (.page-toc
          (a
           :color "#333"))
         (.toc-active
          :background "rgba(0,0,0,0)"
          :box-shadow 0 0 0
          :border-left 10px solid ,orange
          :margin-left 0))

        (.navbar
         :margin-top 2em
         :margin-bottom 26px
         :margin-left 44ex
         :max-width 70%
         :overflow visible
         (a
          :border-bottom none)
         (.navbar-inner
          :min-height 60px
          :padding-right 20px
          :padding-left 20px
          :display flex
          :flex-wrap nowrap
          :border 1px solid "#d7d7d7"
          :border-radius 4px
          :box-shadow 0 1px 4px "rgb(0 0 0 / 7%)")
         (.nav
          :left 0
          :display flex
          :margin 0 10px 0 0
          :list-style none
          (li
           :text-align center
           (a
            :display inline-block
            :padding 17px 15px 17px
            :color "#333"
            :text-decoration none
            :text-shadow 0 1px 0 "#f6f6f6")))
         (.brand
          :padding 0
          :margin -5px 0 0 -15px
          :display block)
         ((.brand > .logo)
          :width 60px))

        ((.page > .footer)
         :margin-left 44ex
         :margin-right 44ex
         (.fineprint
          :font-size smaller)
         (.separator
          :margin 26px 0
          :height 1px
          :linear-gradient left "rgba(0,0,0,0)" 0% "rgba(0,0,0,0.75)" 50% "rgba(0,0,0,0)" 100%)
         (.lisp-logo
          :float right
          :margin-right 1em
          :margin-top -0.5em
          :border none)
         ((.lisp-logo img)
          :width 60px))

        ;; I use this class on tables with GitHub badges
        (.badges
         (a
          :border-bottom none))

        ;; This code renders a GitHub stripe if ASDF system's github url is known
        (|#fork-me|
         :top 3em
         :right -6em
         :color "#fff"
         :display block
         :position fixed
         :text-align center
         :text-decoration none
         :letter-spacing .06em
         :background-color "#A00"
         :padding 0.5em 5em 0.4em 5em
         :text-shadow 0 0 0.75em "#444"
         :box-shadow "0 0 0.5em rgba(0,0,0,0.5)"
         :transform "rotate(45deg) scale(0.75,1)"
         :font bold "16px/1.2em" "Arial, Sans-Serif"
         :z-index 10)

        ((:and |#fork-me| :before)
         :content ""
         :top 0
         :left 0
         :right 0
         :bottom 0
         :position absolute
         :margin -0.3em -5em
         :transform "scale(0.7)"
         :border "2px rgba(255,255,255,0.7) dashed")

        ((:and |#fork-me| :hover)
         :opacity 0.9)

        (.demo :border 0.1em solid "#CCCCCC"
               :background-color "#F5F3ED"
               :margin-top 1em
               :padding 3px
               :padding-right 5px

               (iframe :background-color white
                       :border 1px solid "#CCC"))))

     (lass:compile-and-write
      '(:media "(max-width: 800px)"
        (body
         (.search
          (input
           :font-size 3ex))
         ((.page > .content) :margin 0 2ex 0 2ex
          :padding 1ex)
         ((.page > .footer) :margin 0 2ex 0 2ex
          :padding 1ex
          (.fineprint
           :text-align center))
         
         ;; TODO: Make bar visible and readable on phones
         (.navbar :display none)

         ;; This sidebar trigger was inspired by these articles
         ;; https://www.cssscript.com/hamburger-sidebar-navigation/
         ;; https://gscode.in/css-hamburger-menu/
         (|#sidebar-trigger|
          :display inline-block
          :position relative
          :top 2ex
          :left 3ex
          :z-index 1000
          :width 50px
          :height 44px
          :cursor pointer
          :box-sizing border-box
          
          (span
           :display inline-block
           :transition all .4s
           :box-sizing border-box
           :position absolute
           :left 0
           :width 100%
           :height 4px
           :background-color orange
           :border-radius 4px)

          ((:and span (:nth-of-type 1))
           :top 0)
          ((:and span (:nth-of-type 2))
           :top 20px)
          ((:and span (:nth-of-type 3))
           :bottom 0))

         (((:and |#sidebar-check| :checked)
           ~
           .sidebar)
          :display block
          :width 100%
          :max-width 100%
          :position relative)

         (((:and |#sidebar-check| :checked)
           ~
           (:or .content
            .footer))
          :display none)
         
         (((:and |#sidebar-check| :checked)
           ~
           (label (:and span (:nth-of-type 1))))
          :top 20px
          :transform rotate -45deg)
         (((:and |#sidebar-check| :checked)
           ~
           (label (:and span (:nth-of-type 2))))
          :opacity 0)
         (((:and |#sidebar-check| :checked)
           ~
           (label (:and span (:nth-of-type 3))))
          :top 20px
          :transform rotate 45deg)

         (.sidebar
          :display none
          :box-shadow none
          ;; To remove scroll in the sidebar itself,
          ;; when we show it full screen on small devices
          :height inherit
          (.page-toc
           (p :font-size 3ex)))))))))
               

(defmethod 40ants-doc/themes/api:highlight-theme ((theme 40ants-theme))
  "a11y-dark")


(defparameter *menu-items*
  '(("/about.html" "Who We Are")
    ("https://gitter.im/40ants/team" "Chat With Us")
    ("/tips.html" "Lisp Tips")
    ("/projects.html" "Our Projects")))


(defmethod 40ants-doc/themes/api:render-page-header ((theme 40ants-theme) uri title)
  (with-html
    ;; Code of the GitHub stripe was taken from:
    ;; https://codepen.io/beben-koben/pen/BoLyf
    (when-let* ((asdf-system (40ants-doc/builder:get-current-asdf-system))
                (github-uri (40ants-doc/github::asdf-system-github-uri asdf-system)))
      (:a :id "fork-me"
          :href github-uri
          "Fork me on GitHub"))

    (:div :class "navbar"
          (:div :class "navbar-inner"
                (:a :class "brand"
                    :href "/"
                    (:img :class "logo"
                          :src "https://40ants.com/img/logo.svg"))
                (:ul :class "nav"
                     (loop for (url item) in *menu-items*
                           do (:li (:a :href url
                                       item))))))

    (:input :type "checkbox"
            :id "sidebar-check")
    (:label :for "sidebar-check"
            (:div :id "sidebar-trigger"
                  (:span)
                  (:span)
                  (:span)))))


(defmethod 40ants-doc/themes/api:render-page-footer ((theme 40ants-theme) uri)
  (with-html
    (:div :class "footer"
          (:hr :class "separator")
          (:p :class "fineprint"
              "Created with passion by " (:em "40Ants")
              (:a :class "lisp-logo"
                  :href "http://lisp-lang.org/"
                  (:img :src "https://40ants.com/img/made-with-lisp.svg"))))))
