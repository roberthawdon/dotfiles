(beacon-mode 1)

(map! :leader
      (:prefix ("c h" . "Help info from Clippy")
       :desc "Clippy describes function under point" "f" #'clippy-describe-function
       :desc "Clippy describes variable under point" "v" #'clippy-describe-variable))

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file"           "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(setq doom-theme 'doom-one)
(map! :leader
      :desc "Load new theme" "h t" #'counsel-load-theme)

(use-package emojify
  :hook (after-init . global-emojify-mode))
(map! :leader
      :desc "Toggle emojify" "t e" #'global-emojify-mode)

(when window-system
  (if  (eq system-type 'darwin)
    (setq doom-font "VictorMono Nerd Font")
    (setq doom-variable-pitch-font "Annai MN")
    (setq doom-big-font "VictorMono Nerd Font"))
  (if   (eq system-type 'gnu/linux)
    (setq doom-font "VictorMono Nerd Font"))
    (setq doom-variable-pitch-font "Annai MN")
    (setq doom-big-font "VictorMono Nerd Font"))

  (if (> (display-pixel-height) 1080)
    (setq doom-font-size 24)
    (setq doom-variable-pitch-font-size 30)
    (setq doom-big-font-size 48))
  (if (<= (display-pixel-height) 1080)
    (setq doom-font-size 12)
    (setq doom-variable-pitch-font-size 15)
    (setq doom-big-font-size 24))
;; (setq doom-font (font-spec :family "VictorMono Nerd Font" :size 12)
;;       doom-variable-pitch-font (font-spec :family "Annai MN" :size 15)
;;       doom-big-font (font-spec :family "VictorMono Nerd Font" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(map! :leader
      :desc "Magit pull" "g p" #'magit-pull)

(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))
;;(after! doom-themes
;;  (setq doom-neotree-enable-variable-pitch t))
(map! :leader
      :desc "Toggle neotree file viewer" "t n" #'neotree-toggle
      :desc "Open directory in neotree"  "d n" #'neotree-dir)

(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.2)))))

(setq minimap-window-location 'right)
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle minimap-mode" "m" #'minimap-mode))

(set-face-attribute 'mode-line nil :font "VictorMono Nerd Font")
(setq doom-modeline-height 12     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name

(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)
(after! org
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-hide-emphasis-markers t
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
          '(("google" . "http://www.google.com/search?q=")
            ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
            ("ddg" . "https://duckduckgo.com/?q=")
            ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-table-convert-region-max-lines 20000
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO(t)"           ; A task that is ready to be tackled
             "BLOG(b)"           ; Blog writing assignments
             "GYM(g)"            ; Things to accomplish at the gym
             "PROJ(p)"           ; A project that contains other tasks
             "VIDEO(v)"          ; Video assignments
             "WAIT(w)"           ; Something is holding up this task
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"           ; Task has been completed
             "CANCELLED(c)" )))) ; Task has been cancelled

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(defun dt/insert-auto-tangle-tag ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (evil-org-open-below 1)
  (insert "#+auto_tangle: t ")
  (evil-force-normal-state))

(map! :leader
      :desc "Insert auto_tangle tag" "i a" #'dt/insert-auto-tangle-tag)

(after! org
    (defun dt/org-colors-doom-one ()
    "Enable Doom One colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#51afef" ultra-bold)
            (org-level-2 1.6 "#c678dd" extra-bold)
            (org-level-3 1.5 "#98be65" bold)
            (org-level-4 1.4 "#da8548" semi-bold)
            (org-level-5 1.3 "#5699af" normal)
            (org-level-6 1.2 "#a9a1e1" normal)
            (org-level-7 1.1 "#46d9ff" normal)
            (org-level-8 1.0 "#ff6c6b" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun dt/org-colors-dracula ()
    "Enable Dracula colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#8be9fd" ultra-bold)
            (org-level-2 1.6 "#bd93f9" extra-bold)
            (org-level-3 1.5 "#50fa7b" bold)
            (org-level-4 1.4 "#ff79c6" semi-bold)
            (org-level-5 1.3 "#9aedfe" normal)
            (org-level-6 1.2 "#caa9fa" normal)
            (org-level-7 1.1 "#5af78e" normal)
            (org-level-8 1.0 "#ff92d0" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun dt/org-colors-gruvbox-dark ()
    "Enable Gruvbox Dark colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#458588" ultra-bold)
            (org-level-2 1.6 "#b16286" extra-bold)
            (org-level-3 1.5 "#98971a" bold)
            (org-level-4 1.4 "#fb4934" semi-bold)
            (org-level-5 1.3 "#83a598" normal)
            (org-level-6 1.2 "#d3869b" normal)
            (org-level-7 1.1 "#d79921" normal)
            (org-level-8 1.0 "#8ec07c" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun dt/org-colors-monokai-pro ()
    "Enable Monokai Pro colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#78dce8" ultra-bold)
            (org-level-2 1.6 "#ab9df2" extra-bold)
            (org-level-3 1.5 "#a9dc76" bold)
            (org-level-4 1.4 "#fc9867" semi-bold)
            (org-level-5 1.3 "#ff6188" normal)
            (org-level-6 1.2 "#ffd866" normal)
            (org-level-7 1.1 "#78dce8" normal)
            (org-level-8 1.0 "#ab9df2" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun dt/org-colors-nord ()
    "Enable Nord colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#81a1c1" ultra-bold)
            (org-level-2 1.6 "#b48ead" extra-bold)
            (org-level-3 1.5 "#a3be8c" bold)
            (org-level-4 1.4 "#ebcb8b" semi-bold)
            (org-level-5 1.3 "#bf616a" normal)
            (org-level-6 1.2 "#88c0d0" normal)
            (org-level-7 1.1 "#81a1c1" normal)
            (org-level-8 1.0 "#b48ead" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun dt/org-colors-oceanic-next ()
    "Enable Oceanic Next colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#6699cc" ultra-bold)
            (org-level-2 1.6 "#c594c5" extra-bold)
            (org-level-3 1.5 "#99c794" bold)
            (org-level-4 1.4 "#fac863" semi-bold)
            (org-level-5 1.3 "#5fb3b3" normal)
            (org-level-6 1.2 "#ec5f67" normal)
            (org-level-7 1.1 "#6699cc" normal)
            (org-level-8 1.0 "#c594c5" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun dt/org-colors-palenight ()
    "Enable Palenight colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#82aaff" ultra-bold)
            (org-level-2 1.6 "#c792ea" extra-bold)
            (org-level-3 1.5 "#c3e88d" bold)
            (org-level-4 1.4 "#ffcb6b" semi-bold)
            (org-level-5 1.3 "#a3f7ff" normal)
            (org-level-6 1.2 "#e1acff" normal)
            (org-level-7 1.1 "#f07178" normal)
            (org-level-8 1.0 "#ddffa7" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun dt/org-colors-solarized-dark ()
    "Enable Solarized Dark colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#268bd2" ultra-bold)
            (org-level-2 1.6 "#d33682" extra-bold)
            (org-level-3 1.5 "#859900" bold)
            (org-level-4 1.4 "#b58900" semi-bold)
            (org-level-5 1.3 "#cb4b16" normal)
            (org-level-6 1.2 "#6c71c4" normal)
            (org-level-7 1.1 "#2aa198" normal)
            (org-level-8 1.0 "#657b83" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun dt/org-colors-solarized-light ()
    "Enable Solarized Light colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#268bd2" ultra-bold)
            (org-level-2 1.6 "#d33682" extra-bold)
            (org-level-3 1.5 "#859900" bold)
            (org-level-4 1.4 "#b58900" semi-bold)
            (org-level-5 1.3 "#cb4b16" normal)
            (org-level-6 1.2 "#6c71c4" normal)
            (org-level-7 1.1 "#2aa198" normal)
            (org-level-8 1.0 "#657b83" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun dt/org-colors-tomorrow-night ()
    "Enable Tomorrow Night colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#81a2be" ultra-bold)
            (org-level-2 1.6 "#b294bb" extra-bold)
            (org-level-3 1.5 "#b5bd68" bold)
            (org-level-4 1.4 "#e6c547" semi-bold)
            (org-level-5 1.3 "#cc6666" normal)
            (org-level-6 1.2 "#70c0ba" normal)
            (org-level-7 1.1 "#b77ee0" normal)
            (org-level-8 1.0 "#9ec400" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

  ;; Load our desired dt/org-colors-* theme on startup
  (dt/org-colors-doom-one)
)
